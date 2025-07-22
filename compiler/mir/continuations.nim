## Implements the lowering of `fork` and `land`. For every static `land`, the
## reachable code (i.e., the continuation) is reified into a standalone
## procedure -- the context (i.e., all active locals) are saved into a separate
## context object, which must be passed to the reified procedure.
##
## A rough summary of the reification process is that the procedure body is
## duplicated and all basic-blocks not reachable from the resumption point
## are removed.
##
## Forks in loops make this a bit trickier in practice. Consider:
##
##   loop:
##     scope:
##       def _1 = ...
##       if cond:
##         goto L2
##       fork L1
##       ...
##       land L1:
##       ...
##   L2:
##   ...
##
## Here, the loop must also be part of the reified procedure, but a naive
## removal of all unused basic blocks would leave the `loop` at the start,
## which is wrong.
##
## To keep the implementation simple, the loop is simply unrolled once,
## with the unrolled part then being split, resulting in (re-using the example
## from above):
##
##   scope:
##     def _1 = context._1
##     ... # the code after `land L1`
##   loop:
##     def _1 = ...
##     if cond:
##       goto L2:
##     ... # the code after the fork
##     goto Lexit
##
## A reified continuation is created for each fork/land pair.

# TODO: do not touch `PSym`, `PType`, and `PNode`. The pass(es) must only use
#       MIR-level facilities

import
  std/[
    algorithm,
    packedsets,
    tables
  ],
  compiler/ast/[
    ast_types,
    ast_query,
    ast_idgen,
    ast,
    idents
  ],
  compiler/mir/[
    mirbodies,
    mirchangesets,
    mirconstr,
    mirenv,
    mirtypes,
    mirtrees,
    sourcemaps
  ],
  compiler/sem/[
    mirexec,
    liftdestructors
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ]

from compiler/ast/reports import ReportKind
from compiler/ast/reports_sem import SemReport
from compiler/front/msgs import localReport

const
  PathOps = {mnkPathPos, mnkPathNamed, mnkPathArray, mnkPathConv,
             mnkPathVariant}
  ViewTypes = {tkOpenArray, tkVar, tkLent}

proc simpleRoot(tree: MirTree, pos: NodePosition): NodePosition =
  ## Fetches the root of the operation at `pos`, without following aliases.
  result = pos
  while tree[result].kind in PathOps:
    result = NodePosition tree.operand(result, 0)

proc makeProc(g: ModuleGraph, idgen: IdGenerator, owner: PSym,
              pt: PType): PSym =
  ## Creates the continuation procedure symbol for.
  result = newSym(skProc, owner.name, nextSymId(idgen), owner, owner.info,
                  owner.options)
  result.typ = pt

  # the return type is the same
  let params = newTree(nkFormalParams, copyTree(owner.ast[paramsPos][0]))
  for i in 1..<pt.len:
    params.add newTree(nkIdentDefs,
      newSymNode(pt.n[i].sym),
      g.emptyNode,
      g.emptyNode)

  result.ast = newProcNode(nkProcDef,
    owner.info,
    name = newSymNode(result),
    body = newTree(nkStmtList), # the body only exists at the MIR stage
    params = params,
    pattern = g.emptyNode,
    genericParams = g.emptyNode,
    pragmas = g.emptyNode,
    exceptions = g.emptyNode)

  # prevent transf running on the procedure:
  g.setTransformed(result, result.ast[bodyPos])

proc newField(g: ModuleGraph, idgen: IdGenerator, owner: PSym,
              name: string, typ: PType): PSym =
  newSym(skField, g.cache.getIdent(name), nextSymId(idgen), owner,
         owner.info, typ)

proc newNode(kind: range[mnkParam..mnkLocal], typ: TypeId,
             id: LocalId): MirNode =
  MirNode(kind: kind, typ: typ, local: id)

proc canCopy(g: ModuleGraph, t: PType): bool =
  let op = getAttachedOp(g, t.skipTypes(skipForHooks), attachedAsgn)
  op.isNil or sfError notin op.flags

proc prepareFork*(body: MirBody, changes: var Changeset) =
  ## Captures the values of all owning locals used beyond a 'fork', to make
  ## sure that unique ownership continues to hold.
  template tree: MirTree = body.code

  var
    dfg = computeDfg(tree)
    pos = NodePosition(0)
    locs: seq[MirNode]
    scopes: seq[int]

  proc isLastUse(tree: MirTree, dfg: DataFlowGraph, pos: NodePosition,
                 loc: LocalId): int =
    let all = dfg.subgraphFor(NodePosition(0) .. NodePosition(tree.len))
    var state = TraverseState()

    template isLocal(p: NodePosition|OpValue): bool =
      tree[p].kind in {mnkLocal, mnkParam, mnkTemp} and tree[p].local == loc

    for op, arg in traverse(dfg, all, dfg.find(pos), state):
      let root = simpleRoot(tree, NodePosition arg)
      case op
      of opUse, opMutate, opInvalidate, opConsume:
        if isLocal(root):
          result = 2
          break
      of opDef, opDestroy:
        if isLocal(arg):
          result = 1
          state.exit = true
        elif isLocal(root):
          result = 1
      of opKill:
        if isLocal(arg):
          state.exit = true
      of opMutateGlobal:
        discard "not relevant"

    if result != 2 and loc == resultId and state.exit:
      # the result variable is implicitly used when returning
      result = 2

  # also consider the result variable
  if body[resultId].typ != VoidType:
    locs.add MirNode(kind: mnkLocal, typ: body[resultId].typ, local: resultId)

  while pos < NodePosition(tree.len):
    case tree[pos].kind
    of mnkDef:
      # only owning locals are relevant
      locs.add tree[pos, 0]
    of mnkScope:
      scopes.add locs.len
    of mnkEndScope:
      locs.shrink(scopes.pop())
    of mnkFork:
      locs.shrink(locs.len - 1)
      for it in locs.items:
        let mode = isLastUse(tree, dfg, tree.sibling(pos), it.local)
        # if the local is used, its pre-fork value must be captured before the
        # fork and restored afterwards
        # if the local is only defined, it needs to be cleared after the fork
        if mode == 2:
          var tmp: Value
          changes.insert(tree, pos, pos, bu):
            tmp = bu.allocTemp(it.typ)
            bu.subTree mnkDef:
              bu.use tmp
              # the move analyzer will figure out whether to move or copy
              bu.subTree MirNode(kind: mnkSink, typ: it.typ):
                bu.add it

          changes.insert(tree, tree.sibling(pos), pos, bu):
            bu.subTree mnkInit:
              bu.add it
              bu.subTree MirNode(kind: mnkMove, typ: tmp.typ):
                bu.use tmp
        elif mode == 1:
          changes.insert(tree, tree.sibling(pos), pos, bu):
            bu.subTree mnkVoid:
              bu.buildMagicCall mWasMoved, VoidType:
                bu.emitByName ekKill:
                  bu.add it

    else:
      discard "nothing to do"

    pos = tree.sibling(pos)

proc firstPass*(body: MirBody, owner: PSym, g: ModuleGraph, idgen: IdGenerator,
                env: var MirEnv, changes: var Changeset) =
  ## * populates the continuation context objects, turning them into
  ##   complete objects
  ## * creates the symbols for the reified continuation procedures
  ## * lowers 'fork' into a tuple/object construction
  ## * turns all 'resume's into their decorated form
  type Cont = object
    ## Information gathered about a continuation.
    id: ProcedureId
    discr: int64
      ## the discriminator value of the context variant, if context type
      ## is a case object
    start: int
      ## position of the first field in the context type
    saved: seq[tuple[n: MirNode, isCursor: bool]]
      ## values saved
    locs: seq[MirNode]
      ## storage that's live at the start, but without storing a value

  type CtxObject = object
    ## Information about a context type.
    typ: TypeId
    numUsed: int
      ## number of times the context object is used by continuation types
    next: int
      ## position to use for the next added field

  template tree: MirTree = body.code

  var dfg = computeDfg(body.code)
  let all = dfg.subgraphFor(NodePosition(0) .. NodePosition(tree.len))
  var
    locs: seq[tuple[n: MirNode, isCursor: bool]]
      ## keeps track of all currently live locations, eligible for saving
    scopes: seq[int]
    conts: Table[LabelId, Cont]
    objects: Table[int, CtxObject]

  # the 'result' variable must be saved too (if not empty)
  if body[resultId].typ != VoidType:
    locs.add (
      MirNode(kind: mnkLocal, local: resultId, typ: body[resultId].typ),
      false)

  var pos = NodePosition(0)
  # traverse the statements and keep track of the live locations
  while pos < NodePosition(tree.len):
    case tree[pos].kind
    of mnkScope:
      scopes.add locs.len
    of mnkEndScope:
      let start = scopes.pop()
      locs.shrink(start)
    of mnkDef, mnkDefCursor, mnkBind, mnkBindMut:
      if tree[pos, 0].kind in {mnkAlias, mnkParam, mnkTemp, mnkLocal}:
        # also pick up aliases, even though they cannot be saved
        locs.add (tree[pos, 0], tree[pos].kind == mnkDefCursor)
    of mnkFork:
      let lab = tree[tree.last(pos)].label
      if lab notin conts:
        # setup the procedure
        let pt = env.types[tree[pos, 0].typ][1]
        conts[lab] = Cont(id: env.procedures.add(makeProc(g, idgen, owner, pt)))
    of mnkResume:
      # gather the state that needs to be saved
      var save: Cont.saved
      var empty: Cont.locs
      for it in locs.items:
        var
          usedAt = SourceId(0)
          state = TraverseState()
          mode = 0
        # mode == 1 -> only the storage location is used afterward
        # mode == 2 -> the value is used afterwards
        for op, arg in traverse(dfg, all, dfg.find(pos), state):
          let root = simpleRoot(tree, NodePosition arg)
          case op
          of opUse, opInvalidate, opMutate, opConsume:
            if tree[root].kind in {mnkParam, mnkLocal, mnkTemp, mnkAlias} and
               tree[root].local == it.n.local:
              # the value stored in the location is used on some path
              mode = 2
              usedAt = tree[arg].info
              break
          of opDef, opKill, opDestroy:
            if tree[root].kind in {mnkParam, mnkLocal, mnkTemp, mnkAlias} and
               tree[root].local == it.n.local:
              mode = 1
              usedAt = tree[arg].info
              state.exit = true
          of opMutateGlobal:
            discard "not relevant"

        if mode != 2 and it.n.local == resultId and state.exit:
          # the result variable is used implictly on returning
          mode = 2
          # TODO: make returns explicit in the MIR, so that the result doesn't
          #       have to be special-cased like this

        let canon = env.types.canonical(it.n.typ)
        if (mode == 2 and
            env.types.headerFor(canon, Canonical).kind in ViewTypes) or
           (mode != 0 and it.n.kind == mnkAlias):
          # TODO: report this error as part of borrow checking
          g.config.localReport(body.source[usedAt].info,
            SemReport(kind: rsemCannotBorrowAcrossSuspend,
                      ast: body.source[usedAt]))
          continue

        if mode == 2 and not canCopy(g, env[it.n.typ]):
          g.config.localReport(body.source[it.n.info].info,
            SemReport(kind: rsemCannotSaveLocal,
                      ast: body.source[usedAt]))
          continue

        case mode
        of 1:
          empty.add it.n
        of 2:
          save.add (it.n, it.isCursor)
        else:
          discard "ignore"

      # error detection and reporting:
      var state = TraverseState()
      for op, arg in traverse(dfg, all, dfg.find(pos), state):
        case op
        of opUse, opInvalidate, opMutate, opConsume, opDef, opKill, opDestroy:
          let root = simpleRoot(tree, NodePosition arg)
          if tree[root].kind == mnkParam and env[tree[root].typ].kind != tySink:
            # TODO: report this error as part of borrow checking
            g.config.localReport(body.source[tree[root].info].info,
              SemReport(kind: rsemCannotBorrowParamAcrossSuspend,
                        ast: body.source[tree[root].info]))
            break
        of opMutateGlobal:
          discard "not relevant"

      # the order in the list is the order in which they're added to the
      # context type, which is also the order they're destroyed in
      reverse(save)
      conts[tree[pos, 0].label].saved = save
      conts[tree[pos, 0].label].locs = empty
    else:
      discard "ignore"

    pos = tree.sibling(pos)

  # set up the object info table:
  for cont in conts.values:
    let ctx = env[cont.id].typ[^1].lastSon
    objects.withValue ctx.id, val:
      inc val.numUsed
    do:
      objects[ctx.id] = CtxObject(typ: env.types.add(ctx), numUsed: 1)

  # turn context types that are used multiple times into case objects:
  for obj in objects.mvalues:
    let ctx = env.types[obj.typ]
    if obj.numUsed > 1:
      let
        intType = g.getSysType(owner.info, tyInt)
        f = newField(g, idgen, ctx.sym, "sel", intType)
        rng = newType(tyRange, nextTypeId(idgen), ctx.sym.owner)
      rng.n = newTree(nkRange,
                      newIntTypeNode(0, intType),
                      newIntTypeNode(obj.numUsed - 1, intType))
      rng.rawAddSon(intType)
      f.typ = rng
      f.flags.incl sfDiscriminant
      ctx.n.add newTree(nkRecCase, newSymNode(f))
      inc obj.next

  # fill the context types:
  for cont in conts.mvalues:
    let ctx = env[cont.id].typ[^1].lastSon
    var n: PNode
    if objects[ctx.id].numUsed > 1:
      # append to a new variant
      n = newTree(nkRecList)
      cont.discr = ctx.n[0].len - 1
      ctx.n[0].add newTree(nkOfBranch,
        newIntTypeNode(cont.discr, ctx.n[0][0].typ), n)
    else:
      # append to the type
      cont.discr = -1 # no discriminator
      n = ctx.n

    cont.start = objects[ctx.id].next

    for i, it in cont.saved.pairs:
      let
        pos = objects[ctx.id].next
        f = newField(g, idgen, ctx.sym, "_" & $pos, env[it.n.typ])
      f.position = pos
      inc objects[ctx.id].next
      if it.isCursor:
        f.flags.incl sfCursor
      n.add newSymNode(f)

  # complete the context types:
  for obj in objects.values:
    resolveForwardOps(g, idgen, env.types[obj.typ], owner.info)
    env.types.complete(env.types[obj.typ])

  # second pass: perform the actual lowering (replace 'fork's and
  # decorate 'resume's)
  for pos, n in tree.pairs:
    case n.kind
    of mnkFork:
      let
        dest = tree.child(pos, 0)
        lab = tree[tree.last(pos)].label

      changes.remove(tree, pos)
      # context saving:
      let envTyp = env.types[env.types.lookupField(tree[dest].typ, 0)].typ
      changes.insert(tree, tree.sibling(pos), pos, bu):
        bu.subTree mnkInit:
          bu.pathPos envTyp, 0:
            bu.emitFrom(tree, dest)
          bu.subTree MirNode(kind: mnkObjConstr, typ: envTyp):
            if conts[lab].discr != -1:
              bu.subTree mnkBinding:
                bu.add MirNode(kind: mnkField, field: 0)
                bu.subTree mnkConsume:
                  bu.use literal(mnkIntLit, env.getOrIncl(conts[lab].discr),
                                 env.types.sizeType)

            for i, it in conts[lab].saved.pairs:
              bu.subTree mnkBinding:
                bu.add MirNode(kind: mnkField,
                               field: int32(conts[lab].start + i))
                bu.subTree (if it.isCursor: mnkArg else: mnkConsume):
                  bu.add it.n

      # procval initialization:
      let ptyp = env.types[env.types.lookupField(tree[dest].typ, 1)].typ
      changes.insert(tree, tree.sibling(pos), pos, bu):
        bu.subTree mnkInit:
          bu.pathPos ptyp, 1:
            bu.emitFrom(tree, dest)
          bu.add MirNode(kind: mnkProcVal, prc: conts[lab].id, typ: ptyp)
    of mnkResume:
      # add the procedure ID and the list of saved/reused locals
      let lab = tree[pos, 0].label
      changes.replaceMulti(tree, pos, bu):
        bu.subTree mnkResume:
          bu.emitFrom(tree, tree.child(pos, 0))
          bu.add MirNode(kind: mnkProc, prc: conts[lab].id)
          # the saved locals come first
          for i, it in conts[lab].saved.pairs:
            bu.subTree mnkBinding:
              bu.add MirNode(kind: mnkField,
                             field: int32(conts[lab].start + i))
              bu.add it.n
          # then the used locations
          for it in conts[lab].locs.items:
            bu.add it
    else:
      discard "nothing to do"

proc filter(body: MirBody, cont: ProcedureId, env: var MirEnv): MirBody =
  ## Creates a new body from `body`, by filtering out everything not part of
  ## the given continuation.
  template tree: MirTree = body.code
  var
    saved: Table[LocalId, tuple[typ: TypeId, pos: int32]]
    empty: PackedSet[LocalId]
    ## locals whose storage is live across the fork, but whose value is
    ## not saved
    localMap: Table[LocalId, MirNode]
      ## old local -> new local
    labelMap: Table[LabelId, LabelId]
      ## old label -> new label
    scopes: seq[NodePosition]
    loops: seq[tuple[live: bool, start: NodePosition]]
      ## stack of all loops enclosing the current cursor
    ctxParam: MirNode
    inputParam: MirNode
    bu = initBuilder(SourceId(0))

  discard bu.addLocal(body[resultId])
  localMap[resultId] = newNode(mnkLocal, body[resultId].typ, resultId)

  # parameters:
  proc newParam(env: var MirEnv, s: PSym): MirNode =
    let typ = env.types.add(s.typ)
    newNode(mnkParam, typ, bu.addLocal(Local(name: s.name, typ: typ)))

  let ptyp = env[cont].typ
  if ptyp.len == 3:
    inputParam = newParam(env, ptyp.n[1].sym)
    ctxParam = newParam(env, ptyp.n[2].sym)
  else:
    inputParam = MirNode(kind: mnkNone)
    ctxParam = newParam(env, ptyp.n[1].sym)

  # first step: look for the continuation's entry point and collect all scope
  # and loop starts leading up to it
  var pos = NodePosition(0)
  while pos < NodePosition(tree.len):
    case tree[pos].kind
    of mnkScope:
      scopes.add pos
    of mnkEndScope:
      scopes.shrink(scopes.len - 1)
    of mnkLoopJoin:
      loops.add (false, pos)
    of mnkLoop:
      loops.shrink(loops.len - 1)
    of mnkResume:
      if tree[pos, 1].prc == cont:
        # found it!
        break
    else:
      discard "nothing to do"
    pos = tree.sibling(pos)

  # populate the tables using the 'resume' statement:
  let num = ptyp[^1].lastSon.n.len
  for i in 0..<num:
    let b = tree.child(pos, i + 2)
    saved[tree[b, 1].local] = (tree[b, 1].typ, tree[b, 0].field)

  for i in (2 + num)..<tree[pos].len.int:
    empty.incl(tree[pos, i].local)

  proc def(body: MirBody, n: MirNode) =
    ## Processes the `n` appearing in a def position.
    let loc = bu.addLocal(body[n.local])
    if n.kind == mnkParam:
      # parameters become locals
      localMap[n.local] =
        MirNode(kind: mnkLocal, local: loc, typ: n.typ, info: n.info)
    else:
      var tmp = n
      tmp.local = loc
      localMap[n.local] = tmp

  proc pop[K, V](t: var Table[K, V], k: K): bool =
    var tmp: V
    pop(t, k, tmp)

  # second step: copy all the defs (of saved or re-used locals) and their
  # associated scope starts leading up to the entry point; discard the rest
  pos = NodePosition(0)
  while pos < NodePosition(tree.len):
    case tree[pos].kind
    of mnkScope:
      if scopes.len > 0 and scopes[0] == pos:
        scopes.delete(0)
        bu.emitFrom(tree, pos)
    of mnkDef, mnkDefCursor:
      let name = tree[pos, 0]
      if name.kind == mnkGlobal:
        discard "ignore"
      elif name.local in empty:
        def(body, name)
        bu.setSource(tree[pos].info)
        # the cursor-ness is not relevant anymore
        bu.subTree mnkDef:
          bu.add localMap[name.local]
          bu.add MirNode(kind: mnkNone)
      elif name.local in saved:
        def(body, name)
        bu.setSource(tree[pos].info)
        bu.subTree mnkDef:
          bu.add localMap[name.local]
          bu.subTree MirNode(kind: mnkMove, typ: tree[pos, 0].typ):
            bu.pathNamed tree[pos, 0].typ, saved[name.local].pos:
              bu.add ctxParam
    of mnkResume:
      if tree[pos, 1].prc == cont:
        # found the entry point
        pos = tree.sibling(pos)
        break
      # else: some other 'resume', ignore
    else:
      discard "nothing from the"

    pos = tree.sibling(pos)

  proc copy(body: MirBody, pos: NodePosition) =
    ## Emits the sub-tree from `body` at `pos` while patching all local
    ## references within.
    bu.pop(bu.push do:
      bu.emitFrom(tree, pos)
      for n in bu.staging.mitems:
        case n.kind
        of mnkLocal, mnkTemp, mnkParam, mnkAlias:
          n = localMap[n.local]
        of mnkLabel:
          let id = n.label
          # allocate and register the new label on demand, which also marks
          # the associated join as used/live
          labelMap.withValue id, val:
            n.label = val[]
          do:
            let nid = bu.allocLabel()
            labelMap[id] = nid
            n.label = nid
        else:
          discard "nothing to do"
    )

  proc copy(body: MirBody, pos, endp: NodePosition) =
    ## Emits everything up until `endp`, starting from `pos`.
    var
      pos = pos
      active = true
      scopes = newSeq[bool]()
    while pos < endp:
      case tree[pos].kind
      of mnkLoopJoin:
        loops.add (active, pos)
        if active:
          copy(body, pos)
      of mnkLoop:
        let (live, start) = pop(loops)
        if not live and active:
          # a loop with live end but without a live start, which happens when
          # resuming within loop bodies
          copy(body, start)
          copy(body, tree.sibling(start), pos)
          copy(body, pos)
        elif live:
          # a loop start always need to be paired with an end
          copy(body, pos)
        active = false
      of mnkIf:
        if active:
          copy(body, pos)
      of mnkJoin, mnkFinally:
        # don't copy joins only reached by structured control-flow
        if tree[pos, 0].label in labelMap:
          copy(body, pos)
          # now drop the label
          labelMap.del(tree[pos, 0].label)
          active = true
      of mnkExcept:
        if tree[pos, 0].label in labelMap:
          copy(body, pos)
          # keep the label, so that the 'endstruct' statement is kept
          active = true
      of mnkEndStruct:
        # if the start was not live, neither is the end
        if tree[pos, 0].label in labelMap:
          copy(body, pos)
          labelMap.del(tree[pos, 0].label)
          active = true
      of mnkDef, mnkDefCursor, mnkBind, mnkBindMut:
        if active:
          if tree[pos, 0].kind in {mnkTemp, mnkLocal, mnkParam, mnkAlias}:
            def(body, tree[pos, 0])
          copy(body, pos)
      of mnkRaise, mnkContinue, mnkGoto, mnkCase:
        if active:
          copy(body, pos)
        active = false
      of mnkScope:
        if active:
          copy(body, pos)
        scopes.add active
      of mnkEndScope:
        if scopes.len > 0:
          if scopes.pop():
            copy(body, pos)
        else:
          # make sure scopes already open before `copy` was called are closed
          copy(body, pos)
      else:
        if active:
          copy(body, pos)

      pos = tree.sibling(pos)

  if resultId in saved:
    # restore the result variable's value
    bu.setSource(tree[pos].info)
    bu.subTree mnkInit:
      bu.add newNode(mnkLocal, body[resultId].typ, resultId)
      bu.pathNamed saved[resultId].typ, saved[resultId].pos:
        bu.add ctxParam

  if inputParam.kind != mnkNone:
    # move the input parameter into the 'def' following the 'resume'
    def(body, tree[pos, 0])
    bu.setSource(tree[pos].info)
    bu.subTree mnkDef:
      bu.add localMap[tree[pos, 0].local]
      bu.subTree MirNode(kind: mnkMove, typ: inputParam.typ):
        bu.add inputParam
    pos = tree.sibling(pos)

  # third step: copy over the remainder of the body, but only the
  # parts reachable from the resumption point
  copy(body, pos, tree.len.NodePosition)
  result = createBody(move bu, body.source)

proc removeUnreachableCode(tree: MirTree, changes: var Changeset) =
  ## Removes all unreachable code from the `tree`.
  var live: PackedSet[LabelId]

  proc remove(tree: MirTree, pos: var NodePosition, changes: var Changeset) =
    ## Remove everything until reaching the next live join.
    var depth = 0 # tracks the nesting of bracketed statements
    while pos < NodePosition(tree.len):
      case tree[pos].kind
      of mnkJoin, mnkFinally:
        if not missingOrExcl(live, tree[pos, 0].label):
          break
        changes.remove(tree, pos)
      of mnkExcept:
        inc depth
        if not missingOrExcl(live, tree[pos, 0].label):
          break
        changes.remove(tree, pos)
      of mnkIf, mnkScope, mnkLoopJoin:
        inc depth
        changes.remove(tree, pos)
      of mnkEndScope, mnkLoop:
        if depth == 0:
          discard "needs to be kept"
        else:
          dec depth
          changes.remove(tree, pos)
      of mnkEndStruct:
        if not missingOrExcl(live, tree[pos, 0].label):
          break
        elif depth > 0:
          dec depth
          changes.remove(tree, pos)
        # else: keep
      else:
        changes.remove(tree, pos)

      pos = tree.sibling(pos)

    if pos < NodePosition(tree.len):
      # skip past the re-entry point
      pos = tree.sibling(pos)

  # when in live mode:
  # * keep the statements
  # * mark all used in the statement labels as live
  # * switch to non-live mode after every goto-like statement
  # when not in live mode: remove statements until reaching a join point
  # using a live label
  var pos = NodePosition(0)
  while pos < NodePosition(tree.len):
    case tree[pos].kind
    of mnkIf:
      live.incl(tree[pos, 1].label)
    of mnkDef, mnkDefCursor, mnkVoid, mnkAsgn, mnkInit, mnkSwitch:
      let e = tree.last(pos)
      if tree[e].kind == mnkCheckedCall and
         tree[tree.last(e)].kind != mnkUnwind:
        live.incl(tree[tree.last(e)].label)
    of mnkRaise, mnkContinue:
      if tree[tree.last(pos)].kind != mnkUnwind:
        live.incl(tree[tree.last(pos)].label)
      pos = tree.sibling(pos)
      remove(tree, pos, changes)
      continue
    of mnkGoto:
      live.incl(tree[pos, 0].label)
      pos = tree.sibling(pos)
      remove(tree, pos, changes)
      continue
    of mnkCase:
      for it in tree.subNodes(pos, 1):
        live.incl(tree[tree.last(it)].label)
      pos = tree.sibling(pos)
      remove(tree, pos, changes)
      continue
    of mnkFinally, mnkExcept:
      # remove the label; keeps the set smaller
      live.excl(tree[pos, 0].label)
    of mnkJoin:
      # if the join is only reached by structured control-flow, remove it
      if missingOrExcl(live, tree[pos, 0].label):
        changes.remove(tree, pos)
    else:
      discard "keep"

    pos = tree.sibling(pos)

proc secondPass*(body: MirBody, env: var MirEnv, changes: var Changeset) =
  ## Reifies all continuations in the given `body`.
  var conts: seq[ProcedureId]
  # gather all continuation procedures:
  for pos, n in body.code.pairs:
    if n.kind == mnkResume:
      conts.add body.code[pos, 1].prc

  for it in conts.items:
    let got = filter(body, it, env)
    env.pbodies[it] = got

  # remove the now-unreachable code from the split-up procedure
  removeUnreachableCode(body.code, changes)

proc containsFork*(tree: MirTree): bool =
  ## Whether `tree` contains a 'fork' operation.
  result = false
  for it in tree.items:
    if it.kind == mnkFork:
      result = true
      break
