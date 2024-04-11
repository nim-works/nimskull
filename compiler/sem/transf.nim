#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the transformator. It transforms the syntax tree
## to ease the work of the code generators. Does some transformations:
##
## * inlines iterators
## * converts "continue" to "break"; disambiguates "break"
## * introduces method dispatchers
## * performs lambda lifting for closure support
## * transforms 'defer' into a 'try finally' statement

import
  std/[
    intsets
  ],
  compiler/ast/[
    ast,
    astalgo,
    trees,
    idents,
    renderer,
    types,
    lineinfos
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/sem/[
    ast_analysis,
    closureiters,
    coroutines,
    semfold,
    lambdalifting,
    lowerings
  ],
  compiler/backend/[
    cgmeth
  ],
  compiler/utils/[
    idioms
  ]

from compiler/sem/semdata import makeVarType

type
  PTransCon = ref object # part of TContext; stackable
    mapping: TIdNodeTable     # mapping from symbols to nodes
    owner: PSym               # current owner
    forStmt: PNode            # current for stmt
    forLoopBody: PNode   # transformed for loop body
    yieldStmts: int           # we count the number of yield statements,
                              # because we need to introduce new variables
                              # if we encounter the 2nd yield statement
    next: PTransCon           # for stacking

    definedSyms: IntSet
      ## tracks which symbols appeared in a definition context already. This
      ## is used for what is fundamentally a work-around / fix-up: if typed
      ## AST arguments that contain definitions are used as substitutions
      ## multiple times, the exact same symbol *instance* appears in a definition
      ## position multiple times, which violates the expectations of the mid-
      ## and back-end. Until semantic analysis properly sanitizes the AST, we
      ## workaround the issue here by introducing new symbols for duplicates.
      ## We only do so for ``var``, ``let``, and ``const`` symbols, however --
      ## the others will still cause issues.

  PTransf = ref object
    module: PSym
    transCon: PTransCon      # top of a TransCon stack
    inlining: int            # > 0 if we are in inlining context (copy vars)
    nestedProcs: int         # > 0 if we are in a nested proc
    contSyms, breakSyms: seq[PSym]  # to transform 'continue' and 'break'
    deferDetected: bool
    graph: ModuleGraph
    idgen: IdGenerator

    env: PSym ## the symbol of the local (or parameter) through which
              ## the lifted local environment is accessed. 'nil', if
              ## none exists.

proc transformBody*(g: ModuleGraph; idgen: IdGenerator, prc: PSym, cache: bool): PNode

proc pushTransCon(c: PTransf, t: PTransCon) =
  t.next = c.transCon
  c.transCon = t

proc popTransCon(c: PTransf) =
  c.graph.config.internalAssert(c.transCon != nil, "popTransCon")

  c.transCon = c.transCon.next

proc getCurrOwner(c: PTransf): PSym =
  if c.transCon != nil: result = c.transCon.owner
  else: result = c.module

proc newTemp(c: PTransf, typ: PType, info: TLineInfo): PNode =
  let r = newSym(skTemp, getIdent(c.graph.cache, genPrefix), nextSymId(c.idgen), getCurrOwner(c), info)
  r.typ = typ #skipTypes(typ, {tyGenericInst, tyAlias, tySink})
  incl(r.flags, sfFromGeneric)
  let owner = getCurrOwner(c)
  if owner.isIterator or owner.isCoroutine:
    result = freshVarForClosureIter(c.graph, r, c.idgen, owner)
  else:
    result = newSymNode(r)

proc transform(c: PTransf, n: PNode): PNode

proc transformSons(c: PTransf, n: PNode): PNode =
  result = shallowCopy(n)
  for i in 0..<n.len:
    result[i] = transform(c, n[i])

proc transformSymAux(c: PTransf, n: PNode): PNode =
  let s = n.sym
  if s.typ != nil and s.typ.callConv == ccClosure:
    if s.kind == skIterator:
      # we need the finished environment type here (because we inserting a
      # non-nil instance of it, and thus need valid type-bound operators),
      # which is only available after transforming the routine...
      discard transformBody(c.graph, c.idgen, s, true)

      return liftIterSym(c.graph, n, c.idgen, getCurrOwner(c), c.env)
    elif s.kind in {skProc, skFunc, skConverter, skMethod}:
      # top level .closure procs are still somewhat supported for 'Nake':
      ensureEnvParam(c.graph, c.idgen, s)
      return makeClosure(c.graph, c.idgen, s, nil, n.info)

  var b: PNode
  var tc = c.transCon
  if sfBorrow in s.flags and s.kind in routineKinds:
    # Recurse all the procs and swap syms
    var s = s
    while true:
      # Skips over all borrowed procs getting the last proc symbol without an implementation
      let body = getBody(c.graph, s)
      if body.kind == nkSym and sfBorrow in body.sym.flags and getBody(c.graph, body.sym).kind == nkSym:
        s = body.sym
      else:
        break
    b = getBody(c.graph, s)
    c.graph.config.internalAssert(b.kind == nkSym, n.info, "wrong AST for borrowed symbol")

    b = newSymNode(b.sym, n.info)
  elif c.inlining > 0:
    # see bug #13596: we use ref-based equality in the DFA for destruction
    # injections so we need to ensure unique nodes after iterator inlining
    # which can lead to duplicated for loop bodies! Consider:
    #[
      while remaining > 0:
        if ending == nil:
          yield ms
          break
        ...
        yield ms
    ]#
    b = newSymNode(n.sym, n.info)
  else:
    b = n
  while tc != nil:
    result = idNodeTableGet(tc.mapping, b.sym)
    if result != nil:
      # this slightly convoluted way ensures the line info stays correct:
      if result.kind == nkSym:
        result = copyNode(result)
        result.info = n.info
      return
    tc = tc.next
  result = b

proc transformSym(c: PTransf, n: PNode): PNode =
  result = transformSymAux(c, n)

proc freshVar(c: PTransf; v: PSym): PNode =
  let owner = getCurrOwner(c)
  if {sfGlobal, sfThread} * v.flags != {}:
    # don't introduce copies of symbols of globals. The processing
    # following after ``transf`` expects that the set of existing globals
    # stays unchanged
    result = newSymNode(v)
  elif owner.isIterator or owner.isCoroutine:
    result = freshVarForClosureIter(c.graph, v, c.idgen, owner)
  else:
    var newVar = copySym(v, nextSymId(c.idgen))
    incl(newVar.flags, sfFromGeneric)
    newVar.owner = owner
    result = newSymNode(newVar)

proc transformDefSym(c: PTransf, n: PNode): PNode {.deprecated: "workaround for sem not sanitizing AST".} =
  ## Transforms a node that appears in a definition position. If the same
  ## symbol already appeared in a definition position, a copy is created, and
  ## a mapping registered so that all further usages of the symbol use the
  ## copy.
  # XXX: this is only intended to fix the most simple cases of duplicate symbols
  #      caused by substitution with typed AST (or by macros generating
  #      semantically invalid AST). Remove ``transformDefSym`` once semantic
  #      analysis properly sanitizes the AST from template/macro expansions.
  if n.kind in {nkDotExpr, nkIdent}:
    # * nkDotExpr: a variable lifted into a closure environment
    # * nkIdent: an erroneously visited node in a declarative context
    return transform(c, n)

  assert n.kind == nkSym
  let s = n.sym

  if containsOrIncl(c.transCon.definedSyms, s.id):
    # the symbol already appeared in a definition position; create a copy and
    # add a mapping
    result = freshVar(c, s)
    idNodeTablePut(c.transCon.mapping, s, result)
  else:
    result = transformSym(c, n)

proc transformVarSection(c: PTransf, v: PNode): PNode =
  c.graph.config.internalAssert(v.kind in nkVariableSections,
                                "not a variable section, got: " & $v.kind)

  result = shallowCopy(v)

  for i in 0..<v.len:
    var it = v[i]
    case it.kind
    of nkCommentStmt:
      result[i] = it
    of nkIdentDefs:
      if it[0].kind == nkSym:
        internalAssert(c.graph.config, it.len == 3,
                       "var section must have three subnodes, got: " & $it.len)
        
        let x = freshVar(c, it[0].sym)
        
        idNodeTablePut(c.transCon.mapping, it[0].sym, x)
        
        var defs = newTreeI(nkIdentDefs, it.info):
          [x, it[1], transform(c, it[2])]
        
        if importantComments(c.graph.config):
          # keep documentation information:
          defs.comment = it.comment
        
        if x.kind == nkSym:
          x.sym.ast = defs[2]
        
        result[i] = defs
      else:
        # has been transformed into 'param.x' for closure iterators, so just
        # transform it:
        result[i] = transform(c, it)
    of nkVarTuple:
      let defs = newNodeI(it.kind, it.info, it.len)

      for j in 0..<it.len-2:
        defs[j] =
          if it[j].kind == nkSym:
            let x = freshVar(c, it[j].sym)
            idNodeTablePut(c.transCon.mapping, it[j].sym, x)
            x
          else:
            transform(c, it[j])

      assert(it[^2].kind == nkEmpty)

      defs[^2] = newNodeI(nkEmpty, it.info)
      defs[^1] = transform(c, it[^1])

      result[i] = defs
    else:
      c.graph.config.internalError(it.info):
        "transformVarSection expected identdefs, tuple, or comment, got: " &
          $it.kind

proc transformConstSection(c: PTransf, v: PNode): PNode =
  result = v
  when false:
    result = shallowCopy(v)
    for i in 0..<v.len:
      var it = v[i]
      if it.kind == nkCommentStmt:
        result[i] = it
      else:
        c.graph.config.internalAssert(it.kind == nkConstDef, it.info, "transformConstSection")
        c.graph.config.internalAssert(it[0].kind == nkSym, it.info, "transformConstSection")

        result[i] = it

proc hasContinue(n: PNode): bool =
  case n.kind
  of nkWithoutSons, nkForStmt, nkWhileStmt: discard
  of nkContinueStmt: result = true
  of nkWithSons - {nkForStmt, nkWhileStmt, nkContinueStmt}:
    for i in 0..<n.len:
      if hasContinue(n[i]): return true

proc newLabel(c: PTransf, n: PNode): PSym =
  result = newSym(skLabel, nil, nextSymId(c.idgen), getCurrOwner(c), n.info)
  result.name = getIdent(c.graph.cache, genPrefix)

proc transformBlock(c: PTransf, n: PNode): PNode =
  result = shallowCopy(n)
  if c.inlining > 0:
    # all blocks that reach here are labeled. We still need to ensure that
    # unique symbols are used, so introduce both a copy and associated
    # mapping
    let labl = copySym(n[0].sym, nextSymId c.idgen)
    labl.info = n[0].sym.info
    labl.owner = getCurrOwner(c)
    idNodeTablePut(c.transCon.mapping, n[0].sym, newSymNode(labl))

    result[0] = newSymNode(labl, n[0].info)
    # the breaks in the AST were all already transformed
    result[1] = transform(c, n[1])
  else:
    let labl =
      if n[0].kind != nkEmpty:
        n[0].sym  # already named block? -> Push symbol on the stack
      else:
        newLabel(c, n)

    result[0] = newSymNode(labl, n[0].info)
    c.breakSyms.add(labl)
    result[1] = transform(c, n[1])
    discard c.breakSyms.pop

proc transformLoopBody(c: PTransf, n: PNode): PNode =
  # What if it contains "continue" and "break"? "break" needs
  # an explicit label too, but not the same!

  # We fix this here by making every 'break' belong to its enclosing loop
  # and changing all breaks that belong to a 'block' by annotating it with
  # a label (if it hasn't one already).
  if hasContinue(n):
    let labl = newLabel(c, n)
    c.contSyms.add(labl)

    result = newTreeI(nkBlockStmt, n.info):
      [newSymNode(labl), transform(c, n)]
    discard c.contSyms.pop()
  else:
    result = transform(c, n)

proc transformWhile(c: PTransf; n: PNode): PNode =
  if c.inlining > 0:
    result = transformSons(c, n)
  else:
    # transform a while loop with an arbitrary condition expression into a
    # ``while true`` loop, as those are much easier to process for data-flow
    # analysis and the closure-iterator transformation
    let labl = newLabel(c, n)
    c.breakSyms.add(labl)

    let info = n[0].info
    var
      loop = shallowCopy(n)
      cond = transform(c, n[0])

    # build the transformed loop
    if isTrue(cond):
      # the condition is already statically 'true', no condition handling is
      # needed
      loop[0] = cond
      loop[1] = transformLoopBody(c, n[1])
    else:
      loop[0] = newIntTypeNode(1, c.graph.getSysType(info, tyBool))
      loop[0].info = info

      # unwrap the statement list expression. It helps with the following
      # lowering, and it's also necessary for the closure iterator
      # transformation
      var preamble = PNode(nil)
      if cond.kind in {nkStmtListExpr, nkStmtList}:
        preamble = newNodeI(nkStmtList, info, cond.len - 1)
        for i in 0..<preamble.len:
          preamble[i] = cond[i]

        cond = cond[^1]

      # all definitions part of the condition expression are part of the while's
      # scope, placing the expression into the if's condition slot would thus
      # result in incorrect scoping
      if not isAtom(cond):
        let tmp = newTemp(c, cond.typ, cond.info)
        if preamble.isNil:
          preamble = newTree(nkStmtList)
        preamble.add newTree(nkLetSection, newIdentDefs(tmp, cond))
        cond = tmp

      let exit =
        newTreeI(nkIfStmt, info,
          newTreeI(nkElifBranch, info,
            newTreeIT(nkCall, info, cond.typ,
              newSymNode(c.graph.getSysMagic(info, "not", mNot)),
              cond),
            newBreakStmt(info, labl)))

      var body = transformLoopBody(c, n[1])
      # use a nested scope for the body. This is important for the clean-up
      # semantics, as exiting the loop via the ``break`` used by the exit
      # handling must not run finalizers (if present) for the loop's body
      if body.kind != nkBlockStmt:
        body = newTreeI(nkBlockStmt, n[1].info):
          [newSymNode(newLabel(c, body)), body]

      loop[1] =
        if preamble.isNil: newTree(nkStmtList, [exit, body])
        else:              newTree(nkStmtList, [preamble, exit, body])

    result = newTreeI(nkBlockStmt, n.info):
      [newSymNode(labl), loop]
    discard c.breakSyms.pop

proc transformBreak(c: PTransf, n: PNode): PNode =
  if n[0].kind == nkEmpty:
    # turn into a labeled break, using the break label stack
    result = newBreakStmt(n.info, c.breakSyms[^1])
  else:
    # already a labeled break
    result = transformSons(c, n)

proc introduceNewLocalVars(c: PTransf, n: PNode): PNode =
  case n.kind
  of nkSym:
    result = transformSym(c, n)
  of nkWithoutSons - nkSym:
    # nothing to be done for leaves:
    result = n
  of callableDefs:
    # warning: do not modify this AST; it's the same (as in identity) as
    # the one stored within the symbol's ``ast`` field
    result = n
  of nkVarSection, nkLetSection:
    result = transformVarSection(c, n)
  of nkClosure:
    # it can happen that for-loop-inlining produced a fresh
    # set of variables, including some computed environment
    # (bug #2604). We need to patch this environment here too:
    let a = n[1]
    if a.kind == nkSym:
      n[1] = transformSymAux(c, a)
    return n
  else:
    result = shallowCopy(n)
    for i in 0..<n.len:
      result[i] = introduceNewLocalVars(c, n[i])

proc newTupleAccess(n: PNode, formal: PType, i: Natural): PNode =
  ## Creates a new expression for accessing the `i`-th tuple element, taking
  ## views into account. `formal` is the target type
  let access =
    if n.typ.kind in {tyVar, tyLent}:
      # we need to get the underlying location first
      newTreeIT(nkHiddenDeref, n.info, n.typ.lastSon): n
    else:
      n

  result = newTupleAccessRaw(access, i)
  result.typ = access.typ.skipTypes({tyGenericInst, tyAlias})[i]

  # consider an iterator with ``var (int, var int)`` as the return type and
  # a yield with an expression that is not a literal tuple constructor. The
  # second element is already a view itself, so we must not create a new one
  # from it
  if formal.kind in {tyVar, tyLent} and result.typ.kind != formal.kind:
    assert result.typ.kind notin {tyVar, tyLent},
           "mismatching view type; sem should have rejected this"
    result = newTreeIT(nkHiddenAddr, n.info, formal): result

proc transformYieldAsgn(c: PTransf, dest, rhs: PNode, result: PNode) =
  ## Given a reciever (`dest`) and a source expression (`rhs`; the expression
  ## appearing in a ``yield`` statement), produces a sequence of identdefs for
  ## assigning the, potentially unpacked, source expression to the
  ## destination(s). The identdefs are then appended to `result`.
  case dest.kind
  of nkForStmt, nkVarTuple:
    # ignore conversions when unpacking
    let rhs = skipConv(rhs)

    if rhs.kind == nkTupleConstr:
      # assign each element directly, without going through a temporary location
      for i in 0..<rhs.len:
        transformYieldAsgn(c, dest[i], skipColon(rhs[i]), result)

    else:
      # something that is not a literal tuple constructor. Assign it to a
      # temporary first, which is then unpacked into the destination variables
      let
        tmp = newTemp(c, rhs.typ, rhs.info)
        tupTyp = rhs.typ.skipTypes({tyGenericInst, tyAlias, tyLent, tyVar})

      result.add newIdentDefs(tmp, transform(c, rhs))

      for i in 0..<tupTyp.len:
        transformYieldAsgn(c, dest[i],
                           newTupleAccess(tmp, dest[i].typ, i),
                           result)

  of nkSym, nkDotExpr:
    # ``nkDotExpr`` occurs for yields from closure iterators
    result.add newIdentDefs(dest, transform(c, rhs))
  else:
    unreachable()

proc transformYield(c: PTransf, n: PNode): PNode =
  result = newNodeI(nkStmtList, n.info)

  let e = n[0]
  if e.typ.isNil:
    # can happen in nimsuggest for unknown reasons
    return result

  let dest =
    if c.transCon.forStmt.len == 3:
      # a for loop with a single item. Note that this doesn't mean that no
      # tuple unpacking is used, e.g. ``for (x, y) in z``
      c.transCon.forStmt[0]
    else:
      # tuple unpacking is used
      c.transCon.forStmt

  let section = newNodeI(nkVarSection, n.info)
  transformYieldAsgn(c, dest, e, section)

  # note: it's important that the var section is also processed by
  # ``introduceNewLocalVars``
  result.add(section)
  result.add(c.transCon.forLoopBody)

  inc(c.transCon.yieldStmts)
  if c.transCon.yieldStmts > 1:
    # we need to introduce new local variables:
    result = introduceNewLocalVars(c, result)

  for idx in 0 ..< result.len:
    var changeNode = result[idx]
    changeNode.info = c.transCon.forStmt.info
    for i, child in changeNode:
      child.info = changeNode.info

  # wrap the inlined body in a block. This ensures that the lifetime of locals
  # defined inside the inlined body doesn't extend past the body. The for-loop
  # body was transformed already, so this doesn't interfere with ``break``
  # target rewriting
  result = newTreeI(nkBlockStmt, n.info):
    [newSymNode(newLabel(c, n)), result]

proc transformAddr(c: PTransf, n: PNode): PNode =
  result = transformSons(c, n)
  let n = result
  # collapse ``nkAddr( nkDerefExpr(x) )`` to ``x``, but only if ``x`` is of
  # pointer type. Performing the collapsing when ``x`` is a ``ref`` would be
  # incorrect, as there'd be a formal vs. actual type mismatch then
  if n[0].kind == nkDerefExpr and
     n[0][0].typ.skipTypes(abstractInst).kind == tyPtr:
    result = n[0][0]

proc generateThunk(c: PTransf; prc: PNode, dest: PType): PNode =
  ## Converts 'prc' into '(thunk, nil)' so that it's compatible with
  ## a closure.

  # we cannot generate a proper thunk here for GC-safety reasons
  # (see internal documentation):
  c.graph.config.internalAssert(prc.kind != nkClosure, prc.info, "closure to closure created")

  let conv = newTreeIT(nkHiddenSubConv, prc.info, dest):
    [newNodeI(nkEmpty, prc.info), prc]

  result = newTreeIT(nkClosure, prc.info, dest):
    [conv, newNodeIT(nkNilLit, prc.info, getSysType(c.graph, prc.info, tyNil))]

proc transformConv(c: PTransf, n: PNode): PNode =
  # numeric types need range checks:
  var dest = skipTypes(n.typ, abstractVarRange)
  var source = skipTypes(n[1].typ, abstractVarRange)
  case dest.kind
  of tyInt..tyInt64, tyEnum, tyChar, tyUInt8..tyUInt32:
    # we don't include uint and uint64 here as these are no ordinal types ;-)
    if not isOrdinalType(source):
      # float -> int conversions. ugh.
      result = transformSons(c, n)
    elif firstOrd(c.graph.config, n.typ) <= firstOrd(c.graph.config, n[1].typ) and
        lastOrd(c.graph.config, n[1].typ) <= lastOrd(c.graph.config, n.typ):
      # BUGFIX: simply leave n as it is; we need a nkConv node,
      # but no range check:
      result = transformSons(c, n)
    else:
      # generate a range check:
      let rangeDest = skipTypes(n.typ, abstractVar)
      result = newTreeIT(
        if tyInt64 in {dest.kind, source.kind}: nkChckRange64 else: nkChckRange,
        n.info, n.typ):
        [transform(c, n[1]),
         newIntTypeNode(firstOrd(c.graph.config, rangeDest), rangeDest),
         newIntTypeNode(lastOrd(c.graph.config, rangeDest), rangeDest)]
  of tyFloat..tyFloat64:
    # XXX int64 -> float conversion?
    let rangeDest = skipTypes(n.typ, abstractVar)
    if rangeDest.kind == tyRange:
      result = newTreeIT(nkChckRangeF, n.info, n.typ):
        [transform(c, n[1]), copyTree(rangeDest.n[0]), copyTree(rangeDest.n[1])]
    else:
      result = transformSons(c, n)
  of tyOpenArray, tyVarargs:
    result = transformSons(c, n)
    if dest.kind == tyVarargs:
      # XXX: for simpler handling in ``mirgen``, to-vararg conversions are
      #      changed into to-openArray conversions here. This needs to be
      #      removed again once the MIR uses its own type representation
      result.typ = copyType(dest, c.idgen.nextTypeId(), getCurrOwner(c))
      result.typ.kind = tyOpenArray
  of tyCstring:
    if source.kind == tyString:
      result = newTreeIT(nkStringToCString, n.info, n.typ): transform(c, n[1])
    else:
      result = transformSons(c, n)
  of tyString:
    if source.kind == tyCstring:
      result = newTreeIT(nkCStringToString, n.info, n.typ): transform(c, n[1])
    else:
      result = transformSons(c, n)
  of tyRef, tyPtr:
    dest = skipTypes(dest, abstractPtrs)
    source = skipTypes(source, abstractPtrs)
    case source.kind
    of tyObject:
      let diff = inheritanceDiff(dest, source)
      if diff == 0 or diff == high(int):
        result = transform(c, n[1])
        result.typ = n.typ
      else:
        result = newTreeIT(
          if diff < 0: nkObjUpConv else: nkObjDownConv,
          n.info, n.typ): transform(c, n[1])
    of tyNil:
      # a ``T(nil)`` expression
      # XXX: it might be a better idea to eliminate the conversion during
      #      semantic analysis instead
      result = transform(c, n[1])
      result.typ = n.typ
    else:
      result = transformSons(c, n)
  of tyObject:
    let diff = inheritanceDiff(dest, source)
    if diff == 0 or diff == high(int):
      result = transform(c, n[1])
      result.typ = n.typ
    else:
      result = newTreeIT(
        if diff < 0: nkObjUpConv else: nkObjDownConv,
        n.info, n.typ): transform(c, n[1])
  of tyPointer:
    case source.kind
    of tyNil:
      result = transform(c, n[1])
      result.typ = n.typ
    else:
      result = transformSons(c, n)
  of tyGenericParam, tyOrdinal:
    result = transform(c, n[1])
    # happens sometimes for generated assignments, etc.
  of tyProc:
    result = transformSons(c, n)
    if dest.callConv == ccClosure and source.callConv == ccNimCall:
      result = generateThunk(c, result[1], dest)
  else:
    result = transformSons(c, n)

type
  PassBy = enum
    passByForward ## inline the argument expression at each parameter usage
    passByCopy    ## materialize a full copy (the argument being moved is also
                  ## okay)
    passByValue   ## the parameter is allowed to have a different identity than
                  ## the argument (e.g.: shallow copy), but no full copy must be
                  ## materialized
    passOpenArray
    # XXX: remove ``passOpenArray`` once ``transformConv`` no longer elides
    #      to-openArray conversions

proc putArgInto(arg, formal: PType): PassBy =
  ## Returns what to do for an argument of type `typ` passed to a parameter of
  ## type `formal`, in the context of inlining.
  let t = formal.skipTypes(abstractInst - {tySink, tyTypeDesc})
  case t.kind
  of tyTypeDesc, tyStatic:
    passByForward
  of tySink:
    # use a full copy, which can then, if possible, be turned into a
    # move, later
    passByCopy
  of tyVar, tyLent:
    # always copy views (but not the viewed location)
    passByCopy
  of tyOpenArray, tyVarargs:
    let arg = arg.skipTypes(abstractVar + tyUserTypeClasses + {tyStatic})
    if arg.kind in {tyOpenArray, tyVarargs}:
      # the argument is already of ``openArray`` type -> a copy (of the view)
      # can be performed
      passByCopy
    else:
      # a temporary might be needed
      passOpenArray
  else:
    passByValue

proc parameterToLocationType(g: ModuleGraph, idgen: IdGenerator, typ: PType): PType =
  if typ.kind == tyVarargs:
    # varargs is not a valid location type; turn it into an ``openArray``
    result = newType(tyOpenArray, nextTypeId(idgen), typ.owner)
    result.rawAddSon(typ.base)
    # don't copy the flags, etc.
  else:
    result = typ

proc findWrongOwners(c: PTransf, n: PNode) =
  if n.kind == nkVarSection:
    let x = n[0][0]
    c.graph.config.internalAssert(x.kind != nkSym or x.sym.owner == getCurrOwner(c),
      x.info, "bah " & x.sym.name.s & " " & x.sym.owner.name.s & " " & getCurrOwner(c).name.s)
  else:
    for i in 0..<n.safeLen: findWrongOwners(c, n[i])

proc isSimpleIteratorVar(c: PTransf; iter: PSym): bool =
  proc rec(n: PNode; owner: PSym; dangerousYields: var int) =
    case n.kind
    of nkWithoutSons: discard
    of nkYieldStmt:
      if n[0].kind == nkSym and n[0].sym.owner == owner:
        discard "good: yield a single variable that we own"
      else:
        inc dangerousYields
    of nkWithSons - nkYieldStmt:
      for c in n: rec(c, owner, dangerousYields)

  var dangerousYields = 0
  rec(getBody(c.graph, iter), iter, dangerousYields)
  result = dangerousYields == 0

proc transformFor(c: PTransf, n: PNode): PNode =
  # generate access statements for the parameters (unless they are constant)
  # put mapping from formal parameters to actual parameters
  c.graph.config.internalAssert(n.kind == nkForStmt, n.info, "transformFor")

  var call = n[^2]

  let labl = newLabel(c, n)
  result = newNodeI(nkBlockStmt, n.info, 2)
  result[0] = newSymNode(labl)
  if call.typ.isNil:
    # see bug #3051
    result[1] = newNode(nkEmpty)
  elif call.kind notin nkCallKinds or call[0].kind != nkSym or
      call[0].typ.skipTypes(abstractInst).callConv == ccClosure:
    c.breakSyms.add(labl)
    result[1] = n
    result[1][^1] = transformLoopBody(c, n[^1])
    result[1][^2] = transform(c, n[^2])
    result[1] = lambdalifting.liftForLoop(c.graph, result[1], c.idgen,
                                          getCurrOwner(c), labl)
    discard c.breakSyms.pop
  else:
    var stmtList = newNodeI(nkStmtList, n.info)
    result[1] = stmtList

    c.breakSyms.add(labl)
    let loopBody = transformLoopBody(c, n[^1])
    discard c.breakSyms.pop

    let iter = call[0].sym

    if isSimpleIteratorVar(c, iter):
      # the iterator only yields locations that it owns -> the for-vars can be cursors
      # XXX: maybe leave this to cursor inference instead?
      for it in forLoopDefs(n):
        case it.kind
        of nkSym:     incl it.sym.flags, sfCursor
        of nkDotExpr: discard "lifted into environment; ignore"
        else:         unreachable()

    # this can fail for 'nimsuggest' and 'check':
    if iter.kind != skIterator: return result

    # Bugfix: inlined locals belong to the invoking routine, not to the invoked
    # iterator!
    var newC = PTransCon(
      owner: getCurrOwner(c),
      mapping: newIdNodeTable(),
      forStmt: n,
      forLoopBody: loopBody)
    pushTransCon(c, newC)
    # generate access expressions for the parameters:
    for i in 1..<call.len:
      let
        arg = transform(c, call[i])
        formal = skipTypes(iter.typ, abstractInst).n[i].sym

      proc addLocal(c: PTransf, typ: PType, formal: PSym, value,
                    stmts: PNode): PNode {.nimcall.} =
        ## Creates a new local from `typ` and `formal`, adds statement
        ## for defining and initializing it, and returns a symbol node
        ## holding the local's symbol.
        let owner = getCurrOwner(c)
        let sym = newSym(skLet, formal.name, nextSymId(c.idgen), owner,
                         formal.info)
        sym.typ = parameterToLocationType(c.graph, c.idgen, typ)
        sym.flags.incl sfFromGeneric
        sym.flags.incl sfShadowed

        result =
          if owner.isIterator or owner.isCoroutine:
            freshVarForClosureIter(c.graph, sym, c.idgen, owner)
          else:
            newSymNode(sym)

        stmts.add newTree(nkVarSection, newIdentDefs(result, value))

      template addLocal(typ: PType, value: PNode): PNode =
        addLocal(c, typ, formal, value, stmtList)

      # XXX: two things are currently ignored here:
      #      1. callsite aliasing (`iter(x, (x = 1; 2))`)
      #      2. the for-loop body modifying a location that was passed by-name
      #         to the iterator parameter (i.e. the local corresponding to the
      #         parameter uses ``lent`` or is a cursor)
      #
      #      Triggering any of the two is likely going to result in run-time
      #      crashes or misbehaving programs.

      # don't forward simple expressions like literals; we might still need a
      # location (e.g., when the iterator takes the address of the parameter)
      let expr =
        case putArgInto(arg.typ, formal.typ)
        of passByForward: arg
        of passByCopy:    addLocal(formal.typ, arg)
        of passByValue:
          let e = flattenExpr(arg, stmtList.sons)
          # note that however we choose to pass the argument, if the source
          # is an lvalue, the source must never be moved out of
          if e.typ.kind in IntegralTypes + {tyPtr, tyPointer, tyCstring}:
            # for integral types and other simple types, a normal temporary
            # suffices
            addLocal(formal.typ, e)
          elif e.typ.kind in {tyRef, tySequence, tyString} or
               not canUseView(e):
            # something that is know to be small, or the source is not an
            # lvalue -> use a cursor
            let loc = addLocal(formal.typ, e)
            # TODO: also turn temporaries lifted into the environment into cursors.
            #       This requires the ``liftdestructors`` logic for cursor fields
            #       to be fixed first
            if loc.kind == nkSym:
              loc.sym.flags.incl sfCursor
            loc
          else:
            # a view can be used, which is generally preferred
            let
              vt  = makeVarType(newC.owner, formal.typ, c.idgen, tyLent)
              src = addLocal(vt, newTreeIT(nkHiddenAddr, arg.info, vt, [e]))
            newTreeIT(nkHiddenDeref, arg.info, formal.typ, [src])

        of passOpenArray:
          let e = flattenExpr(arg, stmtList.sons)
          if canUseView(e):
            addLocal(formal.typ, e)
          else:
            # assigning to an openArray location requires an lvalue source
            # operand, but the argument is not an lvalue expression. The
            # argument is assigned to a temporary, which we then create an
            # ``openArray`` view of
            let src = newTemp(c, arg.typ, arg.info)
            stmtList.add newTree(nkVarSection, newIdentDefs(src, arg))
            addLocal(formal.typ, src)

      # map all usages of the parameter symbol to `expr`:
      idNodeTablePut(newC.mapping, formal, expr)

    let body = transformBody(c.graph, c.idgen, iter, true)
    pushInfoContext(c.graph.config, n.info)
    inc(c.inlining)
    stmtList.add(transform(c, body))
    #findWrongOwners(c, stmtList.PNode)
    dec(c.inlining)
    popInfoContext(c.graph.config)
    popTransCon(c)

proc transformCase(c: PTransf, n: PNode): PNode =
  # removes `elif` branches of a case stmt
  # adds ``else: nil`` if needed for the code generator
  # also drops ``of`` branches without labels
  result = newNodeIT(nkCaseStmt, n.info, n.typ)
  var ifs: PNode = nil
  for it in n:
    case it.kind
    of nkElifBranch:
      let e = transform(c, it)
      if ifs == nil:
        # Generate the right node depending on whether `n` is used as a stmt or
        # as an expr
        let kind = if n.typ != nil: nkIfExpr else: nkIfStmt
        ifs = newNodeIT(kind, it.info, n.typ)
      ifs.add(e)
    of nkElse:
      let e = transform(c, it)
      if ifs == nil: result.add(e)
      else: ifs.add(e)
    of nkOfBranch:
      # drop the branch if it has no labels. This is the case for,
      # e.g.: `of []: discard`
      if it.len > 1:
        result.add(transform(c, it))
    else:
      # this must be the selector expression
      result.add(transform(c, it))
  if ifs != nil:
    var elseBranch = newTreeI(nkElse, n.info): ifs
    result.add(elseBranch)
  elif result.lastSon.kind != nkElse and not (
      skipTypes(n[0].typ, abstractVarRange).kind in
        {tyInt..tyInt64, tyChar, tyEnum, tyUInt..tyUInt64}):
    # fix a stupid code gen bug by normalizing:
    let elseBranch = newTreeI(nkElse, n.info): newNodeI(nkNilLit, n.info)
    result.add(elseBranch)

  if result.len == 2 and result[1].kind == nkElse:
    # the case statement has no 'of' branch. Transform it into a
    # ``(discard sel; block: body)``. Why the block and discard?
    # * the discard makes sure side-effects of the selector expression are
    #   computed and that usage of `sel` stays
    # * the block makes sure that lifetimes don't change
    let
      discardStmt = newTreeI(nkDiscardStmt, n.info, result[0])
      body = result[1][^1]
      label = newSymNode(newLabel(c, body))
    result =
      if isEmptyType(result.typ):
        newTreeI(nkStmtList, n.info):
          [discardStmt,
           newTreeI(nkBlockStmt, body.info, label, body)]
      else:
        newTreeIT(nkStmtListExpr, n.info, n.typ):
          [discardStmt,
           newTreeIT(nkBlockExpr, body.info, body.typ, label, body)]

proc transformArrayAccess(c: PTransf, n: PNode): PNode =
  # XXX this is really bad; transf should use a proper AST visitor
  if n[0].kind == nkSym and n[0].sym.kind == skType:
    result = n
  else:
    result = shallowCopy(n)
    for i in 0..<n.len:
      result[i] = transform(c, skipConv(n[i]))

proc transformExpandToAst(c: PTransf, n: PNode): PNode =
  ## Transforms a ``getAst`` call expression to a representation that's easier
  ## to process by ``mirgen``. The argument expression is expanded into the
  ## argument list of ``getAst``, like so:
  ##
  ## .. code-block:: nim
  ##
  ##   getAst(templ(a, b))
  ##   # gets transformed to:
  ##   getAst(templ, a, b)
  ##
  assert n.kind in nkCallKinds
  assert n.len == 2

  # XXX: ``genAst(templ(x))`` (where `templ` is a template) could be
  #      transformed into:
  #
  #        mExpandToAst( NimNodeLit(Sym "templ"), Call( Sym "toNimNode", Sym "x") )
  #
  #      during semantic analysis. The above expression is semantically
  #      equivalent to what currently happens, but it would move decision
  #      making out of both ``mirgen``, ``vmgen``, and the VM. In addition, the
  #      semantics become encoded in the AST. ``toNimNode`` would be a magic
  #      procedure that does the same thing as ``opcDataToAst`` (i.e. turn a
  #      run-time value into its ``NimNode`` representation)

  let
    call = n[1]
    nimNodeTyp = sysTypeFromName(c.graph, n.info, "NimNode")

  result = copyNode(n)
  result.sons.setLen(1 + call.len)
  result[0] = n[0]    # the ``getAst`` symbol
  result[1] = call[0] # the callee symbol

  for i in 1..<call.len:
    let it = call[i]

    if it.kind == nkSym and it.sym.kind in {skMacro, skTemplate}:
      # special case them here so that the following processing doesn't have
      # to -- these are AST values, *not* normal symbols
      result[i + 1] = newTreeIT(nkNimNodeLit, it.info, nimNodeTyp): it
    elif it.typ.skipTypes(abstractInst - {tyTypeDesc}).kind == tyTypeDesc:
      # the expression is a type expression, don't attempt to transform it.
      # ``mirgen`` will fold it into a type literal
      result[i + 1] = it
    else:
      result[i + 1] = transform(c, it)

proc getMergeOp(n: PNode): PSym =
  case n.kind
  of nkCall, nkHiddenCallConv, nkCommand, nkInfix, nkPrefix, nkPostfix,
     nkCallStrLit:
    if n[0].kind == nkSym and n[0].sym.magic == mConStrStr:
      result = n[0].sym
  else: discard

proc flattenTreeAux(d, a: PNode, op: PSym) =
  let op2 = getMergeOp(a)
  if op2 != nil and
      (op2.id == op.id or op.magic != mNone and op2.magic == op.magic):
    for i in 1..<a.len: flattenTreeAux(d, a[i], op)
  else:
    d.add copyTree(a)

proc flattenTree(root: PNode): PNode =
  let op = getMergeOp(root)
  if op != nil:
    result = copyNode(root)
    result.add copyTree(root[0])
    flattenTreeAux(result, root, op)
  else:
    result = root

proc transformCall(c: PTransf, n: PNode): PNode =
  var n = flattenTree(n)
  let op = getMergeOp(n)
  let magic = getMagic(n)
  if op != nil and op.magic != mNone and n.len >= 3:
    result = newNodeIT(nkCall, n.info, n.typ)
    result.add(transform(c, n[0]))
    var j = 1
    while j < n.len:
      var a = transform(c, n[j])
      inc(j)
      if isConstExpr(a):
        while (j < n.len):
          let b = transform(c, n[j])
          if not isConstExpr(b): break
          a = evalOp(op.magic, n, a, b, nil, c.idgen, c.graph)
          inc(j)
      result.add(a)
    if result.len == 2: result = result[1]
  elif magic == mAddr:
    result = newTreeIT(nkAddr, n.info, n.typ): n[1]
    result = transformAddr(c, result)
  elif magic == mTypeOf:
    result = n
  elif magic == mRunnableExamples:
    # discard runnable-example blocks that reach here
    result = c.graph.emptyNode
  elif magic == mProcCall:
    # but do not change to its dispatcher:
    result = transformSons(c, n[1])
  elif magic == mStrToStr:
    result = transform(c, n[1])
  elif magic == mExpandToAst:
    result = transformExpandToAst(c, n)
  elif magic == mSuspend:
    let x = transformSons(c, n)
    result = newNodeI(nkYieldStmt, n.info)
    if x.len > 1:
      result.add x[1]
    else:
      let typ = c.graph.getCompilerProc("CoroutineBase").typ
      result.add newNodeIT(nkNilLit, n.info, typ)
  else:
    let s = transformSons(c, n)
    # bugfix: check after 'transformSons' if it's still a method call:
    # use the dispatcher for the call:
    if s[0].kind == nkSym and s[0].sym.kind == skMethod:
      when false:
        let t = lastSon(s[0].sym.ast)
        if t.kind != nkSym or sfDispatcher notin t.sym.flags:
          methodDef(s[0].sym, false)
      result = methodCall(s, c.graph.config)
    else:
      result = s

proc transformExceptBranch(c: PTransf, n: PNode): PNode =
  if n[0].isInfixAs() and not isImportedException(n[0][1].typ, c.graph.config):
    # Generating `let exc = (excType)(getCurrentException())`
    # -> getCurrentException()
    let excCall = callCodegenProc(c.graph, "getCurrentException")
    # -> (excType)
    let convNode = newTreeIT(nkObjDownConv, n[1].info, n[0][2].typ):
      [excCall]
    # -> let exc = ...
    let identDefs = newTreeI(nkIdentDefs, n[1].info):
      [n[0][2], newNodeI(nkEmpty, n.info), convNode]

    let letSection = newTreeI(nkLetSection, n[1].info): identDefs
    # Place the let statement and body of the 'except' branch into new stmtList.
    let actions = newTreeIT(nkStmtListExpr, n[1].info, n[1].typ):
      [letSection, transform(c, n[1])]
    # Overwrite 'except' branch body with our stmtList.
    result = newTreeI(nkExceptBranch, n[1].info):
      # Replace the `Exception as foobar` with just `Exception`.
      [transform(c, n[0][1]), actions]
  else:
    result = transformSons(c, n)

proc commonOptimizations*(g: ModuleGraph; idgen: IdGenerator; c: PSym, n: PNode): PNode =
  result = n
  for i in 0..<n.safeLen:
    result[i] = commonOptimizations(g, idgen, c, n[i])
  var op = getMergeOp(n)
  if (op != nil) and (op.magic != mNone) and (n.len >= 3):
    result = newNodeIT(nkCall, n.info, n.typ)
    result.add(n[0])
    var args = newNode(nkArgList)
    flattenTreeAux(args, n, op)
    var j = 0
    while j < args.len:
      var a = args[j]
      inc(j)
      if isConstExpr(a):
        while j < args.len:
          let b = args[j]
          if not isConstExpr(b): break
          a = evalOp(op.magic, result, a, b, nil, idgen, g)
          inc(j)
      result.add(a)
    if result.len == 2: result = result[1]
  else:
    # XXX: consider using ``foldInAst`` at the callsite
    var cnst = getConstExpr(c, n, idgen, g)
    # we inline constants if they are not complex constants:
    if cnst != nil and not dontInlineConstant(n, cnst):
      result = cnst
    else:
      result = n

proc transform(c: PTransf, n: PNode): PNode =
  when false:
    var oldDeferAnchor: PNode
    if n.kind in {nkElifBranch, nkOfBranch, nkExceptBranch, nkElifExpr,
                  nkElseExpr, nkElse, nkForStmt, nkWhileStmt, nkFinally,
                  nkBlockStmt, nkBlockExpr}:
      oldDeferAnchor = c.deferAnchor
      c.deferAnchor = n
  case n.kind
  of nkError:
    unreachable("errors can't reach here")
  of nkSym:
    result = transformSym(c, n)
  of nkWithoutSons - {nkSym, nkError}:
    # nothing to be done for leaves:
    result = n
  of nkBracketExpr: result = transformArrayAccess(c, n)
  of procDefs:
    var s = n[namePos].sym
    if n.typ != nil and s.typ.callConv == ccClosure:
      result = transformSym(c, n[namePos])
      # use the same node as before if still a symbol:
      if result.kind == nkSym: result = n
    else:
      result = n
  of nkForStmt:
    result = transformFor(c, n)
  of nkCaseStmt:
    result = transformCase(c, n)
  of nkWhileStmt: result = transformWhile(c, n)
  of nkBlockStmt, nkBlockExpr:
    result = transformBlock(c, n)
  of nkDefer:
    c.deferDetected = true
    result = transformSons(c, n)
    when false:
      let deferPart = newTreeI(nkFinally, n.info): n[0]
      let tryStmt = newNodeI(nkTryStmt, n.info)
      if c.deferAnchor.isNil:
        tryStmt.add c.root
        c.root = tryStmt
        result = tryStmt
      else:
        # modify the corresponding *action*, don't rely on nkStmtList:
        tryStmt.add c.deferAnchor[^1]
        c.deferAnchor[^1] = tryStmt
        result = newNodeI(nkCommentStmt, n.info)
      tryStmt.add deferPart
      # disable the original 'defer' statement:
      n.kind = nkEmpty
  of nkContinueStmt:
    # transform into a break out of the loop's inner block
    result = newBreakStmt(n.info, c.contSyms[^1])
  of nkBreakStmt: result = transformBreak(c, n)
  of nkCallKinds:
    result = transformCall(c, n)
  of nkAddr:
    result = transformAddr(c, n)
  of nkDerefExpr:
    result = transformSons(c, n)
    # collapse ``nkDerefExpr( nkAddr(x) )`` to just ``x``
    if result[0].kind == nkAddr:
      result = result[0][0]
  of nkHiddenDeref:
    # collapsing a ``nkHiddenDeref( nkHiddenAddr(x) )`` to ``x`` is safe.
    # This is because:
    # - ``nkHiddenAddr`` is only used for views or in a ``var`` parameter
    #   context and it's thus known that the result is a view. For
    #   ``nkHiddenDeref``, this is not always the case: it's also emitted for
    #   implicit pointer or ref dereferencing
    # - we know that the ``nkHiddenAddr`` is not used to signal that something
    #   is an argument to a ``var`` parameter, due to it not appearing directly
    #   in an argument context
    # XXX: because of a cgen issues related to ``openArray``, this is not only
    #      an optimization, but a requirement, as invalid C code would be
    #      generated without the collapsing
    result = transformSons(c, n)
    if result[0].kind == nkHiddenAddr:
      result = result[0][0]
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    result = transformConv(c, n)
  of nkDiscardStmt:
    result = n
    if n[0].kind != nkEmpty:
      result = transformSons(c, n)
      if isConstExpr(result[0]):
        # ensure that e.g. discard "some comment" gets optimized away
        # completely:
        result = newNode(nkCommentStmt)
  of nkTemplateDef, nkImportStmt, nkStaticStmt,
      nkExportStmt, nkExportExceptStmt:
    return n
  of nkConstSection:
    # do not replace ``const c = 3`` with ``const 3 = 3``
    return transformConstSection(c, n)
  of nkTypeSection, nkTypeOfExpr, nkMixinStmt, nkBindStmt:
    # no need to transform type sections:
    return n
  of nkLetSection, nkVarSection:
    if c.inlining > 0:
      # we need to copy the variables for multiple yield statements:
      result = transformVarSection(c, n)
    else:
      result = transformSons(c, n)
  of nkYieldStmt:
    if c.inlining > 0:
      result = transformYield(c, n)
    else:
      result = transformSons(c, n)
  of nkIdentDefs, nkConstDef:
    result = shallowCopy(n)
    result[0] = transformDefSym(c, n[0])
    # Skip the second son since it only contains an unsemanticized copy of the
    # variable type used by docgen
    let last = n.len-1
    for i in 1..<last: result[i] = n[i]
    result[last] = transform(c, n[last])
    # XXX comment handling really sucks:
    if importantComments(c.graph.config):
      result.comment = n.comment
  of nkVarTuple:
    result = shallowCopy(n)
    for i in 0..<n.len-2:
      result[i] = transformDefSym(c, n[i])

    result[^2] = transform(c, n[^2])
    result[^1] = transform(c, n[^1])
  of nkClosure:
    # it can happen that for-loop-inlining produced a fresh
    # set of variables, including some computed environment
    # (bug #2604). We need to patch this environment here too:
    let a = n[1]
    if a.kind == nkSym:
      result = copyTree(n)
      result[1] = transformSymAux(c, a)
    else:
      result = n

    if n[0].kind == nkSym and n[0].sym.isCoroutine:
      # the coroutine needs to be transformed eagerly, so that its signature
      # is correct before first reaching code generation
      discard transformBody(c.graph, c.idgen, n[0].sym, true)
  of nkOfBranch:
    result = shallowCopy(n)
    # don't transform the label nodes:
    for i in 0..<n.len-1:
      result[i] = n[i]

    result[^1] = transform(c, n[^1])
  of nkExceptBranch:
    result = transformExceptBranch(c, n)
  of nkNimNodeLit:
    # do not transform the content of a ``NimNode`` literal
    result = n
  of nkStmtListExpr:
    if stupidStmtListExpr(n):
      # the statement-list expression is redundant (i.e. only has a single
      # item or only leading empty nodes) -> skip it
      result = transform(c, n.lastSon)
    else:
      # needs to be kept
      result = transformSons(c, n)
  of nkPragmaExpr:
    # not needed in transformed AST -> drop it
    result = transform(c, n.lastSon)
  else:
    result = transformSons(c, n)
  when false:
    if oldDeferAnchor != nil: c.deferAnchor = oldDeferAnchor

proc processTransf(c: PTransf, n: PNode, owner: PSym): PNode =
  # Note: For interactive mode we cannot call 'passes.skipCodegen' and skip
  # this step! We have to rely that the semantic pass transforms too errornous
  # nodes into an empty node.
  if nfTransf in n.flags: return n
  pushTransCon(c, PTransCon(owner: owner, mapping: newIdNodeTable()))
  result = transform(c, n)
  popTransCon(c)
  incl(result.flags, nfTransf)

proc flattenStmts(n: PNode) =
  var goOn = true
  while goOn:
    goOn = false
    var i = 0
    while i < n.len:
      let it = n[i]
      if it.kind in {nkStmtList, nkStmtListExpr}:
        n.sons[i..i] = it.sons[0..<it.len]
        goOn = true
      inc i

proc liftDeferAux(n: PNode) =
  if n.kind in {nkStmtList, nkStmtListExpr}:
    flattenStmts(n)
    var goOn = true
    while goOn:
      goOn = false
      let last = n.len-1
      for i in 0..last:
        if n[i].kind == nkDefer:
          let deferPart = newTreeI(nkFinally, n[i].info): n[i][0]
          var body = newNodeIT(n.kind, n[i].info, n.typ)
          if i < last:
            body.sons = n.sons[(i+1)..last]
          let tryStmt = newTreeIT(nkTryStmt, n[i].info, n.typ):
            [body, deferPart]
          n[i] = tryStmt
          n.sons.setLen(i+1)
          n.typ = tryStmt.typ
          goOn = true
          break
  for i in 0..n.safeLen-1:
    liftDeferAux(n[i])

template liftDefer(c, root) =
  if c.deferDetected:
    liftDeferAux(root)

proc transformBody*(g: ModuleGraph, idgen: IdGenerator, prc: PSym, body: PNode): PNode =
  ## Applies the various transformations to `body` and returns the result.
  ## This step is not indempotent, and since no caching is performed, it
  ## must not be performed more than once for a routine and its body.
  ##
  ## The transformations are:
  ## 1. the ``lambdalifting`` transformation
  ## 2. general lowerings -- these are the ones implemented here in
  ##    ``transf``
  ## 3. the ``closureiters`` transformation
  ##
  ## Application always happens in that exact order.
  var c = PTransf(graph: g, module: prc.getModule, idgen: idgen)
  if prc.isCoroutineConstr:
    result = preTransformConstr(g, idgen, prc, body)
  else:
    result = body

  (result, c.env) = liftLambdas(g, prc, result, c.idgen)
  result = processTransf(c, result, prc)
  liftDefer(c, result)

  if prc.isIterator:
    result = g.transformClosureIterator(c.idgen, prc, result)
    # the environment type is closed for modification, meaning that we can
    # safely create the type-bound operators now
    finishClosureIterator(c.graph, c.idgen, prc)
  elif prc.isCoroutineConstr:
    result = g.transformCoroutineConstr(c.idgen, prc, result)
  elif prc.isCoroutine:
    result = g.transformCoroutine(c.idgen, prc, result)

  incl(result.flags, nfTransf)

proc transformBody*(g: ModuleGraph; idgen: IdGenerator; prc: PSym; cache: bool): PNode =
  assert prc.kind in routineKinds

  result = g.getTransformed(prc)
  if result != nil:
    discard "already transformed"
  elif nfTransf in getBody(g, prc).flags or prc.kind in {skTemplate}:
    result = getBody(g, prc)
  else:
    g.setTransformed(prc, g.emptyNode) # protects from recursion
    result = transformBody(g, idgen, prc, getBody(g, prc))

    g.setTransformed(prc):
      if cache: result
      else:     nil

proc transformBodyWithCache*(g: ModuleGraph, idgen: IdGenerator, prc: PSym): PNode =
  ## Fetches the cached transformed body of `prc`, transforming it if not
  ## available, new transforms are not cached
  assert prc.kind in routineKinds - {skTemplate, skMacro}
  result = g.getTransformed(prc)
  if result != nil:
    discard "already transformed"
  elif nfTransf in getBody(g, prc).flags:
    # the AST was already transformed:
    result = getBody(g, prc)
  else:
    # no recursion is possible here, so no guard is needed
    result = transformBody(g, idgen, prc, getBody(g, prc))

proc transformStmt*(g: ModuleGraph; idgen: IdGenerator; module: PSym, n: PNode): PNode =
  if nfTransf in n.flags:
    result = n
  else:
    var c = PTransf(graph: g, module: module, idgen: idgen)
    result = processTransf(c, n, module)
    liftDefer(c, result)
    #result = liftLambdasForTopLevel(module, result)
    incl(result.flags, nfTransf)

proc transformExpr*(g: ModuleGraph; idgen: IdGenerator; module: PSym, n: PNode): PNode =
  if nfTransf in n.flags:
    result = n
  else:
    var c = PTransf(graph: g, module: module, idgen: idgen)
    result = processTransf(c, n, module)
    liftDefer(c, result)
    # expressions are not to be injected with destructor calls as that
    # the list of top level statements needs to be collected before.
    incl(result.flags, nfTransf)

proc extractGlobals*(body: PNode, output: var seq[PNode], isNimVm: bool) =
  ## Searches for all ``nkIdentDefs`` defining a global that's not owned by a
  ## module, appends them to `output` in the order they appear in the input
  ## AST, and removes the nodes from `body`. `isNimVm` signals which branch
  ## to select for ``when nimvm`` statements/expressions.
  ##
  ## XXX: this can't happen as part of ``transformBody``, as ``transformBody``
  ##      is reentrant because of ``lambdalifting`` and it's thus not easily
  ##      possible to collect something from the body of a single procedure
  ##      only. There's also the problem that extracting the globals is not
  ##      wanted when transformation happens for a procedure that's invoked
  ##      during CTFE and used in normal code. Eventually, ``transformBody``
  ##      will no longer use the current caching mechanism and only produce the
  ##      transformed version of the input AST, but until then,
  ##      ``collectGlobals`` works good enough
  case body.kind
  of nkTypeSection, nkTypeOfExpr, nkCommentStmt, nkIncludeStmt, nkImportStmt,
     nkImportExceptStmt, nkExportStmt, nkExportExceptStmt, nkFromStmt,
     nkStaticStmt, nkMixinStmt, nkBindStmt, nkLambdaKinds, routineDefs,
     nkNimNodeLit:
    discard "ignore declarative contexts"
  of nkWithoutSons - nkCommentStmt:
    discard "not relevant"
  of nkConv, nkHiddenStdConv, nkHiddenSubConv:
    # only analyse the imperative part:
    extractGlobals(body[1], output, isNimVm)
  of nkWhen:
    # a ``when nimvm`` construct
    # XXX: this logic duplicates what ``mirgen`` already does. Maybe
    #      collecting should happen there? Or should procedure-level globals
    #      be lifted from procedures during semantic analysis already?
    let branch =
      if isNimVm: body[0][1]
      else:       body[1][0]

    extractGlobals(branch, output, isNimVm)
  of nkVarSection, nkLetSection:
    # iterate over all children and extract identdefs of globals:
    var i = 0
    while i < body.len:
      let it = body[i]
      if it.kind == nkIdentDefs and
         it[0].kind == nkSym and
         sfGlobal in it[0].sym.flags and
         it[0].sym.owner.kind in routineKinds:
        # found one; append it to the output:
        output.add(it)
        # there's no need to process the initializer expression of the global,
        # as we know that further globals defined inside them are not visible
        # to the outside
        body.sons.delete(i)
      else:
        inc i

  else:
    # search all child nodes:
    for it in body.items:
      extractGlobals(it, output, isNimVm)
