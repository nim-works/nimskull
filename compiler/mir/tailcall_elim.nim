## Implements the MIR passes making up the portable tail-call elimination (a
## high-level description of how this works can be found `here <tailcallelim.html>`_).

import
  std/private/[
    containers
  ],
  compiler/ast/[
    ast_types,
    ast_query,
    idents,
    types
  ],
  compiler/mir/[
    mirbodies,
    mirchangesets,
    mirconstr,
    mirenv,
    mirtrees,
    mirtypes
  ],
  compiler/modules/[
    modulegraphs
  ]

template subTree(bu: var MirBuilder, k: MirNodeKind, t: TypeId,
                 body: untyped) =
  bu.subTree MirNode(kind: k, typ: t):
    body

proc getCalleeType(tree: MirTree, pos: NodePosition, env: MirEnv): PType =
  if tree[pos].kind == mnkProc:
    env[tree[pos].prc].typ
  else:
    env.types[env.types.canonical(tree[pos].typ)]

proc emitParamBlobInit(bu: var MirBuilder, tree: MirTree, call: NodePosition,
                       to: Value, g: ModuleGraph, owner: PSym,
                       env: var MirEnv) =
  ## Emits the parameter blob initialization, using the arguments from the
  ## call at `call`. `to` provides the destination.
  if tree.numArgs(call) == 0:
    # nothing to store
    return

  let fntype = getCalleeType(tree, tree.callee(call), env)
  # creating a parameter tuple using the graph's IdGenerator yields types
  # with degenerate IDs. Let's hope these types don't end up anywhere
  # problematic...
  let typ = env.types.add(newParamTuple(g.config, g.idgen, owner, fntype))
  var tup = bu.allocTemp(typ)
  bu.buildStmt mnkDef:
    bu.use tup
    bu.subTree mnkTupleConstr, typ:
      var i = 0'i32
      for (mode, _, arg) in tree.arguments(call):
        case mode
        of mnkArg:
          bu.subTree mnkArg:
            bu.emitFrom(tree, NodePosition arg)
        of mnkConsume:
          bu.subTree mnkConsume:
            bu.emitFrom(tree, NodePosition arg)
        of mnkName:
          let pt = env.types[env.types.lookupField(typ, i)].typ
          bu.subTree mnkConsume:
            var tmp: Value
            bu.withFront:
              tmp = bu.wrapTemp pt:
                bu.subTree mnkAddr, pt:
                  bu.emitFrom(tree, NodePosition arg)
            bu.use tmp
        inc i

  bu.subTree mnkVoid:
    bu.buildMagicCall mStoreParams, VoidType:
      bu.emitByVal to
      bu.subTree mnkConsume:
        bu.use tup

proc insertNewResult*(body: var MirBody, g: ModuleGraph, owner: PSym,
                      env: var MirEnv) =
  ## First part of the .tailcall lowering. Changes the 'result' type to
  ## a ``Continuation`` type and turns the previous 'result' variable
  ## (if any) into a normal local.
  let contType = env.types.add(owner.typ.n[0][effectListLen].typ)
  if body[resultId].typ == VoidType:
    body.locals[resultId] =
      Local(name: g.cache.getIdent("result"), typ: contType)
  else:
    var res = body[resultId]
    let newId = body.locals.add(res)
    # patch the result type:
    res.typ = contType
    body.locals[resultId] = res

    # patch the result variable usages in the body:
    for n in body.code.mitems:
      if n.kind == mnkLocal and n.local == resultId:
        n.local = newId
    # note: this pass leaves the body in a state where invariants for
    # return statements don't hold. This is not a problem, however, as
    # the second pass, which has to be run immediately afterwards, makes
    # them hold again

    # wrap the body in a scope containing the 'def' for the new local:
    var c = initChangeset(body)
    c.insert(body.code, NodePosition(0), NodePosition(0), bu):
      bu.subTree mnkScope: discard
      bu.subTree mnkDef:
        bu.use toValue(mnkLocal, newId, body[newId].typ)
        bu.add MirNode(kind: mnkNone)
    c.insert(body.code, NodePosition(body.code.len), NodePosition(0), bu):
      bu.subTree mnkEndScope: discard
    body.apply(c)

proc lowerTailcallBody*(body: MirBody, g: ModuleGraph, owner: PSym,
                        env: var MirEnv, changes: var Changeset) =
  ## Second part of the .tailcall lowering. Lowers all 'tailcall' statements
  ## into constructing and returning a ``Continuation`` object.
  template tree: MirTree = body.code
  let
    contType = env.types.add(owner.typ.n[0][effectListLen].typ)
    nextType = env.types[env.types.lookupField(contType, 1)].typ

  var pos = NodePosition(0)
  while pos < NodePosition(tree.len):
    if tree[pos].kind == mnkVoid and tree[pos, 0].kind == mnkTailCall:
      let
        call = tree.child(pos, 0)
        callee = tree.callee(call)
      changes.replaceMulti(tree, pos, bu):
        bu.buildStmt mnkInit:
          bu.use toValue(mnkLocal, resultId, contType)
          bu.subTree mnkObjConstr, contType:
            bu.subTree mnkBinding:
              bu.add MirNode(kind: mnkField, field: 0)
              bu.subTree mnkConsume:
                bu.use literal(mnkUIntLit, env.getOrIncl(0), BoolType)
            bu.subTree mnkBinding:
              bu.add MirNode(kind: mnkField, field: 1)
              bu.subTree mnkConsume:
                if tree[callee].kind == mnkProc:
                  let prc = env[tree[callee].prc].ast[miscPos][0].sym
                  bu.use toValue(env.procedures.add(prc), env.types.add(prc.typ))
                else:
                  # the static type doesn't match the dyanmic type; cast it
                  var tmp: Value
                  bu.withFront:
                    tmp = bu.wrapTemp nextType:
                      bu.subTree mnkCast, nextType:
                        bu.emitFrom(tree, callee)
                  bu.use tmp

        # set up and store the parameter container
        let dest = toValue(mnkLocal, LocalId(owner.typ.len), PointerType)
        emitParamBlobInit(bu, tree, call, dest, g, owner, env)
        bu.subTree mnkReturn:
          bu.add MirNode(kind: mnkLocal, local: resultId, typ: contType)

      # drop the following return statement:
      pos = tree.sibling(pos)
      changes.remove(tree, pos)
    elif tree[pos].kind == mnkReturn:
      # replace with a Continuation construction + return
      changes.replaceMulti(tree, pos, bu):
        bu.subTree mnkInit:
          bu.use toValue(mnkLocal, resultId, contType)
          bu.subTree mnkObjConstr, contType:
            bu.subTree mnkBinding:
              bu.add MirNode(kind: mnkField, field: 0)
              bu.subTree mnkConsume:
                bu.use literal(mnkUIntLit, env.getOrIncl(1), BoolType)
            if tree[pos].len == 1:
              bu.subTree mnkBinding:
                bu.add MirNode(kind: mnkField, field: 2)
                bu.subTree mnkConsume:
                  bu.add tree[pos, 0]
        bu.subTree mnkReturn:
          bu.use toValue(mnkLocal, resultId, contType)

    pos = tree.sibling(pos)

proc lowerProcvals*(tree: MirTree, env: var MirEnv, changes: var Changeset) =
  ## Turns all procval creation for .tailcall procedures into procval
  ## creation of the associated application procedure.
  for pos, it in tree.pairs:
    if it.kind == mnkProcVal:
      let canon = env.types.canonical(it.typ)
      if env.types.headerFor(canon, Canonical).callConv(env.types) ==
          ccTailcall:
        # taking the address of a .tailcall procedure yields the address of
        # the application procedure
        let np = env[it.prc].ast[miscPos][0].sym
        changes.replace(tree, pos):
          MirNode(kind: mnkProcVal, prc: env.procedures.add(np),
                  typ: env.types.add(np.typ))

proc insertTrampolines*(tree: MirTree, g: ModuleGraph, owner: PSym,
                        env: var MirEnv, changes: var Changeset) =
  ## Turns all non-tailcall invocations of ``.tailcall`` procedures into
  ## trampolines.
  for pos, n in tree.pairs:
    case n.kind
    of mnkCall, mnkCheckedCall:
      let fntype = getCalleeType(tree, tree.callee(pos), env)
      if fntype.callConv == ccTailcall:
        let
          contType = env.types.add(fntype.n[0][effectListLen].typ)
          nextType = env.types[env.types.lookupField(contType, 1)].typ
          mutatesGlobal = tree.mutatesGlobal(pos)

        var cont: Value
        changes.insert(tree, tree.parent(pos), pos, bu):
          cont = bu.allocTemp(contType)
          let
            blobType = g.systemModuleType(g.cache.getIdent("ParamBlob"))
            storage = bu.allocTemp(env.types.add(blobType))
            addrTmp = bu.allocTemp(PointerType)
          bu.subTree mnkDef:
            bu.use storage
            bu.add MirNode(kind: mnkNone)
          bu.subTree mnkDef:
            bu.use addrTmp
            bu.subTree mnkAddr, PointerType:
              bu.use storage
          if tree[tree.callee(pos)].kind == mnkProc:
            # a static call
            bu.subTree mnkDef:
              bu.use cont
              bu.subTree n.kind, contType:
                # insert the the paramtere blob address as a new parameter
                if n.kind == mnkCheckedCall:
                  for i in 0..<(n.len - 1):
                    bu.emitFrom(tree, tree.child(pos, i))
                  bu.emitByVal(addrTmp)
                  bu.add tree[tree.last(pos)] # add the resumption target
                else:
                  for i in 0..<(n.len - 1):
                    bu.emitFrom(tree, tree.child(pos, i))
                  bu.emitByVal(addrTmp)
          else:
            # a dynamic call. The address points to the application procedure,
            # not the .tailcall procedure, and thus, the parameters need to be
            # passed via the blob
            emitParamBlobInit(bu, tree, pos, addrTmp, g, owner, env)

            # the address has wrong type, it needs to be cast first
            let tmp = bu.wrapTemp nextType:
              bu.subTree mnkCast, nextType:
                bu.emitFrom(tree, tree.callee(pos))
            bu.subTree mnkDef:
              bu.use cont
              bu.rawBuildCall n.kind, contType, mutatesGlobal:
                bu.use tmp
                bu.emitByVal(addrTmp)
                if n.kind == mnkCheckedCall:
                  bu.add tree[tree.last(pos)]

          let
            loop = bu.allocLabel()
            exit = bu.allocLabel()
          bu.subTree mnkLoopJoin:
            bu.add MirNode(kind: mnkLabel, label: loop)
          bu.buildIf:
            bu.pathNamed BoolType, 0:
              bu.use cont
          do:
            bu.goto(exit)

          bu.subTree mnkAsgn:
            bu.use cont
            bu.rawBuildCall n.kind, contType, mutatesGlobal:
              bu.pathNamed nextType, 1:
                bu.pathVariant contType, 0:
                  bu.use cont
              bu.emitByVal addrTmp
              if n.kind == mnkCheckedCall:
                bu.add tree[tree.last(pos)]
          bu.subTree mnkLoop:
            bu.add MirNode(kind: mnkLabel, label: loop)
          bu.join(exit)

        if n.typ == VoidType:
          changes.remove(tree, tree.parent(pos))
        else:
          # it's not a void call; extract the result from the
          # Continuation object
          changes.replaceMulti(tree, pos, bu):
            bu.subTree mnkMove, n.typ:
              bu.pathNamed n.typ, 2:
                bu.pathVariant contType, 0:
                  bu.use cont

    else:
      discard "nothing to do"
