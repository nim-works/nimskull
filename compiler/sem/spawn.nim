#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements threadpool's ``spawn``.
import
  compiler/ast/[
    trees,
    ast,
    types,
    idents,
    renderer,
    reports
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/sem/[
    lowerings,
    liftdestructors
  ]

proc callProc(a: PNode): PNode =
  result = newNodeI(nkCall, a.info)
  result.add a
  result.typ = a.typ[0]

# we have 4 cases to consider:
# - a void proc --> nothing to do
# - a proc returning GC'ed memory --> requires a flowVar
# - a proc returning non GC'ed memory --> pass as hidden 'var' parameter
# - not in a parallel environment --> requires a flowVar for memory safety
type
  TSpawnResult* = enum
    srVoid, srFlowVar, srByVar
  TFlowVarKind = enum
    fvInvalid ## invalid type T for 'FlowVar[T]'
    fvGC      ## FlowVar of a GC'ed type
    fvBlob    ## FlowVar of a blob type

proc spawnResult*(t: PType; inParallel: bool): TSpawnResult =
  if t.isEmptyType: srVoid
  elif inParallel and not containsGarbageCollectedRef(t): srByVar
  else: srFlowVar

proc flowVarKind(c: ConfigRef, t: PType): TFlowVarKind =
  if c.selectedGC in {gcArc, gcOrc}: fvBlob
  elif t.skipTypes(abstractInst).kind in {tyRef, tyString, tySequence}: fvGC
  elif containsGarbageCollectedRef(t): fvInvalid
  else: fvBlob

proc typeNeedsNoDeepCopy(t: PType): bool =
  var t = t.skipTypes(abstractInst)
  # for the tconvexhull example (and others) we're a bit lax here and pretend
  # seqs and strings are *by value* only and 'shallow' doesn't exist!
  if t.kind == tyString: return true
  # note that seq[T] is fine, but 'var seq[T]' is not, so we need to skip 'var'
  # for the stricter check and likewise we can skip 'seq' for a less
  # strict check:
  if t.kind in {tyVar, tyLent, tySequence}: t = t.lastSon
  result = not containsGarbageCollectedRef(t)

proc addLocalVar(g: ModuleGraph; varSection, varInit: PNode; idgen: IdGenerator; owner: PSym; typ: PType;
                 v: PNode; useShallowCopy=false): PSym =
  result = newSym(skTemp, getIdent(g.cache, genPrefix), nextSymId idgen, owner, varSection.info,
                  owner.options)
  result.typ = typ
  incl(result.flags, sfFromGeneric)

  var vpart = newNodeI(nkIdentDefs, varSection.info, 3)
  vpart[0] = newSymNode(result)
  vpart[1] = newNodeI(nkEmpty, varSection.info)
  vpart[2] = if varInit.isNil: v else: vpart[1]
  varSection.add vpart
  if varInit != nil:
    if g.config.selectedGC in {gcArc, gcOrc}:
      # inject destructors pass will do its own analysis
      varInit.add newFastMoveStmt(g, newSymNode(result), v)
    else:
      if useShallowCopy and typeNeedsNoDeepCopy(typ) or optTinyRtti in g.config.globalOptions:
        varInit.add newFastMoveStmt(g, newSymNode(result), v)
      else:
        let deepCopyCall = newNodeI(nkCall, varInit.info, 3)
        deepCopyCall[0] = newSymNode(getSysMagic(g, varSection.info, "deepCopy", mDeepCopy))
        deepCopyCall[1] = newSymNode(result)
        deepCopyCall[2] = v
        varInit.add deepCopyCall

discard """
We generate roughly this:

proc f_wrapper(thread, args) =
  barrierEnter(args.barrier)  # for parallel statement
  var a = args.a # thread transfer; deepCopy or shallowCopy or no copy
                 # depending on whether we're in a 'parallel' statement
  var b = args.b
  var fv = args.fv

  fv.owner = thread # optional
  nimArgsPassingDone() # signal parent that the work is done
  #
  args.fv.blob = f(a, b, ...)
  nimFlowVarSignal(args.fv)

  # - or -
  f(a, b, ...)
  barrierLeave(args.barrier)  # for parallel statement

stmtList:
  var scratchObj
  scratchObj.a = a
  scratchObj.b = b

  nimSpawn(f_wrapper, addr scratchObj)
  scratchObj.fv # optional

"""

proc createWrapperProc(g: ModuleGraph; f: PNode; threadParam, argsParam: PSym;
                       varSection, varInit, call, barrier, fv: PNode;
                       idgen: IdGenerator;
                       spawnKind: TSpawnResult, result: PSym) =
  var body = newNodeI(nkStmtList, f.info)
  var threadLocalBarrier: PSym
  if barrier != nil:
    var varSection2 = newNodeI(nkVarSection, barrier.info)
    threadLocalBarrier = addLocalVar(g, varSection2, nilPNode, idgen, result,
                                     barrier.typ, barrier)
    body.add varSection2
    body.add callCodegenProc(g, "barrierEnter", threadLocalBarrier.info,
      threadLocalBarrier.newSymNode)
  var threadLocalProm: PSym
  if spawnKind == srByVar:
    threadLocalProm = addLocalVar(g, varSection, nilPNode, idgen, result, fv.typ, fv)
  elif fv != nil:
    internalAssert(
      g.config,
      fv.typ.kind == tyGenericInst,
      "Expected generic inst type kind, but found " & $fv.typ.kind)

    threadLocalProm = addLocalVar(g, varSection, nilPNode, idgen, result, fv.typ, fv)
  body.add varSection
  body.add varInit
  if fv != nil and spawnKind != srByVar:
    # generate:
    #   fv.owner = threadParam
    body.add newAsgnStmt(indirectAccess(threadLocalProm.newSymNode,
      "owner", fv.info, g.cache), threadParam.newSymNode)

  body.add callCodegenProc(g, "nimArgsPassingDone", threadParam.info,
    threadParam.newSymNode)
  if spawnKind == srByVar:
    body.add newAsgnStmt(genDeref(threadLocalProm.newSymNode), call)
  elif fv != nil:
    let fk = flowVarKind(g.config, fv.typ[1])
    if fk == fvInvalid:
      localReport(g.config, f.info, reportTyp(
        rsemCannotCreateFlowVarOfType, fv.typ[1]))

    body.add newAsgnStmt(indirectAccess(threadLocalProm.newSymNode,
      if fk == fvGC: "data" else: "blob", fv.info, g.cache), call)
    if fk == fvGC:
      let incRefCall = newNodeI(nkCall, fv.info, 2)
      incRefCall[0] = newSymNode(getSysMagic(g, fv.info, "GCref", mGCref))
      incRefCall[1] = indirectAccess(threadLocalProm.newSymNode,
                                          "data", fv.info, g.cache)
      body.add incRefCall
    if barrier == nil:
      # by now 'fv' is shared and thus might have beeen overwritten! we need
      # to use the thread-local view instead:
      body.add callCodegenProc(g, "nimFlowVarSignal", threadLocalProm.info,
        threadLocalProm.newSymNode)
  else:
    body.add call
  if barrier != nil:
    body.add callCodegenProc(g, "barrierLeave", threadLocalBarrier.info,
      threadLocalBarrier.newSymNode)

  var params = newNodeI(nkFormalParams, f.info)
  params.add newNodeI(nkEmpty, f.info)
  params.add threadParam.newSymNode
  params.add argsParam.newSymNode

  var t = newType(tyProc, nextTypeId idgen, threadParam.owner)
  t.rawAddSon nil
  t.rawAddSon threadParam.typ
  t.rawAddSon argsParam.typ
  t.n = newNodeI(nkFormalParams, f.info)
  t.n.add newNodeI(nkEffectList, f.info)
  t.n.add threadParam.newSymNode
  t.n.add argsParam.newSymNode

  let emptyNode = newNodeI(nkEmpty, f.info)
  result.ast = newProcNode(nkProcDef, f.info, body = body,
      params = params, name = newSymNode(result), pattern = emptyNode,
      genericParams = emptyNode, pragmas = emptyNode,
      exceptions = emptyNode)
  result.typ = t

proc createCastExpr(argsParam: PSym; objType: PType; idgen: IdGenerator): PNode =
  result = newNodeI(nkCast, argsParam.info)
  result.add newNodeI(nkEmpty, argsParam.info)
  result.add newSymNode(argsParam)
  result.typ = newType(tyPtr, nextTypeId idgen, objType.owner)
  result.typ.rawAddSon(objType)

template checkMagicProcs(g: ModuleGraph, n: PNode, formal: PNode) =
  if (formal.typ.kind == tyVarargs and formal.typ[0].kind in {tyTyped, tyUntyped}) or
          formal.typ.kind in {tyTyped, tyUntyped}:
    localReport(g.config, n.info, reportAst(
      rsemCannotSpawnMagicProc, n))

proc setupArgsForConcurrency(g: ModuleGraph; n: PNode; objType: PType;
                             idgen: IdGenerator; owner: PSym; scratchObj: PSym,
                             castExpr, call,
                             varSection, varInit, result: PNode) =
  let formals = n[0].typ.n
  let tmpName = getIdent(g.cache, genPrefix)
  for i in 1..<n.len:
    # we pick n's type here, which hopefully is 'tyArray' and not
    # 'tyOpenArray':
    var argType = n[i].typ.skipTypes(abstractInst)
    if i < formals.len:
      if formals[i].typ.kind in {tyVar, tyLent}:
        localReport(g.config, n[i].info, reportAst(
          rsemCannotSpawnProcWithVar, formals[i]))

      checkMagicProcs(g, n[i], formals[i])

      if formals[i].typ.kind in {tyTypeDesc, tyStatic}:
        continue
    #elif containsTyRef(argType):
    #  localReport(n[i].info, "'spawn'ed function cannot refer to 'ref'/closure")

    let fieldname = if i < formals.len: formals[i].sym.name else: tmpName
    var field = newSym(skField, fieldname, nextSymId idgen, objType.owner, n.info, g.config.options)
    field.typ = argType
    objType.addField(field, g.cache, idgen)
    result.add newFastAsgnStmt(newDotExpr(scratchObj, field), n[i])

    let temp = addLocalVar(g, varSection, varInit, idgen, owner, argType,
                           indirectAccess(castExpr, field, n.info))
    call.add(newSymNode(temp))

proc setupArgsForParallelism(g: ModuleGraph; n: PNode; objType: PType;
                             idgen: IdGenerator;
                             owner: PSym; scratchObj: PSym;
                             castExpr, call,
                             varSection, varInit, result: PNode) =
  let formals = n[0].typ.n
  let tmpName = getIdent(g.cache, genPrefix)
  # we need to copy the foreign scratch object fields into local variables
  # for correctness: These are called 'threadLocal' here.
  for i in 1..<n.len:
    let n = n[i]
    if i < formals.len and formals[i].typ.kind in {tyStatic, tyTypeDesc}:
      continue

    checkMagicProcs(g, n, formals[i])

    let argType = skipTypes(if i < formals.len: formals[i].typ else: n.typ,
                            abstractInst)
    #if containsTyRef(argType):
    #  localReport(n.info, "'spawn'ed function cannot refer to 'ref'/closure")

    let fieldname = if i < formals.len: formals[i].sym.name else: tmpName
    var field = newSym(skField, fieldname, nextSymId idgen, objType.owner, n.info, g.config.options)

    if argType.kind in {tyVarargs, tyOpenArray}:
      # important special case: we always create a zero-copy slice:
      let slice = newNodeI(nkCall, n.info, 4)
      slice.typ = n.typ
      slice[0] = newSymNode(createMagic(g, idgen, "slice", mSlice))
      slice[0].typ = getSysType(g, n.info, tyInt) # fake type
      var fieldB = newSym(skField, tmpName, nextSymId idgen, objType.owner, n.info, g.config.options)
      fieldB.typ = getSysType(g, n.info, tyInt)
      objType.addField(fieldB, g.cache, idgen)

      if getMagic(n) == mSlice:
        let a = genAddrOf(n[1], idgen)
        field.typ = a.typ
        objType.addField(field, g.cache, idgen)
        result.add newFastAsgnStmt(newDotExpr(scratchObj, field), a)

        var fieldA = newSym(skField, tmpName, nextSymId idgen, objType.owner, n.info, g.config.options)
        fieldA.typ = getSysType(g, n.info, tyInt)
        objType.addField(fieldA, g.cache, idgen)
        result.add newFastAsgnStmt(newDotExpr(scratchObj, fieldA), n[2])
        result.add newFastAsgnStmt(newDotExpr(scratchObj, fieldB), n[3])

        let threadLocal = addLocalVar(g, varSection, nilPNode, idgen, owner, fieldA.typ,
                                      indirectAccess(castExpr, fieldA, n.info),
                                      useShallowCopy=true)
        slice[2] = threadLocal.newSymNode
      else:
        let a = genAddrOf(n, idgen)
        field.typ = a.typ
        objType.addField(field, g.cache, idgen)
        result.add newFastAsgnStmt(newDotExpr(scratchObj, field), a)
        result.add newFastAsgnStmt(newDotExpr(scratchObj, fieldB), genHigh(g, n))

        slice[2] = newIntLit(g, n.info, 0)
      # the array itself does not need to go through a thread local variable:
      slice[1] = genDeref(indirectAccess(castExpr, field, n.info))

      let threadLocal = addLocalVar(g, varSection, nilPNode, idgen, owner, fieldB.typ,
                                    indirectAccess(castExpr, fieldB, n.info),
                                    useShallowCopy=true)
      slice[3] = threadLocal.newSymNode
      call.add slice
    elif (let size = computeSize(g.config, argType); size < 0 or size > 16) and
        n.getRoot != nil:
      # it is more efficient to pass a pointer instead:
      let a = genAddrOf(n, idgen)
      field.typ = a.typ
      objType.addField(field, g.cache, idgen)
      result.add newFastAsgnStmt(newDotExpr(scratchObj, field), a)
      let threadLocal = addLocalVar(g, varSection, nilPNode, idgen, owner, field.typ,
                                    indirectAccess(castExpr, field, n.info),
                                    useShallowCopy=true)
      call.add(genDeref(threadLocal.newSymNode))
    else:
      # boring case
      field.typ = argType
      objType.addField(field, g.cache, idgen)
      result.add newFastAsgnStmt(newDotExpr(scratchObj, field), n)
      let threadLocal = addLocalVar(g, varSection, varInit,
                                    idgen, owner, field.typ,
                                    indirectAccess(castExpr, field, n.info),
                                    useShallowCopy=true)
      call.add(threadLocal.newSymNode)

proc wrapProcForSpawn*(g: ModuleGraph; idgen: IdGenerator; owner: PSym; spawnExpr: PNode; retType: PType;
                       barrier, dest: PNode = nilPNode): PNode =
  # if 'barrier' != nil, then it is in a 'parallel' section and we
  # generate quite different code
  let n = spawnExpr[^2]
  let spawnKind = spawnResult(retType, barrier!=nil)
  case spawnKind
  of srVoid:
    g.config.internalAssert dest == nil
    result = newNodeI(nkStmtList, n.info)
  of srFlowVar:
    g.config.internalAssert dest == nil
    result = newNodeIT(nkStmtListExpr, n.info, retType)
  of srByVar:
    if dest == nil: localReport(g.config, n, reportSem rsemCannotDiscardSpawn)
    result = newNodeI(nkStmtList, n.info)

  if n.kind notin nkCallKinds:
    localReport(g.config, n, reportSem rsemSpawnRequiresCall)
    return
  if optThreadAnalysis in g.config.globalOptions:
    if {tfThread, tfNoSideEffect} * n[0].typ.flags == {}:
      localReport(g.config, n.info, reportTyp(
        rsemSpawnRequiresGcSafe, n[0].typ, ast = n[0]))

  var fn = n[0]
  let
    name = (if fn.kind == nkSym: fn.sym.name.s else: genPrefix) & "Wrapper"
    wrapperProc = newSym(
      skProc, getIdent(g.cache, name), nextSymId idgen, owner, fn.info, g.config.options)

    threadParam = newSym(
      skParam, getIdent(g.cache, "thread"), nextSymId idgen, wrapperProc, n.info, g.config.options)

    argsParam = newSym(
      skParam, getIdent(g.cache, "args"), nextSymId idgen, wrapperProc, n.info, g.config.options)

  wrapperProc.flags.incl sfInjectDestructors
  block:
    let ptrType = getSysType(g, n.info, tyPointer)
    threadParam.typ = ptrType
    argsParam.typ = ptrType
    argsParam.position = 1

  var objType = createObj(g, idgen, owner, n.info)
  incl(objType.flags, tfFinal)
  let castExpr = createCastExpr(argsParam, objType, idgen)

  var scratchObj = newSym(skVar, getIdent(g.cache, "scratch"), nextSymId idgen, owner, n.info, g.config.options)
  block:
    scratchObj.typ = objType
    incl(scratchObj.flags, sfFromGeneric)
    let varSectionB = newTreeI(nkVarSection, n.info):
      newIdentDefs(scratchObj.newSymNode)
    result.add varSectionB

  var call = newNodeIT(nkCall, n.info, n.typ)
  # templates and macros are in fact valid here due to the nature of
  # the transformation:
  if fn.kind == nkClosure or (fn.typ != nil and fn.typ.callConv == ccClosure):
    localReport(g.config, n.info, reportAst(rsemSpawnForbidsClosure, fn))

  if not (fn.kind == nkSym and fn.sym.kind in {skProc, skTemplate, skMacro,
                                               skFunc, skMethod, skConverter}):
    # for indirect calls we pass the function pointer in the scratchObj
    var argType = n[0].typ.skipTypes(abstractInst)
    var field = newSym(skField, getIdent(g.cache, "fn"), nextSymId idgen, owner, n.info, g.config.options)
    field.typ = argType
    objType.addField(field, g.cache, idgen)
    result.add newFastAsgnStmt(newDotExpr(scratchObj, field), n[0])
    fn = indirectAccess(castExpr, field, n.info)
  elif fn.kind == nkSym and fn.sym.kind == skIterator:
    localReport(g.config, n.info, reportAst(rsemSpawnForbidsIterator, fn))

  call.add(fn)
  var varSection = newNodeI(nkVarSection, n.info)
  var varInit = newNodeI(nkStmtList, n.info)
  if barrier.isNil:
    setupArgsForConcurrency(g, n, objType, idgen, wrapperProc, scratchObj, castExpr, call,
                            varSection, varInit, result)
  else:
    setupArgsForParallelism(g, n, objType, idgen, wrapperProc, scratchObj, castExpr, call,
                            varSection, varInit, result)

  var barrierAsExpr: PNode = nilPNode
  if barrier != nil:
    let typ = newType(tyPtr, nextTypeId idgen, owner)
    typ.rawAddSon(magicsys.getCompilerProc(g, "Barrier").typ)
    var field = newSym(
      skField, getIdent(g.cache, "barrier"), nextSymId idgen, owner, n.info, g.config.options)

    field.typ = typ
    objType.addField(field, g.cache, idgen)
    result.add newFastAsgnStmt(newDotExpr(scratchObj, field), barrier)
    barrierAsExpr = indirectAccess(castExpr, field, n.info)

  var fvField, fvAsExpr: PNode = nilPNode
  if spawnKind == srFlowVar:
    var field = newSym(skField, getIdent(g.cache, "fv"), nextSymId idgen, owner, n.info, g.config.options)
    field.typ = retType
    objType.addField(field, g.cache, idgen)
    fvField = newDotExpr(scratchObj, field)
    fvAsExpr = indirectAccess(castExpr, field, n.info)
    # create flowVar:
    result.add newFastAsgnStmt(fvField, callProc(spawnExpr[^1]))
    if barrier == nil:
      result.add callCodegenProc(g, "nimFlowVarCreateSemaphore", fvField.info, fvField)

  elif spawnKind == srByVar:
    var field = newSym(skField, getIdent(g.cache, "fv"), nextSymId idgen, owner, n.info, g.config.options)
    field.typ = newType(tyPtr, nextTypeId idgen, objType.owner)
    field.typ.rawAddSon(retType)
    objType.addField(field, g.cache, idgen)
    fvAsExpr = indirectAccess(castExpr, field, n.info)
    result.add newFastAsgnStmt(newDotExpr(scratchObj, field), genAddrOf(dest, idgen))

  createTypeBoundOps(g, nil, objType, n.info, idgen)
  createWrapperProc(g, fn, threadParam, argsParam,
                      varSection, varInit, call,
                      barrierAsExpr, fvAsExpr, idgen, spawnKind, wrapperProc)
  result.add callCodegenProc(g, "nimSpawn" & $spawnExpr.len, wrapperProc.info,
    wrapperProc.newSymNode, genAddrOf(scratchObj.newSymNode, idgen), nilPNode, spawnExpr)

  if spawnKind == srFlowVar: result.add fvField
