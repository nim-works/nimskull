#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from cgen.nim

const
  RangeExpandLimit = 256      # do not generate ranges
                              # over 'RangeExpandLimit' elements
  stringCaseThreshold = 8
    # above X strings a hash-switch for strings is generated

proc isAssignedImmediately(conf: ConfigRef; n: PNode): bool {.inline.} =
  if n.kind == nkEmpty: return false
  if isInvalidReturnType(conf, n.typ):
    # var v = f()
    # is transformed into: var v;  f(addr v)
    # where 'f' **does not** initialize the result!
    return false
  result = true

proc inExceptBlockLen(p: BProc): int =
  for x in p.nestedTryStmts:
    if x.inExcept: result.inc

proc startBlockInternal(p: BProc): int {.discardable.} =
  inc(p.labels)
  result = p.blocks.len
  setLen(p.blocks, result + 1)
  p.blocks[result].id = p.labels
  p.blocks[result].nestedTryStmts = p.nestedTryStmts.len.int16
  p.blocks[result].nestedExceptStmts = p.inExceptBlockLen.int16

template startBlock(p: BProc, start: FormatStr = "{$n",
                args: varargs[Rope]): int =
  lineCg(p, cpsStmts, start, args)
  startBlockInternal(p)

proc endBlock(p: BProc)

proc genVarTuple(p: BProc, n: PNode) =
  var tup, field: TLoc
  p.config.internalAssert(n.kind == nkVarTuple, n.info, "genVarTuple")

  # if we have a something that's been captured, use the lowering instead:
  for i in 0..<n.len-2:
    if n[i].kind != nkSym:
      genStmts(p, lowerTupleUnpacking(p.module.g.graph, n, p.module.idgen, p.prc))
      return

  genLineDir(p, n)
  initLocExpr(p, n[^1], tup)
  var t = tup.t.skipTypes(abstractInst)
  for i in 0..<n.len-2:
    let vn = n[i]
    let v = vn.sym
    if sfGlobal in v.flags:
      assignGlobalVar(p, vn, "")
      genObjectInit(p, cpsInit, v.typ, v.loc, constructObj)
    else:
      assignLocalVar(p, vn)
      initLocalVar(p, v, immediateAsgn=isAssignedImmediately(p.config, n[^1]))
    initLoc(field, locExpr, vn, tup.storage)
    if t.kind == tyTuple:
      field.r = "$1.Field$2" % [rdLoc(tup), rope(i)]
    else:
      p.config.internalAssert(t.n[i].kind == nkSym, n.info, "genVarTuple")
      field.r = "$1.$2" % [rdLoc(tup), mangleRecFieldName(p.module, t.n[i].sym)]
    putLocIntoDest(p, v.loc, field)

proc loadInto(p: BProc, le, ri: PNode, a: var TLoc) {.inline.} =
  if ri.kind in nkCallKinds and (ri[0].kind != nkSym or
                                 ri[0].sym.magic == mNone):
    genAsgnCall(p, le, ri, a)
  else:
    # this is a hacky way to fix #1181 (tmissingderef)::
    #
    #  var arr1 = cast[ptr array[4, int8]](addr foo)[]
    #
    # However, fixing this properly really requires modelling 'array' as
    # a 'struct' in C to preserve dereferencing semantics completely. Not
    # worth the effort until version 1.0 is out.
    a.flags.incl(lfEnforceDeref)
    expr(p, ri, a)

proc assignLabel(b: var TBlock): Rope {.inline.} =
  b.label = "LA" & b.id.rope
  result = b.label

proc blockBody(b: var TBlock): Rope =
  result = b.sections[cpsLocals]
  if b.frameLen > 0:
    result.addf("FR_.len+=$1;$n", [b.frameLen.rope])
  result.add(b.sections[cpsInit])
  result.add(b.sections[cpsStmts])

proc endBlock(p: BProc, blockEnd: Rope) =
  let topBlock = p.blocks.len-1
  # the block is merged into the parent block
  p.blocks[topBlock-1].sections[cpsStmts].add(p.blocks[topBlock].blockBody)
  setLen(p.blocks, topBlock)
  # this is done after the block is popped so $n is
  # properly indented when pretty printing is enabled
  line(p, cpsStmts, blockEnd)

proc endBlock(p: BProc) =
  let topBlock = p.blocks.len - 1
  let frameLen = p.blocks[topBlock].frameLen
  var blockEnd: Rope
  if frameLen > 0:
    blockEnd.addf("FR_.len-=$1;$n", [frameLen.rope])
  if p.blocks[topBlock].label != "":
    blockEnd.addf("} $1: ;$n", [p.blocks[topBlock].label])
  else:
    blockEnd.addf("}$n", [])
  endBlock(p, blockEnd)

proc exprBlock(p: BProc, n: PNode, d: var TLoc) =
  startBlock(p)
  expr(p, n, d)
  endBlock(p)

template preserveBreakIdx(body: untyped): untyped =
  var oldBreakIdx = p.breakIdx
  body
  p.breakIdx = oldBreakIdx

proc blockLeaveActions(p: BProc, howManyTrys, howManyExcepts: int) =
  # Called by return and break stmts.
  # Deals with issues faced when jumping out of try/except/finally stmts.

  var stack = newSeq[tuple[fin: PNode, inExcept: bool, label: Natural]](0)

  inc p.withinBlockLeaveActions
  for i in 1..howManyTrys:
    let tryStmt = p.nestedTryStmts.pop
    # Pop this try-stmt of the list of nested trys
    # so we don't infinite recurse on it in the next step.
    stack.add(tryStmt)

    # Find finally-stmt for this try-stmt
    # and generate a copy of its sons
    var finallyStmt = tryStmt.fin
    if finallyStmt != nil:
      genStmts(p, finallyStmt[0])

  dec p.withinBlockLeaveActions

  # push old elements again:
  for i in countdown(howManyTrys-1, 0):
    p.nestedTryStmts.add(stack[i])

  # Pop exceptions that was handled by the
  # except-blocks we are in
  if noSafePoints notin p.flags:
    for i in countdown(howManyExcepts-1, 0):
      linefmt(p, cpsStmts, "#popCurrentException();$n", [])

proc genBreakState(p: BProc, n: PNode, d: var TLoc) =
  ## Generates the code for the ``mFinished`` magic, which tests if a
  ## closure iterator is in the "finished" state (i.e. the internal
  ## ``state`` field has a value < 0)
  var a: TLoc
  initLoc(d, locExpr, n, OnUnknown)

  let arg = n[1]
  if arg.kind == nkClosure:
    initLocExpr(p, arg[1], a)
    d.r = "(((NI*) $1)[1] < 0)" % [rdLoc(a)]
  else:
    initLocExpr(p, arg, a)
    # the environment is guaranteed to contain the 'state' field at offset 1:
    d.r = "((((NI*) $1.ClE_0)[1]) < 0)" % [rdLoc(a)]

proc genGotoVar(p: BProc; value: PNode) =
  if value.kind notin {nkCharLit..nkUInt64Lit}:
    localReport(p.config, value, reportSem rsemExpectedLiteralForGoto)

  else:
    lineF(p, cpsStmts, "goto NIMSTATE_$#;$n", [value.intVal.rope])

proc genBracedInit(p: BProc, n: PNode; isConst: bool; optionalType: PType): Rope

proc potentialValueInit(p: BProc; v: PSym; value: PNode): Rope =
  if lfDynamicLib in v.loc.flags or sfThread in v.flags:
    result = ""
  elif sfGlobal in v.flags and value != nil and isDeepConstExpr(value) and
      p.withinLoop == 0 and not containsGarbageCollectedRef(v.typ):
    #echo "New code produced for ", v.name.s, " ", p.config $ value.info
    result = genBracedInit(p, value, isConst = false, v.typ)
  else:
    result = ""

proc genSingleVar(p: BProc, v: PSym; vn, value: PNode) =
  if sfGoto in v.flags:
    # translate 'var state {.goto.} = X' into 'goto LX':
    genGotoVar(p, value)
    return
  var targetProc = p
  var traverseProc: Rope
  let valueAsRope = potentialValueInit(p, v, value)
  if sfGlobal in v.flags:
    if v.flags * {sfImportc, sfExportc} == {sfImportc} and
        value.kind == nkEmpty and
        v.loc.flags * {lfHeader, lfNoDecl} != {}:
      return
    if sfPure in v.flags:
      # v.owner.kind != skModule:
      targetProc = p.module.preInitProc
    assignGlobalVar(targetProc, vn, valueAsRope)
    # XXX: be careful here.
    # Global variables should not be zeromem-ed within loops
    # (see bug #20).
    # That's why we are doing the construction inside the preInitProc.
    # genObjectInit relies on the C runtime's guarantees that
    # global variables will be initialized to zero.
    if valueAsRope == "":
      var loc = v.loc

      # When the native TLS is unavailable, a global thread-local variable needs
      # one more layer of indirection in order to access the TLS block.
      # Only do this for complex types that may need a call to `objectInit`
      if sfThread in v.flags and emulatedThreadVars(p.config) and
        isComplexValueType(v.typ):
        initLocExprSingleUse(p.module.preInitProc, vn, loc)
      genObjectInit(p.module.preInitProc, cpsInit, v.typ, loc, constructObj)
    # Alternative construction using default constructor (which may zeromem):
    # if sfImportc notin v.flags: constructLoc(p.module.preInitProc, v.loc)
    if sfExportc in v.flags and p.module.g.generatedHeader != nil:
      genVarPrototype(p.module.g.generatedHeader, vn)
  else:
    let imm = isAssignedImmediately(p.config, value)
    assignLocalVar(p, vn)
    initLocalVar(p, v, imm)

  if traverseProc == "":
    traverseProc = ~"NULL"

  if value.kind != nkEmpty and valueAsRope == "":
    genLineDir(targetProc, vn)
    loadInto(targetProc, vn, value, v.loc)

proc genSingleVar(p: BProc, a: PNode) =
  let v = a[0].sym
  genSingleVar(p, v, a[0], a[2])

proc genClosureVar(p: BProc, a: PNode) =
  var immediateAsgn = a[2].kind != nkEmpty
  var v: TLoc
  initLocExpr(p, a[0], v)
  genLineDir(p, a)
  if immediateAsgn:
    loadInto(p, a[0], a[2], v)
  else:
    constructLoc(p, v)

proc genVarStmt(p: BProc, n: PNode) =
  for it in n.sons:
    if it.kind == nkCommentStmt: continue
    if it.kind == nkIdentDefs:
      # can be a lifted var nowadays ...
      if it[0].kind == nkSym:
        genSingleVar(p, it)
      else:
        genClosureVar(p, it)
    else:
      genVarTuple(p, it)

proc genIf(p: BProc, n: PNode, d: var TLoc) =
  #
  #  { if (!expr1) goto L1;
  #   thenPart }
  #  goto LEnd
  #  L1:
  #  { if (!expr2) goto L2;
  #   thenPart2 }
  #  goto LEnd
  #  L2:
  #  { elsePart }
  #  Lend:
  var
    a: TLoc
    lelse: TLabel
  if not isEmptyType(n.typ) and d.k == locNone:
    getTemp(p, n.typ, d)
  genLineDir(p, n)
  let lend = getLabel(p)
  for it in n.sons:
    # bug #4230: avoid false sharing between branches:
    if d.k == locTemp and isEmptyType(n.typ): d.k = locNone
    if it.len == 2:
      startBlock(p)
      initLocExprSingleUse(p, it[0], a)
      lelse = getLabel(p)
      inc(p.labels)
      lineF(p, cpsStmts, "if (!$1) goto $2;$n",
            [rdLoc(a), lelse])
      expr(p, it[1], d)
      endBlock(p)
      if n.len > 1:
        lineF(p, cpsStmts, "goto $1;$n", [lend])
      fixLabel(p, lelse)
    elif it.len == 1:
      startBlock(p)
      expr(p, it[0], d)
      endBlock(p)
    else: internalError(p.config, n.info, "genIf()")
  if n.len > 1: fixLabel(p, lend)

proc genReturnStmt(p: BProc, t: PNode) =
  if nfPreventCg in t.flags: return
  p.flags.incl beforeRetNeeded
  genLineDir(p, t)
  if (t[0].kind != nkEmpty): genStmts(p, t[0])
  blockLeaveActions(p,
    howManyTrys    = p.nestedTryStmts.len,
    howManyExcepts = p.inExceptBlockLen)
  if (p.finallySafePoints.len > 0) and noSafePoints notin p.flags:
    # If we're in a finally block, and we came here by exception
    # consume it before we return.
    var safePoint = p.finallySafePoints[^1]
    linefmt(p, cpsStmts, "if ($1.status != 0) #popCurrentException();$n", [safePoint])
  lineF(p, cpsStmts, "goto BeforeRet_;$n", [])

proc genGotoForCase(p: BProc; caseStmt: PNode) =
  for i in 1..<caseStmt.len:
    startBlock(p)
    let it = caseStmt[i]
    for j in 0..<it.len-1:
      if it[j].kind == nkRange:
        localReport(p.config, it, reportSem rsemDisallowedRangeForComputedGoto)
        return
      let val = getOrdValue(it[j])
      lineF(p, cpsStmts, "NIMSTATE_$#:$n", [val.rope])
    genStmts(p, it.lastSon)
    endBlock(p)


iterator fieldValuePairs(n: PNode): tuple[memberSym, valueSym: PNode] =
  assert(n.kind in {nkLetSection, nkVarSection})
  for identDefs in n:
    if identDefs.kind == nkIdentDefs:
      let valueSym = identDefs[^1]
      for i in 0..<identDefs.len-2:
        let memberSym = identDefs[i]
        yield((memberSym: memberSym, valueSym: valueSym))

proc genComputedGoto(p: BProc; n: PNode) =
  # first pass: Generate array of computed labels:

  # flatten the loop body because otherwise let and var sections
  # wrapped inside stmt lists by inject destructors won't be recognised
  let n = n.flattenStmts()
  var casePos = -1
  var arraySize: int
  for i in 0..<n.len:
    let it = n[i]
    if it.kind == nkCaseStmt:
      if lastSon(it).kind != nkOfBranch:
        localReport(p.config, it, reportSem rsemExpectedExhaustiveCaseForComputedGoto)
        return

      casePos = i
      if enumHasHoles(it[0].typ):
        localReport(p.config, it, reportSem rsemExpectedUnholyEnumForComputedGoto)
        return

      let aSize = lengthOrd(p.config, it[0].typ)
      if aSize > 10_000:
        localReport(p.config, it, reportSem rsemTooManyEntriesForComputedGoto)
        return

      arraySize = toInt(aSize)
      if firstOrd(p.config, it[0].typ) != 0:
        localReport(p.config, it, reportSem rsemExpectedLow0ForComputedGoto)
        return

  if casePos < 0:
    localReport(p.config, n, reportSem rsemExpectedCaseForComputedGoto)
    return

  var id = p.labels+1
  inc p.labels, arraySize+1
  let tmp = "TMP$1_" % [id.rope]
  var gotoArray = "static void* $#[$#] = {" % [tmp, arraySize.rope]
  for i in 1..arraySize-1:
    gotoArray.addf("&&TMP$#_, ", [rope(id+i)])
  gotoArray.addf("&&TMP$#_};$n", [rope(id+arraySize)])
  line(p, cpsLocals, gotoArray)

  for j in 0..<casePos:
    genStmts(p, n[j])

  let caseStmt = n[casePos]
  var a: TLoc
  initLocExpr(p, caseStmt[0], a)
  # first goto:
  lineF(p, cpsStmts, "goto *$#[$#];$n", [tmp, a.rdLoc])

  for i in 1..<caseStmt.len:
    startBlock(p)
    let it = caseStmt[i]
    for j in 0..<it.len-1:
      if it[j].kind == nkRange:
        localReport(p.config, it, reportSem rsemDisallowedRangeForComputedGoto)
        return

      let val = getOrdValue(it[j])
      lineF(p, cpsStmts, "TMP$#_:$n", [intLiteral(toInt64(val)+id+1)])

    genStmts(p, it.lastSon)

    for j in casePos+1..<n.len:
      genStmts(p, n[j])

    for j in 0..<casePos:
      # prevent new local declarations
      # compile declarations as assignments
      let it = n[j]
      if it.kind in {nkLetSection, nkVarSection}:
        let asgn = copyNode(it)
        asgn.transitionSonsKind(nkAsgn)
        asgn.sons.setLen 2
        for sym, value in it.fieldValuePairs:
          if value.kind != nkEmpty:
            asgn[0] = sym
            asgn[1] = value
            genStmts(p, asgn)
      else:
        genStmts(p, it)

    var a: TLoc
    initLocExpr(p, caseStmt[0], a)
    lineF(p, cpsStmts, "goto *$#[$#];$n", [tmp, a.rdLoc])
    endBlock(p)

  for j in casePos+1..<n.len:
    genStmts(p, n[j])


proc genWhileStmt(p: BProc, t: PNode) =
  # we don't generate labels here as for example GCC would produce
  # significantly worse code
  var
    a: TLoc
  assert(t.len == 2)
  inc(p.withinLoop)
  genLineDir(p, t)

  preserveBreakIdx:
    var loopBody = t[1]
    if loopBody.stmtsContainPragma(wComputedGoto) and
       hasComputedGoto in CC[p.config.cCompiler].props:
         # for closure support weird loop bodies are generated:
      if loopBody.len == 2 and loopBody[0].kind == nkEmpty:
        loopBody = loopBody[1]
      genComputedGoto(p, loopBody)
    else:
      p.breakIdx = startBlock(p, "while (1) {$n")
      p.blocks[p.breakIdx].isLoop = true
      initLocExpr(p, t[0], a)
      if (t[0].kind != nkIntLit) or (t[0].intVal == 0):
        let label = assignLabel(p.blocks[p.breakIdx])
        lineF(p, cpsStmts, "if (!$1) goto $2;$n", [rdLoc(a), label])
      genStmts(p, loopBody)

      if optProfiler in p.options:
        # invoke at loop body exit:
        linefmt(p, cpsStmts, "#nimProfile();$n", [])
      endBlock(p)

  dec(p.withinLoop)

proc genBlock(p: BProc, n: PNode, d: var TLoc) =
  if not isEmptyType(n.typ):
    # bug #4505: allocate the temp in the outer scope
    # so that it can escape the generated {}:
    if d.k == locNone:
      getTemp(p, n.typ, d)
    d.flags.incl(lfEnforceDeref)
  preserveBreakIdx:
    p.breakIdx = startBlock(p)
    if n[0].kind != nkEmpty:
      # named block?
      assert(n[0].kind == nkSym)
      var sym = n[0].sym
      sym.loc.k = locOther
      sym.position = p.breakIdx+1
    expr(p, n[1], d)
    endBlock(p)

proc genBreakStmt(p: BProc, t: PNode) =
  var idx = p.breakIdx
  if t[0].kind != nkEmpty:
    # named break?
    assert(t[0].kind == nkSym)
    var sym = t[0].sym
    doAssert(sym.loc.k == locOther)
    idx = sym.position-1
  else:
    # an unnamed 'break' can only break a loop after 'transf' pass:
    while idx >= 0 and not p.blocks[idx].isLoop: dec idx
    p.config.internalAssert(idx >= 0 and p.blocks[idx].isLoop, t.info, "no loop to break")
  let label = assignLabel(p.blocks[idx])
  blockLeaveActions(p,
    p.nestedTryStmts.len - p.blocks[idx].nestedTryStmts,
    p.inExceptBlockLen - p.blocks[idx].nestedExceptStmts)
  genLineDir(p, t)
  lineF(p, cpsStmts, "goto $1;$n", [label])

proc raiseExit(p: BProc) =
  assert p.config.exc == excGoto
  if nimErrorFlagDisabled notin p.flags:
    p.flags.incl nimErrorFlagAccessed
    if p.nestedTryStmts.len == 0:
      p.flags.incl beforeRetNeeded
      # easy case, simply goto 'ret':
      lineCg(p, cpsStmts, "if (NIM_UNLIKELY(*nimErr_)) goto BeforeRet_;$n", [])
    else:
      lineCg(p, cpsStmts, "if (NIM_UNLIKELY(*nimErr_)) goto LA$1_;$n",
        [p.nestedTryStmts[^1].label])

proc raiseInstr(p: BProc): Rope =
  if p.config.exc == excGoto:
    let L = p.nestedTryStmts.len
    if L == 0:
      p.flags.incl beforeRetNeeded
      # easy case, simply goto 'ret':
      result = ropecg(p.module, "goto BeforeRet_;$n", [])
    else:
      # raise inside an 'except' must go to the finally block,
      # raise outside an 'except' block must go to the 'except' list.
      result = ropecg(p.module, "goto LA$1_;$n",
        [p.nestedTryStmts[L-1].label])
      # + ord(p.nestedTryStmts[L-1].inExcept)])
  else:
    result = ""

proc genRaiseStmt(p: BProc, t: PNode) =
  if t[0].kind != nkEmpty:
    var a: TLoc
    initLocExprSingleUse(p, t[0], a)
    var e = rdLoc(a)
    discard getTypeDesc(p.module, t[0].typ)
    var typ = skipTypes(t[0].typ, abstractPtrs)
    genLineDir(p, t)
    if isImportedException(typ, p.config):
      lineF(p, cpsStmts, "throw $1;$n", [e])
    else:
      lineCg(p, cpsStmts, "#raiseExceptionEx((#Exception*)$1, $2, $3, $4, $5);$n",
          [e, makeCString(typ.sym.name.s),
          makeCString(if p.prc != nil: p.prc.name.s else: p.module.module.name.s),
          quotedFilename(p.config, t.info), toLinenumber(t.info)])

  else:
    genLineDir(p, t)
    # reraise the last exception:
    linefmt(p, cpsStmts, "#reraiseException();$n", [])
  let gotoInstr = raiseInstr(p)
  if gotoInstr != "":
    line(p, cpsStmts, gotoInstr)

template genCaseGenericBranch(p: BProc, b: PNode, e: TLoc,
                          rangeFormat, eqFormat: FormatStr, labl: TLabel) =
  var x, y: TLoc
  for i in 0..<b.len - 1:
    if b[i].kind == nkRange:
      initLocExpr(p, b[i][0], x)
      initLocExpr(p, b[i][1], y)
      lineCg(p, cpsStmts, rangeFormat,
           [rdCharLoc(e), rdCharLoc(x), rdCharLoc(y), labl])
    else:
      initLocExpr(p, b[i], x)
      lineCg(p, cpsStmts, eqFormat, [rdCharLoc(e), rdCharLoc(x), labl])

proc genCaseSecondPass(p: BProc, t: PNode, d: var TLoc,
                       labId, until: int): TLabel =
  var lend = getLabel(p)
  for i in 1..until:
    # bug #4230: avoid false sharing between branches:
    if d.k == locTemp and isEmptyType(t.typ): d.k = locNone
    lineF(p, cpsStmts, "LA$1_: ;$n", [rope(labId + i)])
    if t[i].kind == nkOfBranch:
      exprBlock(p, t[i][^1], d)
      lineF(p, cpsStmts, "goto $1;$n", [lend])
    else:
      exprBlock(p, t[i][0], d)
  result = lend

template genIfForCaseUntil(p: BProc, t: PNode, d: var TLoc,
                       rangeFormat, eqFormat: FormatStr,
                       until: int, a: TLoc): TLabel =
  # generate a C-if statement for a Nim case statement
  var res: TLabel
  var labId = p.labels
  for i in 1..until:
    inc(p.labels)
    if t[i].kind == nkOfBranch: # else statement
      genCaseGenericBranch(p, t[i], a, rangeFormat, eqFormat,
                           "LA" & rope(p.labels) & "_")
    else:
      lineF(p, cpsStmts, "goto LA$1_;$n", [rope(p.labels)])
  if until < t.len-1:
    inc(p.labels)
    var gotoTarget = p.labels
    lineF(p, cpsStmts, "goto LA$1_;$n", [rope(gotoTarget)])
    res = genCaseSecondPass(p, t, d, labId, until)
    lineF(p, cpsStmts, "LA$1_: ;$n", [rope(gotoTarget)])
  else:
    res = genCaseSecondPass(p, t, d, labId, until)
  res

template genCaseGeneric(p: BProc, t: PNode, d: var TLoc,
                    rangeFormat, eqFormat: FormatStr) =
  var a: TLoc
  initLocExpr(p, t[0], a)
  var lend = genIfForCaseUntil(p, t, d, rangeFormat, eqFormat, t.len-1, a)
  fixLabel(p, lend)

proc genCaseStringBranch(p: BProc, b: PNode, e: TLoc, labl: TLabel,
                         branches: var openArray[Rope]) =
  var x: TLoc
  for i in 0..<b.len - 1:
    assert(b[i].kind != nkRange)
    initLocExpr(p, b[i], x)
    assert(b[i].kind in {nkStrLit..nkTripleStrLit})
    var j = int(hashString(p.config, b[i].strVal) and high(branches))
    appcg(p.module, branches[j], "if (#eqStrings($1, $2)) goto $3;$n",
         [rdLoc(e), rdLoc(x), labl])

proc genStringCase(p: BProc, t: PNode, d: var TLoc) =
  # count how many constant strings there are in the case:
  var strings = 0
  for i in 1..<t.len:
    if t[i].kind == nkOfBranch: inc(strings, t[i].len - 1)
  if strings > stringCaseThreshold:
    var bitMask = math.nextPowerOfTwo(strings) - 1
    var branches: seq[Rope]
    newSeq(branches, bitMask + 1)
    var a: TLoc
    initLocExpr(p, t[0], a) # fist pass: generate ifs+goto:
    var labId = p.labels
    for i in 1..<t.len:
      inc(p.labels)
      if t[i].kind == nkOfBranch:
        genCaseStringBranch(p, t[i], a, "LA" & rope(p.labels) & "_",
                            branches)
      else:
        # else statement: nothing to do yet
        # but we reserved a label, which we use later
        discard
    linefmt(p, cpsStmts, "switch (#hashString($1) & $2) {$n",
            [rdLoc(a), bitMask])
    for j in 0..high(branches):
      if branches[j] != "":
        lineF(p, cpsStmts, "case $1: $n$2break;$n",
             [intLiteral(j), branches[j]])
    lineF(p, cpsStmts, "}$n", []) # else statement:
    if t[^1].kind != nkOfBranch:
      lineF(p, cpsStmts, "goto LA$1_;$n", [rope(p.labels)])
    # third pass: generate statements
    var lend = genCaseSecondPass(p, t, d, labId, t.len-1)
    fixLabel(p, lend)
  else:
    genCaseGeneric(p, t, d, "", "if (#eqStrings($1, $2)) goto $3;$n")

proc branchHasTooBigRange(b: PNode): bool =
  for it in b:
    # last son is block
    if (it.kind == nkRange) and
        it[1].intVal - it[0].intVal > RangeExpandLimit:
      return true

proc ifSwitchSplitPoint(p: BProc, n: PNode): int =
  for i in 1..<n.len:
    var branch = n[i]
    var stmtBlock = lastSon(branch)
    if stmtBlock.stmtsContainPragma(wLinearScanEnd):
      result = i
    elif hasSwitchRange notin CC[p.config.cCompiler].props:
      if branch.kind == nkOfBranch and branchHasTooBigRange(branch):
        result = i

proc genCaseRange(p: BProc, branch: PNode) =
  for j in 0..<branch.len-1:
    if branch[j].kind == nkRange:
      if hasSwitchRange in CC[p.config.cCompiler].props:
        lineF(p, cpsStmts, "case $1 ... $2:$n", [
            genLiteral(p, branch[j][0]),
            genLiteral(p, branch[j][1])])
      else:
        var v = copyNode(branch[j][0])
        while v.intVal <= branch[j][1].intVal:
          lineF(p, cpsStmts, "case $1:$n", [genLiteral(p, v)])
          inc(v.intVal)
    else:
      lineF(p, cpsStmts, "case $1:$n", [genLiteral(p, branch[j])])

proc genOrdinalCase(p: BProc, n: PNode, d: var TLoc) =
  # analyse 'case' statement:
  var splitPoint = ifSwitchSplitPoint(p, n)

  # generate if part (might be empty):
  var a: TLoc
  initLocExpr(p, n[0], a)
  var lend = if splitPoint > 0: genIfForCaseUntil(p, n, d,
                    rangeFormat = "if ($1 >= $2 && $1 <= $3) goto $4;$n",
                    eqFormat = "if ($1 == $2) goto $3;$n",
                    splitPoint, a) else: ""

  # generate switch part (might be empty):
  if splitPoint+1 < n.len:
    lineF(p, cpsStmts, "switch ($1) {$n", [rdCharLoc(a)])
    var hasDefault = false
    for i in splitPoint+1..<n.len:
      # bug #4230: avoid false sharing between branches:
      if d.k == locTemp and isEmptyType(n.typ): d.k = locNone
      var branch = n[i]
      if branch.kind == nkOfBranch:
        genCaseRange(p, branch)
      else:
        # else part of case statement:
        lineF(p, cpsStmts, "default:$n", [])
        hasDefault = true
      exprBlock(p, branch.lastSon, d)
      lineF(p, cpsStmts, "break;$n", [])
    if (hasAssume in CC[p.config.cCompiler].props) and not hasDefault:
      lineF(p, cpsStmts, "default: __assume(0);$n", [])
    lineF(p, cpsStmts, "}$n", [])
  if lend != "": fixLabel(p, lend)

proc genCase(p: BProc, t: PNode, d: var TLoc) =
  genLineDir(p, t)
  if not isEmptyType(t.typ) and d.k == locNone:
    getTemp(p, t.typ, d)
  case skipTypes(t[0].typ, abstractVarRange).kind
  of tyString:
    genStringCase(p, t, d)
  of tyFloat..tyFloat128:
    genCaseGeneric(p, t, d, "if ($1 >= $2 && $1 <= $3) goto $4;$n",
                            "if ($1 == $2) goto $3;$n")
  else:
    if t[0].kind == nkSym and sfGoto in t[0].sym.flags:
      genGotoForCase(p, t)
    else:
      genOrdinalCase(p, t, d)

proc bodyCanRaise(p: BProc; n: PNode): bool =
  case n.kind
  of nkCallKinds:
    result = canRaiseDisp(p, n[0])
    if not result:
      # also check the arguments:
      for i in 1 ..< n.len:
        if bodyCanRaise(p, n[i]): return true
  of nkRaiseStmt:
    result = true
  of nkTypeSection, nkProcDef, nkConverterDef, nkMethodDef, nkIteratorDef,
      nkMacroDef, nkTemplateDef, nkLambda, nkDo, nkFuncDef:
    result = false
  else:
    for i in 0 ..< safeLen(n):
      if bodyCanRaise(p, n[i]): return true
    result = false

proc genTryGoto(p: BProc; t: PNode; d: var TLoc) =
  let fin = if t[^1].kind == nkFinally: t[^1] else: nil
  inc p.labels
  let lab = p.labels
  let hasExcept = t[1].kind == nkExceptBranch
  if hasExcept: inc p.withinTryWithExcept
  p.nestedTryStmts.add((fin, false, Natural lab))

  p.flags.incl nimErrorFlagAccessed

  var errorFlagSet = false ## tracks whether the error flag is set to 'true'
    ## on a control-flow path connected to the finally section

  template checkSetsErrorFlag(n: PNode) =
    if fin != nil and not errorFlagSet:
      errorFlagSet = bodyCanRaise(p, n)

  if not isEmptyType(t.typ) and d.k == locNone:
    getTemp(p, t.typ, d)

  expr(p, t[0], d)
  checkSetsErrorFlag(t[0])

  if 1 < t.len and t[1].kind == nkExceptBranch:
    startBlock(p, "if (NIM_UNLIKELY(*nimErr_)) {$n")
  else:
    startBlock(p)
  linefmt(p, cpsStmts, "LA$1_:;$n", [lab])

  p.nestedTryStmts[^1].inExcept = true
  var i = 1
  while (i < t.len) and (t[i].kind == nkExceptBranch):

    inc p.labels
    let nextExcept = p.labels
    p.nestedTryStmts[^1].label = nextExcept

    # bug #4230: avoid false sharing between branches:
    if d.k == locTemp and isEmptyType(t.typ): d.k = locNone
    if t[i].len == 1:
      # general except section:
      if i > 1: lineF(p, cpsStmts, "else", [])
      startBlock(p)
      # we handled the exception, remember this:
      linefmt(p, cpsStmts, "*nimErr_ = NIM_FALSE;$n", [])
      expr(p, t[i][0], d)
      checkSetsErrorFlag(t[i][0])
    else:
      var orExpr = ""
      for j in 0..<t[i].len - 1:
        assert(t[i][j].kind == nkType)
        if orExpr != "": orExpr.add("||")
        let checkFor = genTypeInfo2Name(p.module, t[i][j].typ)
        let memberName = "Sup.m_type"
        appcg(p.module, orExpr, "#isObj(#nimBorrowCurrentException()->$1, $2)", [memberName, checkFor])

      if i > 1: line(p, cpsStmts, "else ")
      startBlock(p, "if ($1) {$n", [orExpr])
      # we handled the exception, remember this:
      linefmt(p, cpsStmts, "*nimErr_ = NIM_FALSE;$n", [])
      expr(p, t[i][^1], d)
      checkSetsErrorFlag(t[i][^1])

    linefmt(p, cpsStmts, "#popCurrentException();$n", [])
    linefmt(p, cpsStmts, "LA$1_:;$n", [nextExcept])
    endBlock(p)

    inc(i)
  discard pop(p.nestedTryStmts)
  endBlock(p)

  if i < t.len and t[i].kind == nkFinally:
    startBlock(p)
    # future direction: the code generator should track for each procedure
    # whether it observes the error flag. If the finally clause's body
    # doesn't observes it itself, and also doesn't call any procedure that
    # does, we can also omit the save/restore pair
    if not errorFlagSet:
      # this is an optimization; if the error flag is proven to never be
      # 'true' when the finally section is reached, we don't need to erase
      # nor restore it:
      genStmts(p, t[i][0])
    else:
      # pretend we did handle the error for the safe execution of the 'finally' section:
      p.procSec(cpsLocals).add(ropecg(p.module, "NIM_BOOL oldNimErrFin$1_;$n", [lab]))
      linefmt(p, cpsStmts, "oldNimErrFin$1_ = *nimErr_; *nimErr_ = NIM_FALSE;$n", [lab])
      genStmts(p, t[i][0])
      # this is correct for all these cases:
      # 1. finally is run during ordinary control flow
      # 2. finally is run after 'except' block handling: these however set the
      #    error back to nil.
      # 3. finally is run for exception handling code without any 'except'
      #    handler present or only handlers that did not match.
      linefmt(p, cpsStmts, "*nimErr_ = oldNimErrFin$1_;$n", [lab])
    endBlock(p)
  raiseExit(p)
  if hasExcept: inc p.withinTryWithExcept

proc genAsmOrEmitStmt(p: BProc, t: PNode, isAsmStmt=false): Rope =
  var res = ""
  for it in t.sons:
    case it.kind
    of nkStrLit..nkTripleStrLit:
      res.add(it.strVal)
    of nkSym:
      var sym = it.sym
      if sym.kind in {skProc, skFunc, skIterator, skMethod}:
        var a: TLoc
        initLocExpr(p, it, a)
        res.add($rdLoc(a))
      elif sym.kind == skType:
        res.add($getTypeDesc(p.module, sym.typ))
      else:
        discard getTypeDesc(p.module, skipTypes(sym.typ, abstractPtrs))
        var r = sym.loc.r
        if r == "":
          # if no name has already been given,
          # it doesn't matter much:
          r = mangleName(p.module, sym)
          sym.loc.r = r       # but be consequent!
        res.add($r)
    of nkTypeOfExpr:
      res.add($getTypeDesc(p.module, it.typ))
    else:
      discard getTypeDesc(p.module, skipTypes(it.typ, abstractPtrs))
      var a: TLoc
      initLocExpr(p, it, a)
      res.add($a.rdLoc)

  if isAsmStmt and hasGnuAsm in CC[p.config.cCompiler].props:
    for x in splitLines(res):
      var j = 0
      while j < x.len and x[j] in {' ', '\t'}: inc(j)
      if j < x.len:
        if x[j] in {'"', ':'}:
          # don't modify the line if already in quotes or
          # some clobber register list:
          result.add(x); result.add("\L")
        else:
          # ignore empty lines
          result.add("\"")
          result.add(x.replace("\"", "\\\""))
          result.add("\\n\"\n")
  else:
    res.add("\L")
    result = res.rope

proc genAsmStmt(p: BProc, t: PNode) =
  assert(t.kind == nkAsmStmt)
  genLineDir(p, t)
  var s = genAsmOrEmitStmt(p, t, isAsmStmt=true)
  # see bug #2362, "top level asm statements" seem to be a mis-feature
  # but even if we don't do this, the example in #2362 cannot possibly
  # work:
  if p.prc == nil:
    # top level asm statement?
    p.module.s[cfsProcHeaders].add runtimeFormat(CC[p.config.cCompiler].asmStmtFrmt, [s])
  else:
    p.s(cpsStmts).add indentLine(p, runtimeFormat(CC[p.config.cCompiler].asmStmtFrmt, [s]))

proc determineSection(n: PNode): TCFileSection =
  result = cfsProcHeaders
  if n.len >= 1 and n[0].kind in {nkStrLit..nkTripleStrLit}:
    let sec = n[0].strVal
    if sec.startsWith("/*TYPESECTION*/"): result = cfsTypes
    elif sec.startsWith("/*VARSECTION*/"): result = cfsVars
    elif sec.startsWith("/*INCLUDESECTION*/"): result = cfsHeaders

proc genEmit(p: BProc, t: PNode) =
  var s = genAsmOrEmitStmt(p, t[1])
  if p.prc == nil:
    # top level emit pragma?
    let section = determineSection(t[1])
    genCLineDir(p.module.s[section], t.info, p.config)
    p.module.s[section].add(s)
  else:
    genLineDir(p, t)
    line(p, cpsStmts, s)

proc genPragma(p: BProc, n: PNode) =
  for it in n.sons:
    case whichPragma(it)
    of wEmit: genEmit(p, it)
    else: discard

when false:
  proc genCaseObjDiscMapping(p: BProc, e: PNode, t: PType, field: PSym; d: var TLoc) =
    const ObjDiscMappingProcSlot = -5
    var theProc: PSym = nil
    for idx, p in items(t.methods):
      if idx == ObjDiscMappingProcSlot:
        theProc = p
        break
    if theProc == nil:
      theProc = genCaseObjDiscMapping(t, field, e.info, p.module.g.graph, p.module.idgen)
      t.methods.add((ObjDiscMappingProcSlot, theProc))
    var call = newNodeIT(nkCall, e.info, getSysType(p.module.g.graph, e.info, tyUInt8))
    call.add newSymNode(theProc)
    call.add e
    expr(p, call, d)

proc asgnFieldDiscriminant(p: BProc, e: PNode) =
  var a, tmp: TLoc
  var dotExpr = e[0]
  if dotExpr.kind == nkCheckedFieldExpr: dotExpr = dotExpr[0]
  initLocExpr(p, e[0], a)
  getTemp(p, a.t, tmp)
  expr(p, e[1], tmp)
  genAssignment(p, a, tmp, {})

proc genAsgn(p: BProc, e: PNode, fastAsgn: bool) =
  if e[0].kind == nkSym and sfGoto in e[0].sym.flags:
    genLineDir(p, e)
    genGotoVar(p, e[1])
  elif optFieldCheck in p.options and isDiscriminantField(e[0]):
    genLineDir(p, e)
    asgnFieldDiscriminant(p, e)
  else:
    let le = e[0]
    let ri = e[1]
    var a: TLoc
    discard getTypeDesc(p.module, le.typ.skipTypes(skipPtrs), skVar)
    initLoc(a, locNone, le, OnUnknown)
    a.flags.incl {lfEnforceDeref, lfPrepareForMutation}
    expr(p, le, a)
    a.flags.excl lfPrepareForMutation
    if fastAsgn: a.flags.incl lfNoDeepCopy
    assert(a.t != nil)
    genLineDir(p, ri)
    loadInto(p, le, ri, a)

proc genStmts(p: BProc, t: PNode) =
  var a: TLoc

  let isPush = p.config.hasHint(rsemExtendedContext)
  if isPush: pushInfoContext(p.config, t.info)
  expr(p, t, a)
  if isPush: popInfoContext(p.config)
  internalAssert p.config, a.k in {locNone, locTemp, locLocalVar, locExpr}
