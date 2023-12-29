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

proc isAssignedImmediately(conf: ConfigRef; n: CgNode): bool {.inline.} =
  if n.kind == cnkEmpty: return false
  if isInvalidReturnType(conf, n.typ):
    # var v = f()
    # is transformed into: var v;  f(addr v)
    # where 'f' **does not** initialize the result!
    return false
  result = true

proc inExceptBlockLen(p: BProc): int =
  for x in p.nestedTryStmts:
    if x.inExcept: result.inc

proc startBlockInternal(p: BProc, blk: int) =
  inc(p.labels)
  let result = p.blocks.len
  setLen(p.blocks, result + 1)
  p.blocks[result].id = p.labels
  p.blocks[result].blk = blk
  p.blocks[result].nestedTryStmts = p.nestedTryStmts.len.int16
  p.blocks[result].nestedExceptStmts = p.inExceptBlockLen.int16

template startBlock(p: BProc, start: FormatStr = "{$n",
                args: varargs[Rope]) =
  lineCg(p, cpsStmts, start, args)
  startBlockInternal(p, 0)

template startBlock(p: BProc, id: BlockId) =
  lineCg(p, cpsStmts, "{$n", [])
  startBlockInternal(p, id.int + 1)

proc endBlock(p: BProc)

proc loadInto(p: BProc, le, ri: CgNode, a: var TLoc) {.inline.} =
  if ri.kind in {cnkCall, cnkCheckedCall} and
     getCalleeMagic(ri[0]) == mNone:
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

proc stmtBlock(p: BProc, n: CgNode) =
  startBlock(p)
  genStmts(p, n)
  endBlock(p)

proc blockLeaveActions(p: BProc, howManyTrys, howManyExcepts: int) =
  # Called by return and break stmts.
  # Deals with issues faced when jumping out of try/except/finally stmts.

  var stack = newSeq[typeof(p.nestedTryStmts[0])](0)

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
  block:
    for i in countdown(howManyExcepts-1, 0):
      linefmt(p, cpsStmts, "#popCurrentException();$n", [])

proc genGotoVar(p: BProc; value: CgNode) =
  case value.kind
  of cnkIntLit, cnkUIntLit:
    lineF(p, cpsStmts, "goto NIMSTATE_$#;$n", [value.intVal.rope])
  else:
    localReport(p.config, value.info, reportSem rsemExpectedLiteralForGoto)

proc genBracedInit(p: BProc, n: CgNode; isConst: bool; optionalType: PType): Rope

proc genSingleVar(p: BProc, vn, value: CgNode) =
  ## Generates and emits the C code for the definition statement of a local.
  let v = vn.local

  if sfGoto in p.body[v].flags:
    # translate 'var state {.goto.} = X' into 'goto LX':
    genGotoVar(p, value)
    return

  let imm = isAssignedImmediately(p.config, value)
  assignLocalVar(p, vn)
  initLocalVar(p, v, imm)

  if value.kind != cnkEmpty:
    genLineDir(p, vn)
    loadInto(p, vn, value, p.locals[v])

proc genIf(p: BProc, n: CgNode) =
  #  if (expr1)
  #  {
  #    thenPart
  #  }
  var
    a: TLoc
  genLineDir(p, n)

  initLocExprSingleUse(p, n[0], a)
  lineF(p, cpsStmts, "if ($1)$n", [rdLoc(a)])
  stmtBlock(p, n[1])

proc genReturnStmt(p: BProc, t: CgNode) =
  p.flags.incl beforeRetNeeded
  genLineDir(p, t)
  blockLeaveActions(p,
    howManyTrys    = p.nestedTryStmts.len,
    howManyExcepts = p.inExceptBlockLen)
  lineF(p, cpsStmts, "goto BeforeRet_;$n", [])

proc genGotoForCase(p: BProc; caseStmt: CgNode) =
  for i in 1..<caseStmt.len:
    startBlock(p)
    let it = caseStmt[i]
    for j in 0..<it.len-1:
      if it[j].kind == cnkRange:
        localReport(p.config, it.info, reportSem rsemDisallowedRangeForComputedGoto)
        return
      let val = getOrdValue(it[j])
      lineF(p, cpsStmts, "NIMSTATE_$#:$n", [val.rope])
    genStmts(p, it.lastSon)
    endBlock(p)

proc genAsgn(p: BProc, e: CgNode)

proc genComputedGoto(p: BProc; n: CgNode) =
  # first pass: Generate array of computed labels:

  # flatten the loop body because otherwise let and var sections
  # wrapped inside stmt lists by inject destructors won't be recognised
  # XXX: ^^ this doesn't work as intended (see the comment in
  #      ``flattenStmts``)
  let n = n.flattenStmts()
  var casePos = -1
  var arraySize: int
  for i in 0..<n.len:
    let it = n[i]
    if it.kind == cnkCaseStmt:
      # XXX: move the checks into the semantic analysis phase
      if not isOfBranch(it[^1]):
        localReport(p.config, it.info, reportSem rsemExpectedExhaustiveCaseForComputedGoto)
        return

      casePos = i
      if enumHasHoles(it[0].typ):
        localReport(p.config, it.info, reportSem rsemExpectedUnholyEnumForComputedGoto)
        return

      let aSize = lengthOrd(p.config, it[0].typ)
      if aSize > 10_000:
        localReport(p.config, it.info, reportSem rsemTooManyEntriesForComputedGoto)
        return

      arraySize = toInt(aSize)
      if firstOrd(p.config, it[0].typ) != 0:
        localReport(p.config, it.info, reportSem rsemExpectedLow0ForComputedGoto)
        return

  if casePos < 0:
    localReport(p.config, n.info, reportSem rsemExpectedCaseForComputedGoto)
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
      if it[j].kind == cnkRange:
        localReport(p.config, it.info, reportSem rsemDisallowedRangeForComputedGoto)
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
      if it.kind == cnkDef:
        genAsgn(p, it)
      else:
        genStmts(p, it)

    var a: TLoc
    initLocExpr(p, caseStmt[0], a)
    lineF(p, cpsStmts, "goto *$#[$#];$n", [tmp, a.rdLoc])
    endBlock(p)

  for j in casePos+1..<n.len:
    genStmts(p, n[j])


proc genRepeatStmt(p: BProc, t: CgNode) =
  # we don't generate labels here as for example GCC would produce
  # significantly worse code
  inc(p.withinLoop)
  genLineDir(p, t)

  if true:
    var loopBody = t[0]
    if loopBody.stmtsContainPragma(wComputedGoto) and
       hasComputedGoto in CC[p.config.cCompiler].props:
      genComputedGoto(p, loopBody)
    else:
      startBlock(p, "while (1) {$n")
      genStmts(p, loopBody)

      if optProfiler in p.options:
        # invoke at loop body exit:
        linefmt(p, cpsStmts, "#nimProfile();$n", [])
      endBlock(p)

  dec(p.withinLoop)

proc genBlock(p: BProc, n: CgNode) =
  startBlock(p, n[0].label)
  genStmts(p, n[1])
  endBlock(p)

proc genBreakStmt(p: BProc, t: CgNode) =
  assert t[0].kind == cnkLabel
  var idx = p.blocks.high
  # search for the ``TBlock`` that corresponds to the label. `blk` stores the
  # ID offset by 1, which has to be accounted for here
  while idx >= 0 and p.blocks[idx].blk != (t[0].label.int + 1):
    dec idx

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

proc genRaiseStmt(p: BProc, t: CgNode) =
  if t[0].kind != cnkEmpty:
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

template genCaseGenericBranch(p: BProc, b: CgNode, e: TLoc,
                          rangeFormat, eqFormat: FormatStr, labl: TLabel) =
  var x, y: TLoc
  for i in 0..<b.len - 1:
    if b[i].kind == cnkRange:
      initLocExpr(p, b[i][0], x)
      initLocExpr(p, b[i][1], y)
      lineCg(p, cpsStmts, rangeFormat,
           [rdCharLoc(e), rdCharLoc(x), rdCharLoc(y), labl])
    else:
      initLocExpr(p, b[i], x)
      lineCg(p, cpsStmts, eqFormat, [rdCharLoc(e), rdCharLoc(x), labl])

proc genCaseSecondPass(p: BProc, t: CgNode,
                       labId, until: int): TLabel =
  var lend = getLabel(p)
  for i in 1..until:
    lineF(p, cpsStmts, "LA$1_: ;$n", [rope(labId + i)])
    if isOfBranch(t[i]):
      stmtBlock(p, t[i][^1])
      lineF(p, cpsStmts, "goto $1;$n", [lend])
    else:
      stmtBlock(p, t[i][0])
  result = lend

template genIfForCaseUntil(p: BProc, t: CgNode,
                       rangeFormat, eqFormat: FormatStr,
                       until: int, a: TLoc): TLabel =
  # generate a C-if statement for a Nim case statement
  var res: TLabel
  var labId = p.labels
  for i in 1..until:
    inc(p.labels)
    if isOfBranch(t[i]):
      genCaseGenericBranch(p, t[i], a, rangeFormat, eqFormat,
                           "LA" & rope(p.labels) & "_")
    else:
      lineF(p, cpsStmts, "goto LA$1_;$n", [rope(p.labels)])
  if until < t.len-1:
    inc(p.labels)
    var gotoTarget = p.labels
    lineF(p, cpsStmts, "goto LA$1_;$n", [rope(gotoTarget)])
    res = genCaseSecondPass(p, t, labId, until)
    lineF(p, cpsStmts, "LA$1_: ;$n", [rope(gotoTarget)])
  else:
    res = genCaseSecondPass(p, t, labId, until)
  res

template genCaseGeneric(p: BProc, t: CgNode,
                    rangeFormat, eqFormat: FormatStr) =
  var a: TLoc
  initLocExpr(p, t[0], a)
  var lend = genIfForCaseUntil(p, t, rangeFormat, eqFormat, t.len-1, a)
  fixLabel(p, lend)

proc genCaseStringBranch(p: BProc, b: CgNode, e: TLoc, labl: TLabel,
                         branches: var openArray[Rope]) =
  var x: TLoc
  for i in 0..<b.len - 1:
    assert(b[i].kind != cnkRange)
    initLocExpr(p, b[i], x)
    assert(b[i].kind == cnkStrLit)
    var j = int(hashString(p.config, b[i].strVal) and high(branches))
    appcg(p.module, branches[j], "if (#eqStrings($1, $2)) goto $3;$n",
         [rdLoc(e), rdLoc(x), labl])

proc genStringCase(p: BProc, t: CgNode) =
  # count how many constant strings there are in the case:
  var strings = 0
  for i in 1..<t.len:
    if isOfBranch(t[i]): inc(strings, t[i].len - 1)
  if strings > stringCaseThreshold:
    var bitMask = math.nextPowerOfTwo(strings) - 1
    var branches: seq[Rope]
    newSeq(branches, bitMask + 1)
    var a: TLoc
    initLocExpr(p, t[0], a) # fist pass: generate ifs+goto:
    var labId = p.labels
    for i in 1..<t.len:
      inc(p.labels)
      if isOfBranch(t[i]):
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
    if not isOfBranch(t[^1]):
      lineF(p, cpsStmts, "goto LA$1_;$n", [rope(p.labels)])
    # third pass: generate statements
    var lend = genCaseSecondPass(p, t, labId, t.len-1)
    fixLabel(p, lend)
  else:
    genCaseGeneric(p, t, "", "if (#eqStrings($1, $2)) goto $3;$n")

proc branchHasTooBigRange(b: CgNode): bool =
  for it in b:
    # last son is block
    if (it.kind == cnkRange) and
        getInt(it[1]) - getInt(it[0]) > RangeExpandLimit:
      return true

proc ifSwitchSplitPoint(p: BProc, n: CgNode): int =
  for i in 1..<n.len:
    let branch = n[i]
    if hasSwitchRange notin CC[p.config.cCompiler].props:
      if isOfBranch(branch) and branchHasTooBigRange(branch):
        result = i

proc genCaseRange(p: BProc, branch: PNode|CgNode) =
  const RangeKind =
    when branch is PNode: nkRange
    else:                 cnkRange

  template intLiteral(n: untyped): string =
    intLiteral(p, getInt(n), n.typ)

  for j in 0..<branch.len-1:
    if branch[j].kind == RangeKind:
      if hasSwitchRange in CC[p.config.cCompiler].props:
        lineF(p, cpsStmts, "case $1 ... $2:$n", [
            intLiteral(branch[j][0]),
            intLiteral(branch[j][1])])
      else:
        let
          typ   = branch[j][0].typ
          upper = getInt(branch[j][1])
        var v = getInt(branch[j][0])
        while v <= upper:
          lineF(p, cpsStmts, "case $1:$n", [intLiteral(p, v, typ)])
          inc(v)
    else:
      lineF(p, cpsStmts, "case $1:$n", [intLiteral(branch[j])])

proc genOrdinalCase(p: BProc, n: CgNode) =
  # analyse 'case' statement:
  var splitPoint = ifSwitchSplitPoint(p, n)

  # generate if part (might be empty):
  var a: TLoc
  initLocExpr(p, n[0], a)
  var lend = if splitPoint > 0: genIfForCaseUntil(p, n,
                    rangeFormat = "if ($1 >= $2 && $1 <= $3) goto $4;$n",
                    eqFormat = "if ($1 == $2) goto $3;$n",
                    splitPoint, a) else: ""

  # generate switch part (might be empty):
  if splitPoint+1 < n.len:
    lineF(p, cpsStmts, "switch ($1) {$n", [rdCharLoc(a)])
    var hasDefault = false
    for i in splitPoint+1..<n.len:
      let branch = n[i]
      if isOfBranch(branch):
        genCaseRange(p, branch)
      else:
        # else part of case statement:
        lineF(p, cpsStmts, "default:$n", [])
        hasDefault = true
      stmtBlock(p, branch.lastSon)
      lineF(p, cpsStmts, "break;$n", [])
    if (hasAssume in CC[p.config.cCompiler].props) and not hasDefault:
      lineF(p, cpsStmts, "default: __assume(0);$n", [])
    lineF(p, cpsStmts, "}$n", [])
  if lend != "": fixLabel(p, lend)

proc genCase(p: BProc, t: CgNode) =
  genLineDir(p, t)
  case skipTypes(t[0].typ, abstractVarRange).kind
  of tyString:
    genStringCase(p, t)
  of tyFloat..tyFloat64:
    genCaseGeneric(p, t, "if ($1 >= $2 && $1 <= $3) goto $4;$n",
                         "if ($1 == $2) goto $3;$n")
  else:
    if t[0].kind == cnkLocal and sfGoto in p.body[t[0].local].flags:
      genGotoForCase(p, t)
    else:
      genOrdinalCase(p, t)

proc bodyCanRaise(p: BProc; n: CgNode): bool =
  case n.kind
  of cnkCheckedCall:
    result = true
  of cnkRaiseStmt:
    result = true
  of cnkAtoms:
    result = false
  of cnkWithOperand:
    result = bodyCanRaise(p, n.operand)
  of cnkWithItems - {cnkCheckedCall, cnkRaiseStmt}:
    for it in n.items:
      if bodyCanRaise(p, it): return true
    result = false

proc genTryGoto(p: BProc; t: CgNode) =
  let fin = if t[^1].kind == cnkFinally: t[^1] else: nil
  inc p.labels
  let lab = p.labels
  let hasExcept = t[1].kind == cnkExcept
  if hasExcept: inc p.withinTryWithExcept
  p.nestedTryStmts.add((fin, false, Natural lab))

  p.flags.incl nimErrorFlagAccessed

  var errorFlagSet = false ## tracks whether the error flag is set to 'true'
    ## on a control-flow path connected to the finally section

  template checkSetsErrorFlag(n: CgNode) =
    if fin != nil and not errorFlagSet:
      errorFlagSet = bodyCanRaise(p, n)

  genStmts(p, t[0])
  checkSetsErrorFlag(t[0])

  if 1 < t.len and t[1].kind == cnkExcept:
    startBlock(p, "if (NIM_UNLIKELY(*nimErr_)) {$n")
  else:
    startBlock(p)
  linefmt(p, cpsStmts, "LA$1_:;$n", [lab])

  p.nestedTryStmts[^1].inExcept = true
  var i = 1
  while (i < t.len) and (t[i].kind == cnkExcept):

    inc p.labels
    let nextExcept = p.labels
    p.nestedTryStmts[^1].label = nextExcept

    if t[i].len == 1:
      # general except section:
      if i > 1: lineF(p, cpsStmts, "else", [])
      startBlock(p)
      # we handled the exception, remember this:
      linefmt(p, cpsStmts, "*nimErr_ = NIM_FALSE;$n", [])
      genStmts(p, t[i][0])
      checkSetsErrorFlag(t[i][0])
    else:
      var orExpr = ""
      for j in 0..<t[i].len - 1:
        assert(t[i][j].kind == cnkType)
        if orExpr != "": orExpr.add("||")
        let checkFor = genTypeInfo2Name(p.module, t[i][j].typ)
        let memberName = "Sup.m_type"
        appcg(p.module, orExpr, "#isObj(#nimBorrowCurrentException()->$1, $2)", [memberName, checkFor])

      if i > 1: line(p, cpsStmts, "else ")
      startBlock(p, "if ($1) {$n", [orExpr])
      # we handled the exception, remember this:
      linefmt(p, cpsStmts, "*nimErr_ = NIM_FALSE;$n", [])
      genStmts(p, t[i][^1])
      checkSetsErrorFlag(t[i][^1])

    linefmt(p, cpsStmts, "#popCurrentException();$n", [])
    linefmt(p, cpsStmts, "LA$1_:;$n", [nextExcept])
    endBlock(p)

    inc(i)
  discard pop(p.nestedTryStmts)
  endBlock(p)

  if i < t.len and t[i].kind == cnkFinally:
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

proc genAsmOrEmitStmt(p: BProc, t: CgNode, isAsmStmt=false): Rope =
  var res = ""
  for it in t.items:
    case it.kind
    of cnkStrLit:
      res.add(it.strVal)
    of cnkField:
        let sym = it.sym
        # special support for raw field symbols
        discard getTypeDesc(p.module, skipTypes(sym.typ, abstractPtrs))
        p.config.internalAssert(sym.locId != 0, it.info):
          "field's surrounding type not setup"
        res.add(p.fieldName(sym))
    of cnkLocal:
      # make sure the C type description is available:
      discard getTypeDesc(p.module, skipTypes(it.typ, abstractPtrs))
      res.add(rdLoc(p.locals[it.local]))
    of cnkType:
      res.add(getTypeDesc(p.module, it.typ))
    else:
      discard getTypeDesc(p.module, skipTypes(it.typ, abstractPtrs))
      var a: TLoc
      initLocExpr(p, it, a)
      res.add(a.rdLoc)

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

proc genAsmStmt(p: BProc, t: CgNode) =
  assert(t.kind == cnkAsmStmt)
  genLineDir(p, t)
  var s = genAsmOrEmitStmt(p, t, isAsmStmt=true)
  # see bug #2362, "top level asm statements" seem to be a mis-feature
  # but even if we don't do this, the example in #2362 cannot possibly
  # work:
  if sfTopLevel in p.prc.flags:
    # top level asm statement?
    p.module.s[cfsProcHeaders].add runtimeFormat(CC[p.config.cCompiler].asmStmtFrmt, [s])
  else:
    p.s(cpsStmts).add indentLine(p, runtimeFormat(CC[p.config.cCompiler].asmStmtFrmt, [s]))

proc determineSection(n: CgNode): TCFileSection =
  result = cfsProcHeaders
  if n.len >= 1 and n[0].kind == cnkStrLit:
    let sec = n[0].strVal
    if sec.startsWith("/*TYPESECTION*/"): result = cfsTypes
    elif sec.startsWith("/*VARSECTION*/"): result = cfsVars
    elif sec.startsWith("/*INCLUDESECTION*/"): result = cfsHeaders

proc genEmit(p: BProc, t: CgNode) =
  var s = genAsmOrEmitStmt(p, t)
  if sfTopLevel in p.prc.flags:
    # top level emit pragma?
    let section = determineSection(t)
    genCLineDir(p.module.s[section], t.info, p.config)
    p.module.s[section].add(s)
  else:
    genLineDir(p, t)
    line(p, cpsStmts, s)

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

proc asgnFieldDiscriminant(p: BProc, e: CgNode) =
  var a, tmp: TLoc
  var dotExpr = e[0]
  if dotExpr.kind == cnkCheckedFieldAccess: dotExpr = dotExpr[0]
  initLocExpr(p, e[0], a)
  getTemp(p, a.t, tmp)
  expr(p, e[1], tmp)
  genAssignment(p, a, tmp)

proc genAsgn(p: BProc, e: CgNode) =
  if e[0].kind == cnkLocal and sfGoto in p.body[e[0].local].flags:
    genLineDir(p, e)
    genGotoVar(p, e[1])
  elif optFieldCheck in p.options and isDiscriminantField(e[0]):
    genLineDir(p, e)
    asgnFieldDiscriminant(p, e)
  else:
    let le = e[0]
    let ri = e[1]
    var a: TLoc
    initLoc(a, locNone, le, OnUnknown)
    a.flags.incl {lfEnforceDeref, lfPrepareForMutation, lfWantLvalue}
    expr(p, le, a)
    a.flags.excl {lfPrepareForMutation, lfWantLvalue}
    assert(a.t != nil)
    genLineDir(p, ri)
    loadInto(p, le, ri, a)

proc genStmts(p: BProc, t: CgNode) =
  var a: TLoc

  let isPush = p.config.hasHint(rsemExtendedContext)
  if isPush: pushInfoContext(p.config, t.info)
  expr(p, t, a)
  if isPush: popInfoContext(p.config)
  internalAssert p.config, a.k in {locNone, locTemp, locLocalVar, locExpr}
