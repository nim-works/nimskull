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
     getCalleeMagic(p.env, ri[0]) == mNone:
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

proc genGotoVar(p: BProc; value: CgNode) =
  case value.kind
  of cnkIntLit, cnkUIntLit:
    lineF(p, cpsStmts, "goto NIMSTATE_$#;$n", [value.intVal.rope])
  else:
    localReport(p.config, value.info, reportSem rsemExpectedLiteralForGoto)

proc genBracedInit(p: BProc, n: CgNode; optionalType: PType): Rope

proc genSingleVar(p: BProc, vn, value: CgNode) =
  ## Generates and emits the C code for the definition statement of a local.
  let v = vn.local

  if sfGoto in p.body[v].flags:
    # translate 'var state {.goto.} = X' into 'goto LX':
    genGotoVar(p, value)
    return

  assignLocalVar(p, vn)
  # default-initialize the local if no initial value is supplied. Automatic
  # initialization is also ommitted when the `value` expression is a
  # potentially-raising call, because for a definition like:
  #   def x = f()
  # no except or finally section that is entered when `f` raises can access
  # `x`, nor can any code that the except or finally section can exit into,
  # meaning that it's safe to potentially leave `x` uninitialized if `f`
  # raises
  initLocalVar(p, v, immediateAsgn = (value.kind != cnkEmpty))

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
  startBlock(p)

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

proc exit(n: CgNode): CgNode =
  # XXX: exists as a convenience for overflow check, index check, etc.
  #      code gen. Should be removed once those are fully lowered prior
  #      to code generation
  case n.kind
  of cnkCheckedCall: n[^1]
  else:              nil

proc raiseInstr(p: BProc, n: CgNode): Rope =
  if n != nil:
    case n.kind
    of cnkLabel:
      # easy case, simply goto the target:
      result = ropecg(p.module, "goto $1;", [n.label])
    of cnkTargetList:
      for i in 0..<n.len:
        if n[i].kind in {cnkLabel, cnkResume}:
          let label = toCLabel(n[i], p.specifier)
          result = ropecg(p.module, "goto $1$2_;", [label])
          break
    else:
      unreachable(n.kind)
  else:
    result = ""

proc raiseExit(p: BProc, n: CgNode) =
  assert p.config.exc == excGoto
  if nimErrorFlagDisabled notin p.flags:
    p.flags.incl nimErrorFlagAccessed
    lineCg(p, cpsStmts, "if (NIM_UNLIKELY(*nimErr_)) $1$n",
           [raiseInstr(p, n)])

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
      lineCg(p, cpsStmts, "#raiseException2((#Exception*)$1, $2, $3, $4);$n",
          [e,
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
        let sym = it.field
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

proc genAsgn(p: BProc, e: CgNode) =
  if e[0].kind == cnkLocal and sfGoto in p.body[e[0].local].flags:
    genLineDir(p, e)
    genGotoVar(p, e[1])
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

proc genStmt(p: BProc, t: CgNode) =
  var a: TLoc

  let isPush = p.config.hasHint(rsemExtendedContext)
  if isPush: pushInfoContext(p.config, t.info)
  expr(p, t, a)
  if isPush: popInfoContext(p.config)
  internalAssert p.config, a.k in {locNone, locTemp, locLocalVar, locExpr}

proc gen(p: BProc, code: openArray[CInstr], stmts: CgNode) =
  ## Generates and emits the C code for `code` and `stmts`. This is the main
  ## driver of C code generation.
  var pos = 0
  while pos < code.len:
    let it = code[pos]
    case it.op
    of opLabel:
      lineCg(p, cpsStmts, "$1:;$n", [it.label])
    of opJump:
      lineCg(p, cpsStmts, "goto $1;$n", [it.label])
    of opDispJump:
      # must only be part of a dispatcher
      unreachable()
    of opSetTarget:
      lineCg(p, cpsStmts, "Target$1_ = $2;$n", [$it.discr, it.value])
    of opDispatcher:
      lineF(p, cpsLocals, "NU8 Target$1_;$N", [$it.discr])
      lineF(p, cpsStmts, "switch (Target$1_) {$n", [$it.discr])
      for i in 0..<it.value:
        inc pos
        lineCg(p, cpsStmts, "case $1: goto $2;$n", [i, code[pos].label])

      # help the C compiler a bit by making the case statement exhaustive
      if hasAssume in CC[p.config.cCompiler].props:
        lineF(p, cpsStmts, "default: __assume(0);$n", [])
      # TODO: use ``__builtin_unreachable();`` for compiler supporting the
      #       GCC built-ins
      lineF(p, cpsStmts, "}$n", [])
    of opBackup:
      if nimErrorFlagDisabled notin p.flags:
        p.flags.incl nimErrorFlagAccessed
        lineCg(p, cpsStmts, "NI32 oldNimErrFin$1_ = *nimErr_; *nimErr_ = NIM_FALSE;$n",
              [$it.local])
    of opRestore:
      if nimErrorFlagDisabled notin p.flags:
        p.flags.incl nimErrorFlagAccessed
        lineCg(p, cpsStmts, "*nimErr_ = oldNimErrFin$1_;$n", [$it.local])
    of opErrJump:
      if nimErrorFlagDisabled notin p.flags:
        p.flags.incl nimErrorFlagAccessed
        lineCg(p, cpsStmts, "if (NIM_UNLIKELY(*nimErr_)) goto $1;$n",
               [it.label])

    of opStmts:
      # generate the code for all statements; no label specifier is set
      p.specifier = none CLabelSpecifier
      for i in it.stmts.items:
        genStmt(p, stmts[i])
    of opStmt:
      p.specifier = some it.specifier
      genStmt(p, stmts[it.stmt])

    inc pos

proc genStmts*(p: BProc, n: CgNode) =
  ## Generates and emits the C code for the statement list node `n`, which
  ## makes up the full body of the procedure. This is the external entry
  ## point into the C code generator.
  assert n.kind == cnkStmtList
  gen(p, toInstrList(n, true), n)
