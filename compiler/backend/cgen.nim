#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the C code generator.

import
  std/[
    hashes,
    intsets,
    os,
    math,
    tables,
    sets
  ],
  compiler/ast/[
    ast_query,
    ast_types,
    ast_idgen,
    astalgo,
    trees,
    idents,
    types,
    typesrenderer,
    wordrecg,
    renderer,
    lineinfos,
    astmsgs,
    ndi
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/utils/[
    containers,
    platform,
    nversion,
    bitsets,
    ropes,
    pathutils,
    idioms,
    int128
  ],
  compiler/sem/[
    rodutils,
    lowerings,
  ],
  compiler/backend/[
    compat,
    extccomp,
    ccgutils,
    cgendata,
    cgir
  ],
  compiler/plugins/[
  ]

# xxx: reports are a code smell meaning data types are misplaced...
#      like the backend report sem errors.
from compiler/ast/reports_sem import SemReport,
  reportSem,
  reportStr,
  reportSym,
  reportTyp
from compiler/ast/report_enums import ReportKind

from compiler/sem/passes import moduleHasChanged # XXX: leftover dependency
import std/strutils except `%`, addf # collides with ropes.`%`

from compiler/ast/ast import newType, rawAddSon

when defined(nimCompilerStacktraceHints):
  import compiler/utils/debugutils

when options.hasTinyCBackend:
  import backend/tccgen

const NonMagics* = {mNewString, mNewStringOfCap, mNewSeq, mSetLengthSeq,
                    mAppendSeqElem, mEnumToStr, mExit, mParseBiggestFloat,
                    mDotDot, mEqCString, mIsolate}
  ## magics that are treated like normal procedures by the code generator.
  ## This set only applies when using the new runtime.

const
  sfTopLevel* = sfMainModule
    ## the procedure contains top-level code, which currently affects how
    ## emit, asm, and error handling works

template `[]=`(x: CgNode, i: Natural, n: CgNode) =
  x.kids[i] = n

proc findPendingModule(m: BModule, s: PSym): BModule =
  let ms = s.itemId.module  #getModule(s)
  result = m.g.modules[ms]

proc initLoc(result: var TLoc, k: TLocKind, lode: CgNode, s: TStorageLoc) =
  result.k = k
  result.storage = s
  result.lode = lode
  result.r = ""
  result.flags = {}

func initLoc(kind: TLocKind, n: CgNode, name: sink string, storage: TStorageLoc): TLoc =
  TLoc(k: kind, storage: storage, lode: n, r: name, flags: {})

proc fillLoc(a: var TLoc, k: TLocKind, lode: CgNode, r: Rope, s: TStorageLoc) =
  ## fills the loc if it is not already initialized
  if a.k == locNone:
    a.k = k
    a.lode = lode
    a.storage = s
    if a.r == "": a.r = r

proc t(a: TLoc): PType {.inline.} =
  if a.lode.kind == cnkSym:
    result = a.lode.sym.typ
  else:
    result = a.lode.typ

proc lodeTyp(t: PType): CgNode =
  result = newNode(cnkEmpty, typ = t)

proc isSimpleConst(typ: PType): bool =
  let t = skipTypes(typ, abstractVar)
  result = t.kind notin
      {tyTuple, tyObject, tyArray, tySet, tySequence} and not
      (t.kind == tyProc and t.callConv == ccClosure)

proc useHeader(m: BModule, sym: PSym) =
  if exfHeader in sym.extFlags:
    let str = getStr(m.g.graph.getLib(sym.annex).path)
    m.includeHeader(str)

proc cgsym(m: BModule, name: string): Rope

proc getCFile(m: BModule): AbsoluteFile

import macros

proc cgFormatValue(result: var string; value: string) =
  result.add value

proc cgFormatValue(result: var string; value: BiggestInt) =
  result.addInt value

proc cgFormatValue(result: var string; value: Int128) =
  result.addInt128 value

# TODO: please document
macro ropecg(m: BModule, frmt: static[FormatStr], args: untyped): Rope =
  args.expectKind nnkBracket
  # echo "ropecg ", newLit(frmt).repr, ", ", args.repr
  var i = 0
  result = nnkStmtListExpr.newTree()

  result.add quote do:
    assert `m` != nil

  let resVar = genSym(nskVar, "res")
  # during `koch boot` the median of all generates strings from this
  # macro is around 40 bytes in length.
  result.add newVarStmt(resVar, newCall(bindSym"newStringOfCap", newLit(80)))
  let formatValue = bindSym"cgFormatValue"

  var num = 0
  var strLit = ""

  template flushStrLit() =
    if strLit != "":
      result.add newCall(ident "add", resVar, newLit(strLit))
      strLit.setLen 0

  while i < frmt.len:
    if frmt[i] == '$':
      inc(i)                  # skip '$'
      case frmt[i]
      of '$':
        strLit.add '$'
        inc(i)
      of '#':
        flushStrLit()
        inc(i)
        result.add newCall(formatValue, resVar, args[num])
        inc(num)
      of '0'..'9':
        var j = 0
        while true:
          j = (j * 10) + ord(frmt[i]) - ord('0')
          inc(i)
          if i >= frmt.len or not (frmt[i] in {'0'..'9'}): break
        num = j
        if j > args.len:
          error("ropes: invalid format string " & newLit(frmt).repr & " args.len: " & $args.len)

        flushStrLit()
        result.add newCall(formatValue, resVar, args[j-1])
      of 'n':
        flushStrLit()
        result.add quote do:
          if optLineDir notin `m`.config.options:
            `resVar`.add("\L")
        inc(i)
      of 'N':
        strLit.add "\L"
        inc(i)
      else:
        error("ropes: invalid format string $" & frmt[i])
    elif frmt[i] == '#' and frmt[i+1] in IdentStartChars:
      inc(i)
      var j = i
      while frmt[j] in IdentChars: inc(j)
      var ident = newLit(substr(frmt, i, j-1))
      i = j
      flushStrLit()
      result.add newCall(formatValue, resVar, newCall(ident"cgsym", m, ident))
    elif frmt[i] == '#' and frmt[i+1] == '$':
      inc(i, 2)
      var j = 0
      while frmt[i] in Digits:
        j = (j * 10) + ord(frmt[i]) - ord('0')
        inc(i)
      let ident = args[j-1]
      flushStrLit()
      result.add newCall(formatValue, resVar, newCall(ident"cgsym", m, ident))
    var start = i
    while i < frmt.len:
      if frmt[i] != '$' and frmt[i] != '#': inc(i)
      else: break
    if i - 1 >= start:
      strLit.add(substr(frmt, start, i - 1))

  flushStrLit()
  result.add newCall(ident"rope", resVar)

proc indentLine(p: BProc, r: Rope): Rope =
  result = r
  for i in 0..<p.blocks.len:
    prepend(result, "\t".rope)

template appcg(m: BModule, c: var Rope, frmt: FormatStr,
           args: untyped) =
  c.add(ropecg(m, frmt, args))

template appcg(m: BModule, sec: TCFileSection, frmt: FormatStr,
           args: untyped) =
  m.s[sec].add(ropecg(m, frmt, args))

template appcg(p: BProc, sec: TCProcSection, frmt: FormatStr,
           args: untyped) =
  p.s(sec).add(ropecg(p.module, frmt, args))

template line(p: BProc, sec: TCProcSection, r: Rope) =
  p.s(sec).add(indentLine(p, r))

template line(p: BProc, sec: TCProcSection, r: string) =
  p.s(sec).add(indentLine(p, r.rope))

template lineF(p: BProc, sec: TCProcSection, frmt: FormatStr,
              args: untyped) =
  p.s(sec).add(indentLine(p, frmt % args))

template lineCg(p: BProc, sec: TCProcSection, frmt: FormatStr,
               args: untyped) =
  p.s(sec).add(indentLine(p, ropecg(p.module, frmt, args)))

template linefmt(p: BProc, sec: TCProcSection, frmt: FormatStr,
             args: untyped) =
  p.s(sec).add(indentLine(p, ropecg(p.module, frmt, args)))

proc safeLineNm(info: TLineInfo): int =
  result = toLinenumber(info)
  if result < 0: result = 0 # negative numbers are not allowed in #line

proc genCLineDir(r: var Rope, filename: string, line: int; conf: ConfigRef) =
  assert line >= 0
  if optLineDir in conf.options and line > 0:
    r.addf("$N#line $2 $1$N",
        [rope(makeSingleLineCString(filename)), rope(line)])

proc genCLineDir(r: var Rope, info: TLineInfo; conf: ConfigRef) =
  genCLineDir(r, toFullPath(conf, info), info.safeLineNm, conf)

proc freshLineInfo(p: BProc; info: TLineInfo): bool =
  if p.lastLineInfo.line != info.line or
     p.lastLineInfo.fileIndex != info.fileIndex:
    p.lastLineInfo.line = info.line
    p.lastLineInfo.fileIndex = info.fileIndex
    result = true

proc genLineDir(p: BProc, t: CgNode) =
  let line = t.info.safeLineNm

  if optEmbedOrigSrc in p.config.globalOptions:
    p.s(cpsStmts).add(~"//" & sourceLine(p.config, t.info) & "\L")
  genCLineDir(p.s(cpsStmts), toFullPath(p.config, t.info), line, p.config)
  if ({optLineTrace, optStackTrace} * p.options == {optLineTrace, optStackTrace}) and
      (p.prc == nil or sfPure notin p.prc.flags) and t.info.fileIndex != InvalidFileIdx:
    if freshLineInfo(p, t.info):
      linefmt(p, cpsStmts, "nimln_($1, $2);$n",
              [line, quotedFilename(p.config, t.info)])

proc accessThreadLocalVar(p: BProc, s: PSym)
proc emulatedThreadVars*(conf: ConfigRef): bool {.inline.}
proc useProc(m: BModule, prc: PSym)
proc raiseInstr(p: BProc): Rope

proc getTempName(m: BModule): Rope =
  result = m.tmpBase & rope(m.labels)
  inc m.labels

proc rdLoc(a: TLoc): Rope =
  # 'read' location (deref if indirect)
  result = a.r
  if lfIndirect in a.flags: result = "(*$1)" % [result]

proc lenExpr(p: BProc; a: TLoc): Rope =
  result = rdLoc(a) & ".len"

proc dataField(p: BProc): Rope =
  result = rope".p->data"

include ccgliterals
include ccgtypes

# ------------------------------ Manager of temporaries ------------------

func mapTypeChooser(p: BProc, n: CgNode): TSymKind =
  case n.kind
  of cnkSym:
    n.sym.kind
  of cnkLocal:
    if n.local == resultId:
      skResult
    elif p.locals[n.local].k == locParam:
      skParam
    else:
      skVar
  else:
    skVar

func mapTypeChooser(a: TLoc): TSymKind =
  let n = a.lode
  case n.kind
  of cnkSym:
    n.sym.kind
  of cnkLocal:
    if n.local == resultId:
      skResult
    elif a.k == locParam:
      skParam
    else:
      skVar
  else:
    skVar

proc addrLoc(conf: ConfigRef; a: TLoc): Rope =
  result = a.r
  if lfIndirect notin a.flags and mapType(conf, a.t, mapTypeChooser(a)) != ctArray:
    result = "(&" & result & ")"

proc byRefLoc(p: BProc; a: TLoc): Rope =
  result = a.r
  if lfIndirect notin a.flags and mapType(p.config, a.t, mapTypeChooser(a)) != ctArray:
    result = "(&" & result & ")"

proc rdCharLoc(a: TLoc): Rope =
  # read a location that may need a char-cast:
  result = rdLoc(a)
  if skipTypes(a.t, abstractRange).kind == tyChar:
    result = "((NU8)($1))" % [result]

proc genObjConstr(p: BProc, e: CgNode, d: var TLoc)
proc rawConstExpr(p: BProc, n: CgNode; d: var TLoc)
proc genAssignment(p: BProc, dest, src: TLoc)

type
  ObjConstrMode = enum
    constructObj,
    constructRefObj

proc genObjectInitHeader(p: BProc, section: TCProcSection, t: PType, r: Rope,
                         info: TLineInfo) =
  var
    r = r
    s = skipTypes(t, abstractInst)

  while s.kind == tyObject and s[0] != nil:
    r.add(".Sup")
    s = skipTypes(s[0], skipPtrs)

  linefmt(p, section, "$1.m_type = $2;$n",
          [r, genTypeInfoV2(p.module, t, info)])

proc genObjectInit(p: BProc, section: TCProcSection, t: PType, a: TLoc,
                   mode: ObjConstrMode) =

  proc defaultValueExpr(p: BProc, t: PType, info: TLineInfo): TLoc =
    ## Sets up and returns a loc storing the expression representing the
    ## default value for `t`.
    let kind =
      case t.skipTypes(abstractInst).kind
      of tyObject: cnkObjConstr
      of tyTuple:  cnkTupleConstr
      of tyArray:  cnkArrayConstr
      else:
        unreachable("cannot have embedded type fields")

    rawConstExpr(p, newExpr(kind, info, t), result)

  case analyseObjectWithTypeField(t)
  of frNone:
    discard
  of frHeader:
    var r = rdLoc(a)
    if mode == constructRefObj: r = "(*$1)" % [r]
    genObjectInitHeader(p, section, t, r, a.lode.info)
  of frEmbedded:
      if mode == constructRefObj:
        let objType = t.skipTypes(abstractInst+{tyRef})
        let tmp = defaultValueExpr(p, objType, a.lode.info)
        linefmt(p, cpsStmts,
            "#nimCopyMem((void*)$1, (NIM_CONST void*)&$2, sizeof($3));$n",
            [rdLoc(a), rdLoc(tmp), getTypeDesc(p.module, objType, mapTypeChooser(a))])
      else:
        let tmp = defaultValueExpr(p, t, a.lode.info)
        genAssignment(p, a, tmp)

proc constructLoc(p: BProc, loc: var TLoc; doInitObj = true) =
  let kind = mapTypeChooser(loc)
  case mapType(p.config, loc.t, kind)
  of ctChar, ctBool, ctInt, ctInt8, ctInt16, ctInt32, ctInt64,
     ctFloat, ctFloat32, ctFloat64,
     ctUInt, ctUInt8, ctUInt16, ctUInt32, ctUInt64:
    # numeric type
    linefmt(p, cpsStmts, "$1 = 0;$n", [rdLoc(loc)])
  of ctPtrToArray, ctPtr, ctCString, ctProc:
    # a simple ptr-like type -> assign nil
    linefmt(p, cpsStmts, "$1 = NIM_NIL;$n", [rdLoc(loc)])
  of ctNimStr, ctNimSeq:
    linefmt(p, cpsStmts, "$1.len = 0; $1.p = NIM_NIL;$n", [rdLoc(loc)])
  of ctArray, ctStruct, ctNimOpenArray:
    linefmt(p, cpsStmts, "#nimZeroMem((void*)$1, sizeof($2));$n",
            [addrLoc(p.config, loc), getTypeDesc(p.module, loc.t, kind)])

    if doInitObj:
      genObjectInit(p, cpsStmts, loc.t, loc, constructObj)
  of ctVoid:
    unreachable()

proc resetLoc(p: BProc, loc: var TLoc; doInitObj = true) =
  # resetting the loc is achieved by constructing a new empty value inside
  # it
  constructLoc(p, loc, doInitObj)

proc initLocalVar(p: BProc, v: LocalId, immediateAsgn: bool) =
  if sfNoInit notin p.body[v].flags:
    # we know it is a local variable and thus on the stack!
    # If ``not immediateAsgn`` it is not initialized in a binding like
    # ``var v = X`` and thus we need to init it.
    # If ``v`` contains a GC-ref we may pass it to ``unsureAsgnRef`` somehow
    # which requires initialization. However this can really only happen if
    # ``var v = X()`` gets transformed into ``X(&v)``.
    # Nowadays the logic in ccgcalls deals with this case however.
    if not immediateAsgn:
      constructLoc(p, p.locals[v])

proc getTemp(p: BProc, t: PType, result: var TLoc) =
  inc(p.labels)
  result.r = "T" & rope(p.labels) & "_"
  linefmt(p, cpsLocals, "$1 $2;$n", [getTypeDesc(p.module, t, skVar), result.r])
  result.k = locTemp
  result.lode = lodeTyp t
  result.storage = OnStack
  result.flags = {}
  when false:
    # XXX Introduce a compiler switch in order to detect these easily.
    if getSize(p.config, t) > 1024 * 1024:
      if p.prc != nil:
        echo "ENORMOUS TEMPORARY! ", p.config $ p.prc.info
      else:
        echo "ENORMOUS TEMPORARY! ", p.config $ p.lastLineInfo
      writeStackTrace()

proc getIntTemp(p: BProc, result: var TLoc) =
  inc(p.labels)
  result.r = "T" & rope(p.labels) & "_"
  linefmt(p, cpsLocals, "NI $1;$n", [result.r])
  result.k = locTemp
  result.storage = OnStack
  result.lode = lodeTyp getSysType(p.module.g.graph, unknownLineInfo, tyInt)
  result.flags = {}

proc localVarDecl(p: BProc; n: CgNode, decl: Local): Rope =
  let loc = initLoc(locLocalVar, n, mangleLocalName(p, decl.name, n.local),
                    OnStack)

  if decl.alignment > 0:
    result.addf("NIM_ALIGN($1) ", [$decl.alignment])

  result.add getTypeDesc(p.module, decl.typ, skVar)
  if true:
    if sfRegister in decl.flags: result.add(" register")
    if sfVolatile in decl.flags: result.add(" volatile")
    if sfNoalias  in decl.flags: result.add(" NIM_NOALIAS")
    result.add(" ")
    result.add(loc.r)

  p.locals[n.local] = loc

proc assignLocalVar(p: BProc, n: CgNode) =
  let nl = if optLineDir in p.config.options: "" else: "\L"
  let decl = localVarDecl(p, n, p.body[n.local]) & ";" & nl
  line(p, cpsLocals, decl)

include ccgthreadvars

proc varInDynamicLib(m: BModule, sym: PSym)

proc fillGlobalLoc*(m: BModule, s: PSym) =
  let n = CgNode(kind: cnkSym, info: s.info, typ: s.typ, sym: s)
  m.globals.put(s, initLoc(locGlobalVar, n, mangleName(m.g.graph, s), OnHeap))

proc defineGlobalVar*(m: BModule, s: PSym) =
  fillGlobalLoc(m, s)

  assert s.id notin m.declaredThings
  assert findPendingModule(m, s) == m, "not the attached-to module"

  if exfDynamicLib in s.extFlags:
    incl(m.declaredThings, s.id)
    varInDynamicLib(m, s)
  else:
    useHeader(m, s)
    if exfNoDecl notin s.extFlags:
      incl(m.declaredThings, s.id)
      var decl = ""
      var td = getTypeDesc(m, m.globals[s].t, skVar)
      if true:
        if s.kind in {skLet, skVar, skField, skForVar} and s.alignment > 0:
          decl.addf "NIM_ALIGN($1) ", [rope(s.alignment)]
        if sfImportc in s.flags: decl.add("extern ")
        elif exfExportLib in s.extFlags: decl.add("N_LIB_EXPORT_VAR ")
        else: decl.add("N_LIB_PRIVATE ")
        decl.add(td)
        if sfRegister in s.flags: decl.add(" register")
        if sfVolatile in s.flags: decl.add(" volatile")
        if sfNoalias in s.flags: decl.add(" NIM_NOALIAS")
        decl.addf(" $1;$n", [m.globals[s].r])

      m.s[cfsVars].add(decl)

proc fillProcLoc*(m: BModule; sym: PSym) =
  if sym.locId == 0:
    m.procs.put(sym): ProcLoc(sym: sym, name: mangleName(m.g.graph, sym))

proc getLabel(p: BProc): TLabel =
  inc(p.labels)
  result = "LA" & rope(p.labels) & "_"

proc fixLabel(p: BProc, labl: TLabel) =
  lineF(p, cpsStmts, "$1: ;$n", [labl])

proc genVarPrototype*(m: BModule, n: CgNode)
proc genProcPrototype*(m: BModule, sym: PSym)
proc genStmts*(p: BProc, t: CgNode)
proc expr(p: BProc, n: CgNode, d: var TLoc)
proc putLocIntoDest(p: BProc, d: var TLoc, s: TLoc)
proc intLiteral(i: BiggestInt): Rope
proc intLiteral(p: BProc, i: Int128, ty: PType): Rope
proc genLiteral(p: BProc, n: CgNode): Rope
proc raiseExit(p: BProc)

proc initLocExpr(p: BProc, e: CgNode, result: var TLoc) =
  initLoc(result, locNone, e, OnUnknown)
  expr(p, e, result)

proc initLocExpr(p: BProc, e: CgNode, result: var TLoc, flags: set[LocFlag]) =
  initLoc(result, locNone, e, OnUnknown)
  result.flags = flags
  expr(p, e, result)

proc initLocExprSingleUse(p: BProc, e: CgNode, result: var TLoc) =
  initLoc(result, locNone, e, OnUnknown)
  if e.kind == cnkCall and getCalleeMagic(e[0]) == mNone:
    # We cannot check for tfNoSideEffect here because of mutable parameters.
    discard "bug #8202; enforce evaluation order for nested calls"
    # We may need to consider that 'f(g())' cannot be rewritten to 'tmp = g(); f(tmp)'
    # if 'tmp' lacks a move/assignment operator.
  else:
    result.flags.incl lfSingleUse
  expr(p, e, result)

include ccgcalls, "ccgstmts.nim"

proc initFrame(p: BProc, procname, filename: Rope): Rope =
  const frameDefines = """
  $1  define nimfr_(proc, file) \
      TFrame FR_; \
      FR_.procname = proc; FR_.filename = file; FR_.line = 0; FR_.len = 0; #nimFrame(&FR_);

  $1  define nimfrs_(proc, file, slots, length) \
      struct {TFrame* prev;NCSTRING procname;NI line;NCSTRING filename; NI len; VarSlot s[slots];} FR_; \
      FR_.procname = proc; FR_.filename = file; FR_.line = 0; FR_.len = length; #nimFrame((TFrame*)&FR_);

  $1  define nimln_(n, file) \
      FR_.line = n; FR_.filename = file;
  """
  if p.module.s[cfsFrameDefines].len == 0:
    appcg(p.module, p.module.s[cfsFrameDefines], frameDefines, ["#"])

  discard cgsym(p.module, "nimFrame")
  result = ropecg(p.module, "\tnimfr_($1, $2);$n", [procname, filename])

proc deinitFrame(p: BProc): Rope =
  result = ropecg(p.module, "\t#popFrame();$n", [])

include ccgexprs

proc mangleDynLibProc(sym: PSym): Rope =
  if sfCompilerProc in sym.flags:
    # NOTE: sym.extname is the external name!
    result = rope(sym.name.s)
  else:
    result = rope(strutils.`%`("Dl_$1_", $sym.id))

proc fillDynlibProcLoc(m: BModule, s: PSym) =
  if s.locId == 0:
    # XXX: a dynlib procedure is not really a ``locProc``, but rather a
    #      global variable
    m.procs.put(s): ProcLoc(name: mangleDynLibProc(s), sym: s)

proc symInDynamicLib*(m: BModule, sym: PSym) =
  fillDynlibProcLoc(m, sym)
  m.s[cfsVars].addf("$2 $1;$n",
                    [m.procs[sym].name, getTypeDesc(m, sym.typ, skVar)])


proc varInDynamicLib(m: BModule, sym: PSym) =
  let tmp = mangleDynLibProc(sym)
  incl(m.globals[sym].flags, lfIndirect)
  m.globals[sym].r = tmp  # from now on we only need the internal name
  m.s[cfsVars].addf("$2* $1;$n",
      [tmp, getTypeDesc(m, sym.typ, skVar)])

proc cgsym(m: BModule, name: string): Rope =
  let sym = magicsys.getCompilerProc(m.g.graph, name)
  if sym != nil:
    case sym.kind
    of skProc, skFunc, skMethod, skConverter, skIterator:
      useProc(m, sym)
      m.extra.add(sym) # notify the caller about the dependency
    of skVar, skResult, skLet: genVarPrototype(m, newSymNode sym)
    of skType: discard getTypeDesc(m, sym.typ)
    else: internalError(m.config, "cgsym: " & name & ": " & $sym.kind)
  else:
    # we used to exclude the system module from this check, but for DLL
    # generation support this sloppyness leads to hard to detect bugs, so
    # we're picky here for the system module too:
    localReport(m.config, reportStr(rsemSystemNeeds, name))

  result = sym.extname

proc generateHeaders(m: BModule) =
  m.s[cfsHeaders].add("\L#include \"nimbase.h\"\L")

  for it in m.headerFiles:
    if it[0] == '#':
      m.s[cfsHeaders].add(rope(it.replace('`', '"') & "\L"))
    elif it[0] notin {'"', '<'}:
      m.s[cfsHeaders].addf("#include \"$1\"$N", [rope(it)])
    else:
      m.s[cfsHeaders].addf("#include $1$N", [rope(it)])
  m.s[cfsHeaders].add("""#undef LANGUAGE_C
#undef MIPSEB
#undef MIPSEL
#undef PPC
#undef R3000
#undef R4000
#undef i386
#undef linux
#undef mips
#undef near
#undef far
#undef powerpc
#undef unix
""")

proc closureSetup(p: BProc, prc: PSym) =
  if prc.typ.callConv != ccClosure: return
  # prc.ast[paramsPos].last contains the type we're after:
  var ls = lastSon(prc.ast[paramsPos])
  p.config.internalAssert(ls.kind == nkSym, prc.info, "closure generation failed")
  var env = ls.sym.position + 1 # parameters start at ID 1

  let n = newLocalRef(LocalId(env), ls.info, ls.typ)
  assignLocalVar(p, n)
  # generate cast assignment:
  linefmt(p, cpsStmts, "$1 = ($2) ClE_0;$n",
          [rdLoc(p.locals[n.local]), getTypeDesc(p.module, ls.typ)])

func containsResult(n: CgNode): bool =
  result = false
  case n.kind
  of cnkAtoms - {cnkLocal}:
    discard "ignore"
  of cnkLocal:
    if n.local == resultId:
      result = true
  of cnkWithOperand:
    result = containsResult(n.operand)
  of cnkWithItems:
    for i in 0..<n.len:
      if containsResult(n[i]): return true

type
  InitResultEnum = enum Unknown, InitSkippable, InitRequired

proc allPathsAsgnResult(n: CgNode): InitResultEnum =
  # Exceptions coming from calls don't have not be considered here:
  #
  # proc bar(): string = raise newException(...)
  #
  # proc foo(): string =
  #   # optimized out: 'reset(result)'
  #   result = bar()
  #
  # try:
  #   a = foo()
  # except:
  #   echo "a was not written to"
  #
  template allPathsInBranch(it) =
    let a = allPathsAsgnResult(it)
    case a
    of InitRequired: return InitRequired
    of InitSkippable: discard
    of Unknown:
      # sticky, but can be overwritten by InitRequired:
      result = Unknown

  result = Unknown
  case n.kind
  of cnkStmtList:
    for it in n:
      result = allPathsAsgnResult(it)
      if result != Unknown: return result
  of cnkAsgn, cnkFastAsgn:
    if n[0].kind == cnkLocal and n[0].local == resultId:
      if not containsResult(n[1]): result = InitSkippable
      else: result = InitRequired
    elif containsResult(n):
      result = InitRequired
  of cnkReturnStmt:
      if true:
        # This is a bare `return` statement, if `result` was not initialized
        # anywhere else (or if we're not sure about this) let's require it to be
        # initialized. This avoids cases like #9286 where this heuristic lead to
        # wrong code being generated.
        result = InitRequired
  of cnkIfStmt:
    result = InitSkippable
    # the condition must not use 'result':
    if containsResult(n[0]):
      return InitRequired

    allPathsInBranch(n[1])
    # if the 'if' statement is not exhaustive and yet it touched 'result'
    # in some way, say Unknown.
    result = Unknown
  of cnkCaseStmt:
    if containsResult(n[0]): return InitRequired
    result = InitSkippable
    var exhaustive = skipTypes(n[0].typ,
        abstractVarRange-{tyTypeDesc}).kind notin {tyFloat..tyFloat64, tyString}
    for i in 1..<n.len:
      let it = n[i]
      allPathsInBranch(it.lastSon)
      if not isOfBranch(it): exhaustive = true
    if not exhaustive: result = Unknown
  of cnkRepeatStmt:
    result = allPathsAsgnResult(n[0])
    # a 'repeat' loop is always executed at least once
    if result == InitSkippable: result = Unknown
  of cnkAtoms - {cnkLocal, cnkReturnStmt}:
    result = Unknown
  of cnkLocal:
    # some path reads from 'result' before it was written to!
    if n.local == resultId: result = InitRequired
  of cnkTryStmt:
    # We need to watch out for the following problem:
    # try:
    #   result = stuffThatRaises()
    # except:
    #   discard "result was not set"
    #
    # So ... even if the assignment to 'result' is the very first
    # assignment this is not good enough! The only pattern we allow for
    # is 'finally: result = x'
    result = InitSkippable
    allPathsInBranch(n[0])
    for i in 1..<n.len:
      if n[i].kind == cnkFinally:
        result = allPathsAsgnResult(n[i].lastSon)
      else:
        allPathsInBranch(n[i].lastSon)
  of cnkWithOperand:
    allPathsInBranch(n.operand)
  else:
    for it in n.items:
      allPathsInBranch(it)

proc isNoReturn(m: BModule; s: PSym): bool {.inline.} =
  sfNoReturn in s.flags and m.config.exc != excGoto

proc startProc*(m: BModule, prc: PSym; procBody: sink Body): BProc =
  var p = newProc(prc, m)
  p.body = procBody
  assert(prc.ast != nil)
  fillProcLoc(m, prc) # ensure that a loc exists
  if m.procs[prc].params.len == 0:
    # if a prototype was emitted, the parameter list already exists
    m.procs[prc].params = prepareParameters(m, prc.typ)

  synchronize(p.locals, p.body.locals)

  if sfPure notin prc.flags and prc.typ[0] != nil:
    m.config.internalAssert(resultPos < prc.ast.len, prc.info, "proc has no result symbol")
    let
      res = resultId
      resNode = newLocalRef(res, prc.info, prc.typ[0])
    if not isInvalidReturnType(m.config, prc.typ[0]):
      # declare the result symbol:
      assignLocalVar(p, resNode)
      if sfNoInit notin prc.flags:
        initLocalVar(p, res, immediateAsgn=false)
    else:
      p.locals[res] = initResultParamLoc(p.config, resNode)
      scopeMangledParam(p, p.body[res].name)
      # We simplify 'unsureAsgn(result, nil); unsureAsgn(result, x)'
      # to 'unsureAsgn(result, x)'
      # Sketch why this is correct: If 'result' points to a stack location
      # the 'unsureAsgn' is a nop. If it points to a global variable the
      # global is either 'nil' or points to valid memory and so the RC operation
      # succeeds without touching not-initialized memory.
      if sfNoInit in prc.flags: discard
      elif p.body.code != nil and
           allPathsAsgnResult(p.body.code) == InitSkippable: discard
      else:
        resetLoc(p, p.locals[res])
      if skipTypes(resNode.typ, abstractInst).kind == tyArray:
        #incl(res.locFlags, lfIndirect)
        p.locals[res].storage = OnUnknown

  # setup the locs for the parameters:
  for i in 1..<m.procs[prc].params.len:
    p.locals[LocalId(i)] = m.procs[prc].params[i]

  # for now, we treat all compilerprocs as being able to run in a boot
  # environment where the error flag is not yet accessible. This is not quite
  # correct, and a dedicated facility for designating runtime procedures as
  # usable in a boot environment is eventually required
  # The ``threadProcWrapper`` is special-cased to have the flag disabled too,
  # as thread-local storage might not have been set up when the flag is first
  # queried. Making it a compilerproc is not possible, due to it being a
  # generic routine
  if sfCompilerProc in prc.flags or (prc.name.s == "threadProcWrapper" and
     sfSystemModule in getModule(prc).flags):
    p.flags.incl nimErrorFlagDisabled

  for i in 1..<prc.typ.n.len:
    let param = prc.typ.n[i].sym
    if p.params[i].k == locNone: continue
    scopeMangledParam(p, param.name)
  closureSetup(p, prc)

  if sfPure notin prc.flags and optStackTrace in prc.options:
    # HACK: we need to raise the dependencies here already. Doing so when
    #       finishing the procedure would be too late in the case of
    #       procedures for which code is generated incrementally
    discard cgsym(p.module, "nimFrame")
    discard cgsym(p.module, "popFrame")

  result = p

proc finishProc*(p: BProc, prc: PSym): string =
  if {nimErrorFlagAccessed, nimErrorFlagDeclared} * p.flags == {nimErrorFlagAccessed}:
    p.flags.incl nimErrorFlagDeclared
    p.blocks[0].sections[cpsLocals].add(ropecg(p.module, "NIM_BOOL* nimErr_;$n", []))
    p.blocks[0].sections[cpsInit].add(ropecg(p.module, "nimErr_ = #nimErrorFlag();$n", []))

  var
    header = genProcHeader(p.module, prc, p.params)
    returnStmt = ""

  if sfPure notin prc.flags and not isInvalidReturnType(p.config, prc.typ[0]):
    returnStmt = ropecg(p.module, "\treturn $1;$n",
                        [rdLoc(p.locals[resultId])])

  var generatedProc: Rope
  generatedProc.genCLineDir prc.info, p.config
  if isNoReturn(p.module, prc):
    if hasDeclspec in extccomp.CC[p.config.cCompiler].props:
      header = "__declspec(noreturn) " & header
  if sfPure in prc.flags:
    if hasDeclspec in extccomp.CC[p.config.cCompiler].props:
      header = "__declspec(naked) " & header
    generatedProc.add ropecg(p.module, "$1 {$n$2$3$4}$N$N",
                         [header, p.s(cpsLocals), p.s(cpsInit), p.s(cpsStmts)])
  else:
    generatedProc.add ropecg(p.module, "$1 {$n", [header])
    if optStackTrace in prc.options:
      generatedProc.add(p.s(cpsLocals))
      var procname = makeCString(prc.name.s)
      generatedProc.add(initFrame(p, procname, quotedFilename(p.config, prc.info)))
    else:
      generatedProc.add(p.s(cpsLocals))
    if optProfiler in prc.options:
      # invoke at proc entry for recursion:
      appcg(p, cpsInit, "\t#nimProfile();$n", [])
    # this pair of {} was added because C++ is stricter with its control flow
    # integrity checks, leaving them in
    if beforeRetNeeded in p.flags: generatedProc.add("{")
    generatedProc.add(p.s(cpsInit))
    generatedProc.add(p.s(cpsStmts))
    if beforeRetNeeded in p.flags: generatedProc.add(~"\t}BeforeRet_: ;$n")

    if sfTopLevel in prc.flags:
      generatedProc.add ropecg(p.module, "\t#nimTestErrorFlag();$n", [])

    if optStackTrace in prc.options: generatedProc.add(deinitFrame(p))
    generatedProc.add(returnStmt)
    generatedProc.add(~"}$N")

  result = generatedProc

proc genProc*(m: BModule, prc: PSym, procBody: sink Body): Rope =
  ## Generates the code for the procedure `prc`, where `procBody` is the code
  ## of the body with all applicable lowerings and transformation applied.
  let p = startProc(m, prc, procBody)
  genStmts(p, p.body.code)
  result = finishProc(p, prc)

proc genPartial*(p: BProc, n: CgNode) =
  ## Generates the C code for `n` and appends the result to `p`. This
  ## is intended for CG IR that wasn't already available when calling
  ## `startProc`.
  synchronize(p.locals, p.body.locals)
  genStmts(p, n)

proc genProcPrototype(m: BModule, sym: PSym) =
  useHeader(m, sym)
  if exfNoDecl in sym.extFlags: return
  if exfDynamicLib in sym.extFlags:
    if sym.itemId.module != m.module.position and
        not containsOrIncl(m.declaredThings, sym.id):
      m.s[cfsVars].add(ropecg(m, "$1 $2 $3;$n",
                        ["extern",
                        getTypeDesc(m, sym.typ), m.procs[sym].name]))

  elif not containsOrIncl(m.declaredProtos, sym.id):
    if m.procs[sym].params.len == 0:
      m.procs[sym].params = prepareParameters(m, sym.typ)
    var header = genProcHeader(m, sym, m.procs[sym].params)
    block:
      if isNoReturn(m, sym) and hasDeclspec in extccomp.CC[m.config.cCompiler].props:
        header = "__declspec(noreturn) " & header
      if sfPure in sym.flags and hasAttribute in CC[m.config.cCompiler].props:
        header.add(" __attribute__((naked))")
      if isNoReturn(m, sym) and hasAttribute in CC[m.config.cCompiler].props:
        header.add(" __attribute__((noreturn))")
    m.s[cfsProcHeaders].add(ropecg(m, "$1;$N", [header]))

proc useProc(m: BModule, prc: PSym) =
  if exfImportCompilerProc in prc.extFlags:
    fillProcLoc(m, prc)
    useHeader(m, prc)
    # dependency to a compilerproc:
    discard cgsym(m, prc.name.s)
  elif exfDynamicLib in prc.extFlags:
    # a special name is used for run-time imported procedures:
    fillDynlibProcLoc(m, prc)
    genProcPrototype(m, prc)
  elif exfNoDecl in prc.extFlags or sfImportc in prc.flags:
    fillProcLoc(m, prc)
    genProcPrototype(m, prc)
  else:
    # mangle based on the attached-to module
    fillProcLoc(findPendingModule(m, prc), prc)
    genProcPrototype(m, prc)

proc genVarPrototype(m: BModule, n: CgNode) =
  #assert(sfGlobal in sym.flags)
  let sym = n.sym
  useHeader(m, sym)
  if (exfNoDecl in sym.extFlags) or contains(m.declaredThings, sym.id):
    return
  if sym.owner.id != m.module.id:
    # else we already have the symbol generated!
    if sfThread in sym.flags:
      declareThreadVar(m, sym, true)
    else:
      incl(m.declaredThings, sym.id)
      if sym.kind in {skLet, skVar, skField, skForVar} and sym.alignment > 0:
        m.s[cfsVars].addf "NIM_ALIGN($1) ", [rope(sym.alignment)]
      m.s[cfsVars].add("extern ")
      m.s[cfsVars].add(getTypeDesc(m, sym.typ, skVar))
      if exfDynamicLib in sym.extFlags: m.s[cfsVars].add("*")
      if sfRegister in sym.flags: m.s[cfsVars].add(" register")
      if sfVolatile in sym.flags: m.s[cfsVars].add(" volatile")
      if sfNoalias in sym.flags: m.s[cfsVars].add(" NIM_NOALIAS")
      m.s[cfsVars].addf(" $1;$n", [m.globals[sym].r])

proc addNimDefines(result: var Rope; conf: ConfigRef) {.inline.} =
  result.addf("#define NIM_INTBITS $1\L", [
    platform.CPU[conf.target.targetCPU].intSize.rope])
  if conf.isDefined("nimEmulateOverflowChecks"):
    result.add("#define NIM_EmulateOverflowChecks\L")

proc headerTop(): Rope =
  result = "/* Generated by Nim Compiler v$1 */$N" % [rope(VersionAsString)]

proc getCopyright(conf: ConfigRef; cfile: Cfile): Rope =
  result = headerTop()
  if optCompileOnly notin conf.globalOptions:
    result.add ("/* Compiled for: $1, $2, $3 */$N" &
        "/* Command for C compiler:$n   $4 */$N") %
        [rope(platform.OS[conf.target.targetOS].name),
        rope(platform.CPU[conf.target.targetCPU].name),
        rope(extccomp.CC[conf.cCompiler].name),
        rope(getCompileCFileCmd(conf, cfile))]

proc getFileHeader(conf: ConfigRef; cfile: Cfile): Rope =
  result = getCopyright(conf, cfile)
  addNimDefines(result, conf)

proc getSomeNameForModule(m: PSym): Rope =
  assert m.kind == skModule
  assert m.owner.kind == skPackage
  if {sfSystemModule, sfMainModule} * m.flags == {}:
    result = m.owner.name.s.mangle.rope
    result.add "_"
  result.add m.name.s.mangle

proc getSomeInitName(m: BModule, suffix: string): Rope =
  result = getSomeNameForModule(m.module)
  result.add suffix

proc getInitName*(m: BModule): Rope =
  if sfMainModule in m.module.flags:
    # generate constant name for main module, for "easy" debugging.
    result = rope"NimMainModule"
  else:
    result = getSomeInitName(m, "Init000")

proc getDatInitName*(m: BModule): Rope = getSomeInitName(m, "DatInit000")

proc genMainProc*(m: BModule, body: Rope) =
  ## this function is called in cgenWriteModules after all modules are closed,
  ## it means raising dependency on the symbols is too late as it will not propagate
  ## into other modules, only simple rope manipulations are allowed

  const
    # not a big deal if we always compile these 3 global vars... makes the HCR code easier
    PosixCmdLine =
      "N_LIB_PRIVATE int cmdCount;$N" &
      "N_LIB_PRIVATE char** cmdLine;$N" &
      "N_LIB_PRIVATE char** gEnv;$N"

    PreMainBody = "$N" &
      PosixCmdLine

    MainProcs =
      "\tNimMain();$N"

    MainProcsWithResult =
      MainProcs & ("\treturn $1nim_program_result;$N")

    NimMainProc =
      "N_CDECL(void, NimMain)(void) {$N" &
        "$1$N" &
      "}$N$N"

    NimMainBody = NimMainProc

    PosixCMain =
      "int main(int argc, char** args, char** env) {$N" &
        "\tcmdLine = args;$N" &
        "\tcmdCount = argc;$N" &
        "\tgEnv = env;$N" &
        MainProcsWithResult &
      "}$N$N"

    StandaloneCMain =
      "int main(void) {$N" &
        MainProcs &
        "\treturn 0;$N" &
      "}$N$N"

    WinNimMain = NimMainBody

    WinCMain = "N_STDCALL(int, WinMain)(HINSTANCE hCurInstance, $N" &
      "                        HINSTANCE hPrevInstance, $N" &
      "                        LPSTR lpCmdLine, int nCmdShow) {$N" &
      MainProcsWithResult & "}$N$N"

    WinNimDllMain = "N_LIB_EXPORT " & NimMainProc

    WinCDllMain =
      "BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fwdreason, $N" &
      "                    LPVOID lpvReserved) {$N" &
      "\tif(fwdreason == DLL_PROCESS_ATTACH) {$N" & MainProcs & "}$N" &
      "\treturn 1;$N}$N$N"

    PosixNimDllMain = WinNimDllMain

    PosixCDllMain =
      "N_LIB_PRIVATE void NIM_POSIX_INIT NimMainInit(void) {$N" &
        MainProcs &
      "}$N$N"

  if m.config.target.targetOS == osWindows and
      m.config.globalOptions * {optGenGuiApp, optGenDynLib} != {}:
    m.includeHeader("<windows.h>")

  appcg(m, m.s[cfsProcs], PreMainBody, [])

  if m.config.target.targetOS == osWindows and
      m.config.globalOptions * {optGenGuiApp, optGenDynLib} != {}:
    if optGenGuiApp in m.config.globalOptions:
      const nimMain = WinNimMain
      appcg(m, m.s[cfsProcs], nimMain,
        [body])
    else:
      const nimMain = WinNimDllMain
      appcg(m, m.s[cfsProcs], nimMain,
        [body])
  elif optGenDynLib in m.config.globalOptions:
    const nimMain = PosixNimDllMain
    appcg(m, m.s[cfsProcs], nimMain,
        [body])
  elif m.config.target.targetOS == osStandalone:
    const nimMain = NimMainBody
    appcg(m, m.s[cfsProcs], nimMain,
        [body])
  else:
    const nimMain = NimMainBody
    appcg(m, m.s[cfsProcs], nimMain,
        [body])

  if optNoMain notin m.config.globalOptions:
    if m.config.target.targetOS == osWindows and
        m.config.globalOptions * {optGenGuiApp, optGenDynLib} != {}:
      if optGenGuiApp in m.config.globalOptions:
        const otherMain = WinCMain
        appcg(m, m.s[cfsProcs], otherMain, [""])
      else:
        const otherMain = WinCDllMain
        appcg(m, m.s[cfsProcs], otherMain, [])
    elif optGenDynLib in m.config.globalOptions:
      const otherMain = PosixCDllMain
      appcg(m, m.s[cfsProcs], otherMain, [])
    elif m.config.target.targetOS == osStandalone:
      const otherMain = StandaloneCMain
      appcg(m, m.s[cfsProcs], otherMain, [])
    else:
      const otherMain = PosixCMain
      appcg(m, m.s[cfsProcs], otherMain, [""])

proc genDatInitCode*(m: BModule): bool =
  ## this function is called after all modules are closed,
  ## it means raising dependency on the symbols is too late as it will not propagate
  ## into other modules, only simple rope manipulations are allowed

  var moduleDatInitRequired = false

  var prc = "$1 N_NIMCALL(void, $2)(void) {$N" %
    [rope("N_LIB_PRIVATE"), getDatInitName(m)]

  # we don't want to break into such init code - could happen if a line
  # directive from a function written by the user spills after itself
  genCLineDir(prc, "generated_not_to_break_here", 999999, m.config)

  if m.typeNodes > 0:
    # emit a definition for the node storage, if used
    appcg(m, m.s[cfsTypeInit1], "static #TNimNode $1[$2];$n",
          [m.typeNodesName, m.typeNodes])

  for i in cfsTypeInit1..cfsDebugInit:
    if m.s[i].len != 0:
      moduleDatInitRequired = true
      prc.add(m.s[i])

  # setting up the TLS must happen before initializing the ``system`` module,
  # so we emit the call at the end of the data-init procedure:
  if sfSystemModule in m.module.flags and
     emulatedThreadVars(m.config) and m.config.target.targetOS != osStandalone:
    moduleDatInitRequired = true
    prc.addf("\tinitThreadVarsEmulation();$N", [])

  prc.addf("}$N$N", [])

  if moduleDatInitRequired:
    m.s[cfsDatInitProc].add(prc)
    #rememberFlag(m.g.graph, m.module, HasDatInitProc)

  result = moduleDatInitRequired

proc genModule(m: BModule, cfile: Cfile): Rope =
  var moduleIsEmpty = true

  result = getFileHeader(m.config, cfile)

  generateThreadLocalStorage(m)
  generateHeaders(m)
  result.add(m.s[cfsHeaders])
  if m.s[cfsFrameDefines].len > 0:
    result.add(m.s[cfsFrameDefines])
  else:
    result.add("#define nimfr_(x, y)\n#define nimln_(x, y)\n")

  for i in cfsForwardTypes..cfsProcs:
    if m.s[i].len > 0:
      moduleIsEmpty = false
      result.add(m.s[i])

  if m.s[cfsInitProc].len > 0:
    moduleIsEmpty = false
    result.add(m.s[cfsInitProc])
  if m.s[cfsDatInitProc].len > 0:
    moduleIsEmpty = false
    result.add(m.s[cfsDatInitProc])

  if moduleIsEmpty:
    result = ""

proc rawNewModule*(g: BModuleList; module: PSym, filename: AbsoluteFile): BModule =
  new(result)
  result.g = g
  result.tmpBase = rope("TM" & $hashOwner(module) & "_")
  result.headerFiles = @[]
  result.declaredThings = initIntSet()
  result.declaredProtos = initIntSet()
  result.cfilename = filename
  result.filename = filename
  result.typeCache = initTable[SigHash, Rope]()
  result.forwTypeCache = initTable[SigHash, Rope]()
  result.module = module
  result.typeInfoMarker = initTable[SigHash, Rope]()
  result.sigConflicts = initCountTable[SigHash]()
  result.dataCache = initTable[ConstrTree, int]()
  result.typeStack = @[]
  result.typeNodesName = getTempName(result)
  # no line tracing for the init sections of the system module so that we
  # don't generate a TFrame which can confuse the stack bottom initialization:
  if sfSystemModule in module.flags:
    incl result.flags, preventStackTrace
  let ndiName = if optCDebug in g.config.globalOptions: changeFileExt(completeCfilePath(g.config, filename), "ndi")
                else: AbsoluteFile""
  open(result.ndi, ndiName, g.config)

proc rawNewModule(g: BModuleList; module: PSym; conf: ConfigRef): BModule =
  result = rawNewModule(g, module, AbsoluteFile toFullPath(conf, module.position.FileIndex))

proc newModule*(g: BModuleList; module: PSym; conf: ConfigRef): BModule =
  # we should create only one cgen module for each module sym
  result = rawNewModule(g, module, conf)
  if module.position >= g.modules.len:
    setLen(g.modules, module.position + 1)
  #growCache g.modules, module.position
  g.modules[module.position] = result

proc writeHeader(m: BModule) =
  var result = headerTop()
  var guard = "__$1__" % [m.filename.splitFile.name.rope]
  result.addf("#ifndef $1$n#define $1$n", [guard])
  addNimDefines(result, m.config)
  generateHeaders(m)

  generateThreadLocalStorage(m)
  for i in cfsHeaders..cfsProcs:
    result.add(m.s[i])
  result.add(m.s[cfsInitProc])

  if optGenDynLib in m.config.globalOptions:
    result.add("N_LIB_IMPORT ")
  result.addf("N_CDECL(void, NimMain)(void);$n", [])
  result.addf("#endif /* $1 */$n", [guard])
  if not writeRope(result, m.filename):
    localReport(m.config, reportStr(rsemCannotOpenFile, m.filename.string))

proc getCFile(m: BModule): AbsoluteFile =
  result = changeFileExt(completeCfilePath(m.config, withPackageName(m.config, m.cfilename)), ".nim.c")

proc shouldRecompile(m: BModule; code: Rope, cfile: Cfile): bool =
  if optForceFullMake notin m.config.globalOptions:
    if not moduleHasChanged(m.g.graph, m.module):
      result = false
    elif not equalsFile(code, cfile.cname):
      when false:
        #m.config.symbolFiles == readOnlySf: #isDefined(m.config, "nimdiff"):
        if fileExists(cfile.cname):
          copyFile(cfile.cname.string, cfile.cname.string & ".backup")
          echo "diff ", cfile.cname.string, ".backup ", cfile.cname.string
        else:
          echo "new file ", cfile.cname.string
      if not writeRope(code, cfile.cname):
        localReport(m.config, reportStr(rsemCannotOpenFile, cfile.cname.string))

      result = true
    elif fileExists(cfile.obj) and os.fileNewer(cfile.obj.string, cfile.cname.string):
      result = false
    else:
      result = true
  else:
    if not writeRope(code, cfile.cname):
      localReport(m.config, reportStr(rsemCannotOpenFile, cfile.cname.string))

    result = true

proc finalizeModule*(m: BModule) =
  finishTypeDescriptions(m)

proc finalizeMainModule*(m: BModule) =
  generateThreadVarsSize(m) # TODO: not the job of the code generator

proc writeModule(m: BModule) =
  template onExit() = close(m.ndi, m.config)
  let cfile = getCFile(m)
  var cf = Cfile(nimname: m.module.name.s, cname: cfile,
                  obj: completeCfilePath(m.config, toObjFile(m.config, cfile)), flags: {})
  var code = genModule(m, cf)
  if code != "" or m.config.symbolFiles != disabledSf:
    when hasTinyCBackend:
      if m.config.cmd == cmdTcc:
        tccgen.compileCCode($code, m.config)
        onExit()
        return

    if not shouldRecompile(m, code, cf):
      cf.flags = {CfileFlag.Cached}

    addFileToCompile(m.config, cf)
  onExit()

proc cgenWriteModules*(backend: RootRef, config: ConfigRef) =
  let g = BModuleList(backend)
  g.config = config

  # note: we don't need to call ``writeModule`` in module closed order
  # anymore, as the procedure now does what its name implies: writing the
  # module to disk. However, it also queues the C file for compilation,
  # so we still keep the behaviour for now
  for m in cgenModules(g):
    m.writeModule()
  writeMapping(config, g.mapping)
  if g.generatedHeader != nil: writeHeader(g.generatedHeader)
