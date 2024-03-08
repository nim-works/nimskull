#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This is the JavaScript code generator.

discard """
The JS code generator contains only 1 trick:

Trick 1
-------
Some locations (for example 'var int') require "fat pointers" (`etyBaseIndex`)
which are pairs (array, index). The derefence operation is then 'array[index]'.
Check `mapType` for the details.

"""

import
  system/[
    formatfloat
  ],
  std/[
    sets,
    math,
    tables,
    intsets
  ],
  compiler/ast/[
    ast_idgen,
    ast_query,
    ast_types,
    idents,
    types,
    renderer,
    lineinfos
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/mir/[
    mirenv,
    mirtrees
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/utils/[
    containers,
    idioms,
    int128,
    nversion,
    ropes
  ],
  compiler/backend/[
    cgir,
    compat,
    jsflow
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import reportSem,
  reportStr,
  reportSym
from compiler/ast/reports_backend import BackendReport
from compiler/ast/report_enums import ReportKind

import std/strutils except addf # clashes with ropes.addf

type
  TJSGen = object
    idgen*: IdGenerator
    module*: PSym
    graph: ModuleGraph
    config: ConfigRef

  BModule* = ref TJSGen
  TJSTypeKind = enum       # necessary JS "types"
    etyNone,                  # no type
    etyNull,                  # null type
    etyProc,                  # proc type
    etyBool,                  # bool type
    etySeq,                   # Nim seq or string type
    etyInt,                   # JavaScript's int
    etyFloat,                 # JavaScript's float
    etyString,                # JavaScript's string
    etyObject,                # JavaScript's reference to an object
    etyBaseIndex              # base + index needed
  TResKind = enum
    resNone,                  # not set
    resExpr,                  # is some complex expression
    resVal,                   # is a temporary/value/l-value
    resCallee                 # expression is callee
  TCompRes = object
    kind: TResKind
    typ: TJSTypeKind
    res: Rope               # result part; index if this is an
                             # (address, index)-tuple
    address: Rope           # address of an (address, index)-tuple
    tmpLoc: Rope            # tmp var which stores the (address, index)
                            # pair to prevent multiple evals.
                            # the tmp is initialized upon evaling the
                            # address.
                            # might be nil.

  PGlobals* = ref object
    typeInfo, constants*, code*: Rope
    typeInfoGenerated: IntSet
    dataGenerated: IntSet
    unique: int    # for temp identifier generation

    names: Table[int, string]
      ## maps a symbol IDs to the symbol's JavaScript name
    procs: SeqMap[ProcedureId, string]
      ## the JavaScript name for each procedure
    consts: SeqMap[ConstId, Loc]
    globals: SeqMap[GlobalId, Loc]

    env*: MirEnv
      ## the project-wide MIR environment

  StorageFlag = enum
    stfIndirect ## the value is stored in a single-item array
    stfBoxed    ## the pointer value is stored as a two-element array

  StorageFlags = set[StorageFlag]

  Loc = object
    ## Information about the JavaScript variable corresponding to a
    ## |NimSkull| location.
    ##
    ## Future direction: ``Loc`` is intended to also be used for constants
    ## and globals.
    name: string
    typ: PType
    storage: StorageFlags

  PProc* = ref TProc
  TProc* = object
    prc: PSym
    fullBody*: Body
      ## the procedure's full body
    defs, body: Rope
    options: TOptions
    module: BModule
    g: PGlobals
    unique: int    # for temp identifier generation
    extraIndent: int

    locals: OrdinalSeq[LocalId, Loc]
      ## stores all relevant code generator state for the procedure's
      ## locals
    addrTaken: PackedSet[LocalId]
      ## locals that have their address taken at some point

const
  sfModuleInit* = sfMainModule
    ## the procedure is the 'init' procedure of a module

  NonMagics* = { mAbsI, mDotDot, mParseBiggestFloat, mExit }
    ## magics that are treated like normal procedures by the code
    ## generator

template `$`(x: BlockId): string =
  $ord(x)

template isFilled(x: string): bool =
  x.len != 0

# forward declarations:
proc setupLocalLoc(p: PProc, id: LocalId, kind: TSymKind; name = "")

func analyseIfAddressTaken(n: CgNode, addrTaken: var PackedSet[LocalId]) =
  ## Recursively traverses the tree `n` and includes the IDs of all locals
  ## that have their address taken in `addrTaken`.
  proc skipAllConv(n: CgNode): CgNode {.nimcall.} =
    var n {.cursor.} = n
    while true:
      case n.kind
      of cnkLvalueConv, cnkObjDownConv, cnkObjUpConv:
        n = n.operand
      of cnkStmtListExpr:
        n = n[^1]
      else:
        break

    result = n

  case n.kind
  of cnkHiddenAddr, cnkAddr:
    let x = skipAllConv(n.operand)
    # we're only interested in locals. Since the code generator ignores the
    # ``tyLent`` type, creating a ``lent T`` view doesn't count as taking the
    # address
    if x.kind == cnkLocal and n.typ.kind != tyLent:
      addrTaken.incl x.local
    else:
      # the operand expression must still be anaylsed
      analyseIfAddressTaken(n.operand, addrTaken)
  of cnkAtoms:
    discard "ignore"
  of cnkWithOperand - {cnkHiddenAddr, cnkAddr}:
    analyseIfAddressTaken(n.operand, addrTaken)
  of cnkWithItems:
    for it in n.items:
      analyseIfAddressTaken(it, addrTaken)

template config*(p: PProc): ConfigRef = p.module.config
template env*(p: PProc): untyped = p.g.env

proc indentLine(p: PProc, r: Rope): Rope =
  for i in 0..<p.extraIndent:
    result.add "  "
  result.add r

template line(p: PProc, added: string) =
  p.body.add(indentLine(p, rope(added)))

template line(p: PProc, added: Rope) =
  p.body.add(indentLine(p, added))

template lineF(p: PProc, frmt: FormatStr, args: varargs[Rope]) =
  p.body.add(indentLine(p, ropes.`%`(frmt, args)))

template nested(p, body) =
  inc p.extraIndent
  body
  dec p.extraIndent

template startBlock(p: PProc, frmt: FormatStr, args: varargs[Rope]) =
  lineF(p, frmt, args)
  inc p.extraIndent

template endBlock(p: PProc) =
  dec p.extraIndent
  lineF(p, "}$n", [])

proc newGlobals*(): PGlobals =
  new(result)
  result.typeInfoGenerated = initIntSet()

proc rdLoc(a: TCompRes): Rope {.inline.} =
  if a.typ != etyBaseIndex:
    result = a.res
  else:
    result = "$1[$2]" % [a.address, a.res]

proc newProc(globals: PGlobals, module: BModule, prc: PSym,
             options: TOptions): PProc =
  result = PProc(
    options: options,
    module: module,
    prc: prc,
    g: globals,
    extraIndent: int(prc != nil))

proc newInitProc(globals: PGlobals, module: BModule): PProc =
  result = newProc(globals, module, nil, {})

proc mapType(typ: PType; indirect = false): TJSTypeKind =
  ## Returns how a value of `typ` is represented on the JavaScript side.
  ## `indirect` indicates whether a pointer-like type was followed.
  let t = skipTypes(typ, abstractInst)
  case t.kind
  of tyVar, tyRef, tyPtr:
    # for efficiency, don't use a base+index pair for values that have
    # reference semantics already (e.g. JavaScript objects). However,
    # double indirections (e.g. ``ptr ptr object``) do need the base+index
    # pair.
    if not indirect and mapType(t.lastSon, true) == etyObject:
      result = etyObject
    else:
      result = etyBaseIndex
  of tyPointer:
    # treat a tyPointer like a typed pointer to an array of bytes
    result = etyBaseIndex
  of tyRange, tyDistinct, tyOrdinal, tyProxy, tyLent:
    # tyLent is no-op as JS has pass-by-reference semantics
    result = mapType(t[0], indirect)
  of tyInt..tyInt64, tyUInt..tyUInt64, tyChar: result = etyInt
  of tyBool: result = etyBool
  of tyFloat..tyFloat64: result = etyFloat
  of tySet: result = etyObject # map a set to a table
  of tyString, tySequence: result = etySeq
  of tyObject, tyArray, tyTuple, tyOpenArray, tyVarargs, tyUncheckedArray:
    result = etyObject
  of tyNil: result = etyNull
  of tyGenericParam, tyGenericBody, tyGenericInvocation,
     tyNone, tyFromExpr, tyForward, tyEmpty,
     tyUntyped, tyTyped, tyTypeDesc, tyBuiltInTypeClass, tyCompositeTypeClass,
     tyAnd, tyOr, tyNot, tyAnything, tyVoid:
    result = etyNone
  of tyGenericInst, tyInferred, tyAlias, tyUserTypeClass, tyUserTypeClassInst,
     tySink, tyEnum:
    result = mapType(typ.lastSon, indirect)
  of tyStatic:
    if t.n != nil: result = mapType(t.lastSon, indirect)
    else: result = etyNone
  of tyProc: result = etyProc
  of tyCstring: result = etyString

func mangleJs(name: string): string =
  ## Mangles the given `name` and returns the result. The mangling is required
  ## in order to ensure that NimSkull identifiers map to valid JavaScript
  ## identifiers
  result = newStringOfCap(name.len)
  for c in name.items:
    case c
    of 'A'..'Z', 'a'..'z', '_', '0'..'9':
      result.add c
    else:
      result.add("HEX" & toHex(ord(c), 2))

func mangleName(m: BModule, s: PSym): Rope =
  proc validJsName(name: string): bool =
    result = true
    const reservedWords = ["abstract", "await", "boolean", "break", "byte",
      "case", "catch", "char", "class", "const", "continue", "debugger",
      "default", "delete", "do", "double", "else", "enum", "export", "extends",
      "false", "final", "finally", "float", "for", "function", "goto", "if",
      "implements", "import", "in", "instanceof", "int", "interface", "let",
      "long", "native", "new", "null", "package", "private", "protected",
      "public", "return", "short", "static", "super", "switch", "synchronized",
      "this", "throw", "throws", "transient", "true", "try", "typeof", "var",
      "void", "volatile", "while", "with", "yield"]
    case name
    of reservedWords:
      return false
    else:
      discard
    if name[0] in {'0'..'9'}: return false
    for chr in name:
      if chr notin {'A'..'Z','a'..'z','_','$','0'..'9'}:
        return false
  result = s.extname
  if result == "":
    if s.kind == skField and s.name.s.validJsName:
      result = rope(s.name.s)
    else:
      result = mangleJs(s.name.s)
    # From ES5 on reserved words can be used as object field names
    if s.kind != skField:
      result.add("_")
      result.add(rope(s.id))

func mangleName(loc: Local, id: LocalId): string =
  if loc.name.isNil:
    # locals that don't have a name (e.g., temporaries) just use the
    # ID
    result = "_" & $id.int
  else:
    result = mangleJs(loc.name.s)
    result.add "_"
    result.addInt id.int

proc ensureMangledName(p: PProc, s: PSym): lent string =
  ## Looks up and returns the mangled name for the non-local symbol
  ## `s`, generating and caching the mangled name if it hasn't been
  ## already.
  let n = addr p.g.names.mgetOrPut(s.id, "")
  if n[].len == 0:
    # the mangled named hasn't been generated yet
    n[] = mangleName(p.module, s)

  result = p.g.names[s.id]

proc ensureMangledName(p: PProc, id: ProcedureId): lent string =
  if id notin p.g.procs:
    # the mangled name hasn't been generated yet
    p.g.procs[id] = mangleName(p.module, p.env[id])

  result = p.g.procs[id]

proc escapeJSString(s: string): string =
  result = newStringOfCap(s.len + s.len shr 2)
  result.add("\"")
  for c in items(s):
    case c
    of '\l': result.add("\\n")
    of '\r': result.add("\\r")
    of '\t': result.add("\\t")
    of '\b': result.add("\\b")
    of '\a': result.add("\\a")
    of '\e': result.add("\\e")
    of '\v': result.add("\\v")
    of '\\': result.add("\\\\")
    of '\"': result.add("\\\"")
    else: result.add(c)
  result.add("\"")

proc makeJSString(s: string, escapeNonAscii = true): Rope =
  if escapeNonAscii:
    result = strutils.escape(s).rope
  else:
    result = escapeJSString(s).rope

include jstypes

proc gen(p: PProc, n: CgNode, r: var TCompRes)
proc genStmt(p: PProc, n: CgNode)

proc gen(p: PProc, n: CgNode): TCompRes {.inline.} =
  ## Convenience procedure that returns instead of requiring a ``var``
  ## parameter.
  gen(p, n, result)

proc useMagic(p: PProc, name: string) =
  if name.len == 0: return
  var s = magicsys.getCompilerProc(p.module.graph, name)
  if s != nil:
    discard p.env.procedures.add(s)
  else:
    if p.prc != nil:
      globalReport(p.config, p.prc.info, reportStr(rsemSystemNeeds, name))

    else:
      localReport(p.config, reportStr(rsemSystemNeeds, name))

proc getTemp(p: PProc, defineInLocals: bool = true): Rope =
  inc(p.unique)
  result = "Temporary$1" % [rope(p.unique)]
  if defineInLocals:
    p.defs.add(p.indentLine("var $1;$n" % [result]))

type
  TMagicOps = array[mAddI..mUnaryPlusF64, string]

const # magic checked op
  jsMagics: TMagicOps = [
    mAddI: "addInt",
    mSubI: "subInt",
    mMulI: "mulInt",
    mDivI: "divInt",
    mModI: "modInt",
    mSucc: "addInt",
    mPred: "subInt",
    mAddF64: "",
    mSubF64: "",
    mMulF64: "",
    mDivF64: "",
    mShrI: "",
    mShlI: "",
    mAshrI: "",
    mBitandI: "",
    mBitorI: "",
    mBitxorI: "",
    mMinI: "nimMin",
    mMaxI: "nimMax",
    mAddU: "",
    mSubU: "",
    mMulU: "",
    mDivU: "",
    mModU: "",
    mEqI: "",
    mLeI: "",
    mLtI: "",
    mEqF64: "",
    mLeF64: "",
    mLtF64: "",
    mLeU: "",
    mLtU: "",
    mEqEnum: "",
    mLeEnum: "",
    mLtEnum: "",
    mEqCh: "",
    mLeCh: "",
    mLtCh: "",
    mEqB: "",
    mLeB: "",
    mLtB: "",
    mEqRef: "",
    mLePtr: "",
    mLtPtr: "",
    mXor: "",
    mEqCString: "",
    mEqProc: "",
    mUnaryMinusI: "negInt",
    mUnaryMinusI64: "negInt64",
    mAbsI: "absInt",
    mNot: "",
    mUnaryPlusI: "",
    mBitnotI: "",
    mUnaryPlusF64: ""]

template binaryExpr(p: PProc, n: CgNode, r: var TCompRes, magic, frmt: string) =
  # $1 and $2 in the `frmt` string bind to lhs and rhs of the expr,
  # $3 or $4 are legacy substitutions that also bind to the lhs and rhs
  var x, y: TCompRes
  useMagic(p, magic)
  gen(p, n[1], x)
  gen(p, n[2], y)

  var
    a = x.rdLoc
    b = y.rdLoc

  r.res = frmt % [a, b, a, b]
  r.kind = resExpr

template binaryExpr(p: PProc, a, b: CgNode, r: var TCompRes, frmt: string) =
  var x, y: TCompRes
  gen(p, a, x)
  gen(p, b, y)
  r.res = frmt % [x.rdLoc, y.rdLoc]

proc unsignedTrimmerJS(size: BiggestInt): Rope =
  case size
  of 1: rope"& 0xff"
  of 2: rope"& 0xffff"
  of 4: rope">>> 0"
  else: rope""


template unsignedTrimmer(size: BiggestInt): Rope =
  size.unsignedTrimmerJS

proc binaryUintExpr(p: PProc, n: CgNode, r: var TCompRes, op: string,
                    reassign: static[bool] = false) =
  var x, y: TCompRes
  gen(p, n[1], x)
  gen(p, n[2], y)
  let trimmer = unsignedTrimmer(n[1].typ.skipTypes(abstractRange).size)
  when reassign:
    r.res = "$1 = (($1 $2 $3) $4)" % [x.rdLoc, rope op, y.rdLoc, trimmer]
  else:
    r.res = "(($1 $2 $3) $4)" % [x.rdLoc, rope op, y.rdLoc, trimmer]
  r.kind = resExpr

template unaryExpr(p: PProc, n: CgNode, r: var TCompRes, magic, frmt: string) =
  # $1 binds to n[1]
  useMagic(p, magic)
  gen(p, n[1], r)
  r.res = frmt % [r.rdLoc]
  r.kind = resExpr

proc arithAux(p: PProc, n: CgNode, r: var TCompRes, op: TMagic) =
  var
    x, y: TCompRes
    xLoc,yLoc: Rope

  useMagic(p, jsMagics[op])
  if numArgs(n) == 2:
    gen(p, n[1], x)
    gen(p, n[2], y)
    xLoc = x.rdLoc
    yLoc = y.rdLoc
  else:
    gen(p, n[1], r)
    xLoc = r.rdLoc

  template applyFormat(frmt) =
    r.res = frmt % [xLoc, yLoc]

  case op:
  of mAddI: applyFormat("addInt($1, $2)")
  of mSubI: applyFormat("subInt($1, $2)")
  of mMulI: applyFormat("mulInt($1, $2)")
  of mDivI: applyFormat("divInt($1, $2)")
  of mModI: applyFormat("modInt($1, $2)")
  of mSucc: applyFormat("addInt($1, $2)")
  of mPred: applyFormat("subInt($1, $2)")
  of mAddF64: applyFormat("($1 + $2)")
  of mSubF64: applyFormat("($1 - $2)")
  of mMulF64: applyFormat("($1 * $2)")
  of mDivF64: applyFormat("($1 / $2)")
  of mShrI: applyFormat("")
  of mShlI:
    if n[1].typ.size <= 4:
      applyFormat("($1 << $2)")
    else:
      applyFormat("($1 * Math.pow(2, $2))")
  of mAshrI:
    if n[2].typ.size <= 4:
      applyFormat("($1 >> $2)")
    else:
      applyFormat("Math.floor($1 / Math.pow(2, $2))")
  of mBitandI: applyFormat("($1 & $2)")
  of mBitorI: applyFormat("($1 | $2)")
  of mBitxorI: applyFormat("($1 ^ $2)")
  of mMinI: applyFormat("nimMin($1, $2)")
  of mMaxI: applyFormat("nimMax($1, $2)")
  of mAddU: applyFormat("")
  of mSubU: applyFormat("")
  of mMulU: applyFormat("")
  of mDivU: applyFormat("")
  of mModU: applyFormat("($1 % $2)")
  of mEqI: applyFormat("($1 == $2)")
  of mLeI: applyFormat("($1 <= $2)")
  of mLtI: applyFormat("($1 < $2)")
  of mEqF64: applyFormat("($1 == $2)")
  of mLeF64: applyFormat("($1 <= $2)")
  of mLtF64: applyFormat("($1 < $2)")
  of mLeU: applyFormat("($1 <= $2)")
  of mLtU: applyFormat("($1 < $2)")
  of mEqEnum: applyFormat("($1 == $2)")
  of mLeEnum: applyFormat("($1 <= $2)")
  of mLtEnum: applyFormat("($1 < $2)")
  of mEqCh: applyFormat("($1 == $2)")
  of mLeCh: applyFormat("($1 <= $2)")
  of mLtCh: applyFormat("($1 < $2)")
  of mEqB: applyFormat("($1 == $2)")
  of mLeB: applyFormat("($1 <= $2)")
  of mLtB: applyFormat("($1 < $2)")
  of mEqRef: applyFormat("($1 == $2)")
  of mLePtr: applyFormat("($1 <= $2)")
  of mLtPtr: applyFormat("($1 < $2)")
  of mXor: applyFormat("($1 != $2)")
  of mEqCString: applyFormat("($1 == $2)")
  of mEqProc: applyFormat("($1 == $2)")
  of mUnaryMinusI: applyFormat("negInt($1)")
  of mUnaryMinusI64: applyFormat("negInt64($1)")
  of mAbsI: applyFormat("absInt($1)")
  of mNot: applyFormat("!($1)")
  of mUnaryPlusI: applyFormat("+($1)")
  of mBitnotI: applyFormat("~($1)")
  of mUnaryPlusF64: applyFormat("+($1)")
  else:
    unreachable(op)

proc arith(p: PProc, n: CgNode, r: var TCompRes, op: TMagic) =
  case op
  of mAddU: binaryUintExpr(p, n, r, "+")
  of mSubU: binaryUintExpr(p, n, r, "-")
  of mMulU: binaryUintExpr(p, n, r, "*")
  of mDivU:
    binaryUintExpr(p, n, r, "/")
    if n[1].typ.skipTypes(abstractRange).size == 8:
      r.res = "Math.trunc($1)" % [r.res]
  of mShrI:
    var x, y: TCompRes
    gen(p, n[1], x)
    gen(p, n[2], y)
    r.res = "($1 >>> $2)" % [x.rdLoc, y.rdLoc]
  of mEqRef:
    if mapType(n[1].typ) != etyBaseIndex:
      arithAux(p, n, r, op)
    else:
      var x, y: TCompRes
      gen(p, n[1], x)
      gen(p, n[2], y)
      r.res = "($# == $# && $# == $#)" % [x.address, y.address, x.res, y.res]
  of mEqProc:
    if skipTypes(n[1].typ, abstractInst).callConv == ccClosure:
      # a closure itself is a value type, so a dedicated comparision is
      # required
      binaryExpr(p, n, r, "cmpClosures", "cmpClosures($1, $2)")
    else:
      arithAux(p, n, r, op)
  else:
    arithAux(p, n, r, op)
  r.kind = resExpr

proc hasFrameInfo(p: PProc): bool =
  ({optLineTrace, optStackTrace} * p.options == {optLineTrace, optStackTrace}) and
      ((p.prc == nil) or not (sfPure in p.prc.flags))

proc lineDir(config: ConfigRef, info: TLineInfo, line: int): Rope =
  ropes.`%`("/* line $2 \"$1\" */$n",
         [rope(toFullPath(config, info)), rope(line)])

proc genLineDir(p: PProc, n: CgNode) =
  let line = toLinenumber(n.info)
  if line < 0:
    return
  if optLineDir in p.options or optLineDir in p.config.options:
    lineF(p, "$1", [lineDir(p.config, n.info, line)])
  if hasFrameInfo(p):
    lineF(p, "F.line = $1;$n", [rope(line)])

proc genExcept(p: PProc, n: CgNode) =
  ## Generates and emits code for an ``cnkExcept`` join point.
  if n.len > 1:
    # handler with filter
    var orExpr = ""
    for i in 1..<n.len - 1:
      useMagic(p, "isObj")
      let throwObj = n[i]

      if orExpr != "": orExpr.add("||")
      # Generate the correct type checking code depending on whether this is a
      # |NimSkull|-native or a JS-native exception
      if isImportedException(throwObj.typ, p.config):
        orExpr.addf("EXCEPTION instanceof $1",
          [throwObj.typ.sym.extname])
      else:
        orExpr.addf("isObj(EXCEPTION.m_type, $1)",
          [genTypeInfo(p, throwObj.typ)])

    # re-throw the exception when it doesn't match the filter
    lineF(p, "if (!(EXCEPTION && ($1))) { throw EXCEPTION; }\L", [orExpr])
  else:
    # catch-all handler
    discard

  # set the current exception:
  lineF(p, "lastJSError = EXCEPTION;$n", [])

  # restore the framePtr (it's incorrect when coming from unwinding)
  if hasFrameInfo(p):
    lineF(p, "framePtr = F;$n", [])

proc genRaiseStmt(p: PProc, n: CgNode) =
  if n[0].kind != cnkEmpty:
    var a: TCompRes
    gen(p, n[0], a)
    genLineDir(p, n)
    useMagic(p, "raiseException")
    lineF(p, "raiseException($1);$n",
             [a.rdLoc])
  else:
    genLineDir(p, n)
    useMagic(p, "reraiseException")
    line(p, "reraiseException();\L")

func intLiteral(v: Int128, typ: PType): string =
  if typ.kind == tyBool:
    if v == Zero: "false"
    else:         "true"
  else:           $v

proc genCaseJS(p: PProc, n: CgNode) =
  var
    cond: TCompRes
    totalRange = Zero
  genLineDir(p, n)
  gen(p, n[0], cond)
  let stringSwitch = skipTypes(n[0].typ, abstractVar).kind == tyString
  if stringSwitch:
    useMagic(p, "toJSStr")
    lineF(p, "switch (toJSStr($1)) {$n", [cond.rdLoc])
  else:
    lineF(p, "switch ($1) {$n", [cond.rdLoc])

  for i in 1..<n.len:
    let it = n[i]
    if isOfBranch(it):
      for j in 0..<it.len - 1:
        let e = it[j]
        if e.kind == cnkRange:
          let upper = getInt(e[1])
          var v = getInt(e[0])
          totalRange += upper - v
          if totalRange > 65535:
            localReport(p.config, n.info, BackendReport(kind: rbackJsTooCaseTooLarge))

          while v <= upper:
            lineF(p, "case $1:$n", [intLiteral(v, e[0].typ)])
            inc v
        else:
          if stringSwitch:
            case e.kind
            of cnkStrLit: lineF(p, "case $1:$n",
                [makeJSString(e.strVal, false)])
            else: internalError(p.config, e.info, "jsgen.genCaseStmt: 2")
          else:
            gen(p, e, cond)
            lineF(p, "case $1:$n", [cond.rdLoc])
      p.nested:
        lineF(p, "break Label$1;$n", [$it[^1].label])
    else:
      lineF(p, "default: $n", [])
      p.nested:
        lineF(p, "break Label$1;$n", [$it[^1].label])
  lineF(p, "}$n", [])

proc genAsmOrEmitStmt(p: PProc, n: CgNode) =
  genLineDir(p, n)
  p.body.add p.indentLine("")
  for i in 0..<n.len:
    let it = n[i]
    case it.kind
    of cnkStrLit:
      p.body.add(it.strVal)
    of cnkProc, cnkConst, cnkGlobal, cnkLocal:
      # for backwards compatibility we don't deref syms here :-(
      if false:
        discard
      else:
        var r: TCompRes
        gen(p, it, r)

        if it.typ.kind == tyPointer:
          # A fat pointer is disguised as an array
          r.res = r.address
          r.address = ""
          r.typ = etyNone
        elif r.typ == etyBaseIndex:
          # Deference first
          r.res = "$1[$2]" % [r.address, r.res]
          r.address = ""
          r.typ = etyNone

        p.body.add(r.rdLoc)
    else:
      var r: TCompRes
      gen(p, it, r)
      p.body.add(r.rdLoc)
  p.body.add "\L"

proc generateHeader(params: openArray[Loc]): string =
  ## Generates the JavaScript function parameter list for `params`.
  result = ""

  for param in params.items:
    if param.name == "":
      # it's a compile-time-only parameter
      continue
    if result != "": result.add(", ")
    result.add(param.name)
    if mapType(param.typ) == etyBaseIndex:
      result.add(", ")
      result.add(param.name)
      result.add("_Idx")

const
  nodeKindsNeedNoCopy = cnkLiterals + {
    cnkObjConstr, cnkTupleConstr, cnkArrayConstr, cnkCall, cnkCheckedCall,
    cnkNeg, cnkAdd, cnkSub, cnkMul, cnkDiv, cnkModI }

proc needsNoCopy(p: PProc; y: CgNode): bool =
  return y.kind in nodeKindsNeedNoCopy or
        ((mapType(y.typ) != etyBaseIndex) and
          (skipTypes(y.typ, abstractInst).kind in
            {tyRef, tyPtr, tyLent, tyVar, tyCstring, tyProc, tyOpenArray} + IntegralTypes))

proc genAsgnAux(p: PProc, x, y: CgNode, noCopyNeeded: bool) =
  var a, b: TCompRes
  var xtyp = mapType(x.typ)

  # disable `[]=` for cstring
  if x.kind == cnkArrayAccess and x[0].typ.skipTypes(abstractInst).kind == tyCstring:
    localReport(p.config, x.info, reportSem rsemUnexpectedArrayAssignForCstring)

  gen(p, x, a)
  genLineDir(p, y)
  gen(p, y, b)

  # we don't care if it's an etyBaseIndex (global) of a string, it's
  # still a string that needs to be copied properly:
  if x.typ.skipTypes(abstractInst).kind in {tySequence, tyString}:
    xtyp = etySeq
  case xtyp
  of etySeq:
    if x.typ.kind in {tyVar, tyLent} or (needsNoCopy(p, y) and needsNoCopy(p, x)) or noCopyNeeded:
      lineF(p, "$1 = $2;$n", [a.rdLoc, b.rdLoc])
    else:
      useMagic(p, "nimCopy")
      lineF(p, "$1 = nimCopy(null, $2, $3);$n",
               [a.rdLoc, b.res, genTypeInfo(p, y.typ)])
  of etyObject:
    if x.typ.kind in {tyVar, tyLent} or (needsNoCopy(p, y) and needsNoCopy(p, x)) or noCopyNeeded:
      lineF(p, "$1 = $2;$n", [a.rdLoc, b.rdLoc])
    else:
      useMagic(p, "nimCopy")
      lineF(p, "$1 = nimCopy($1, $2, $3);$n",
            [a.res, b.res, genTypeInfo(p, x.typ)])
  of etyBaseIndex:
    if a.typ != etyBaseIndex or b.typ != etyBaseIndex:
      if y.kind in {cnkCall, cnkCheckedCall}:
        let tmp = p.getTemp(false)
        lineF(p, "var $1 = $4; $2 = $1[0]; $3 = $1[1];$n", [tmp, a.address, a.res, b.rdLoc])
      elif b.typ == etyBaseIndex:
        lineF(p, "$# = [$#, $#];$n", [a.res, b.address, b.res])
      elif x.typ.kind == tyVar and y.typ.kind == tyPtr:
        lineF(p, "$# = [$#, $#];$n", [a.res, b.address, b.res])
        lineF(p, "$1 = $2;$n", [a.address, b.res])
        lineF(p, "$1 = $2;$n", [a.rdLoc, b.rdLoc])
      elif a.typ == etyBaseIndex:
        # array indexing may not map to var type
        if b.address != "":
          lineF(p, "$1 = $2; $3 = $4;$n", [a.address, b.address, a.res, b.res])
        else:
          lineF(p, "$1 = $2;$n", [a.address, b.res])
      else:
        internalError(p.config, x.info, $("genAsgn", b.typ, a.typ))
    elif b.address != "":
      lineF(p, "$1 = $2; $3 = $4;$n", [a.address, b.address, a.res, b.res])
    else:
      lineF(p, "$1 = $2;$n", [a.address, b.res])
  else:
    lineF(p, "$1 = $2;$n", [a.rdLoc, b.rdLoc])

proc genAsgn(p: PProc, n: CgNode) =
  genAsgnAux(p, n[0], n[1], noCopyNeeded=false)

proc genFastAsgn(p: PProc, n: CgNode) =
  # 'shallowCopy' always produced 'noCopyNeeded = true' here but this is wrong
  # for code like
  #  while j >= pos:
  #    dest[i].shallowCopy(dest[j])
  # See bug #5933. So we try to be more compatible with the C backend semantics
  # here for 'shallowCopy'. This is an educated guess and might require further
  # changes later:
  let noCopy = n[0].typ.skipTypes(abstractInst).kind in {tySequence, tyString}
  genAsgnAux(p, n[0], n[1], noCopyNeeded=noCopy)

proc getFieldPosition(p: PProc; f: CgNode): int =
  case f.kind
  of cnkIntLit: result = int(f.intVal)
  of cnkField:  result = f.field.position
  else: internalError(p.config, f.info, "genFieldPosition")

proc genFieldAddr(p: PProc, n: CgNode, r: var TCompRes) =
  var a: TCompRes
  r.typ = etyBaseIndex
  let b = if n.kind == cnkHiddenAddr: n.operand else: n
  gen(p, b[0], a)
  case b.kind
  of cnkTupleAccess:
    r.res = makeJSString("Field" & $getFieldPosition(p, b[1]))
  of cnkFieldAccess:
    p.config.internalAssert(b[1].kind == cnkField, b[1].info, "genFieldAddr")
    r.res = makeJSString(ensureMangledName(p, b[1].field))
  else:
    unreachable(n.kind)
  internalAssert p.config, a.typ != etyBaseIndex
  r.address = a.res
  r.kind = resExpr

proc genFieldAccess(p: PProc, n: CgNode, r: var TCompRes) =
  gen(p, n[0], r)
  r.typ = mapType(n.typ)

  case n.kind
  of cnkTupleAccess:
    r.res = ("$1.Field$2") %
        [r.res, getFieldPosition(p, n[1]).rope]
  of cnkFieldAccess:
    p.config.internalAssert(n[1].kind == cnkField, n[1].info, "genFieldAccess")
    r.res = "$1.$2" % [r.res, ensureMangledName(p, n[1].field)]
  else:
    unreachable(n.kind)

  if r.typ == etyBaseIndex:
    r.address = "$1[0]" % [r.res]
    r.res = "$1[1]" % [r.res]
  r.kind = resExpr

proc genAddr(p: PProc, n: CgNode, r: var TCompRes)

proc genFieldCheck(p: PProc, e: CgNode) =
  let
    setx = gen(p, e[1])
    val = gen(p, e[2])
    invert = e[3].intVal == 1

  useMagic(p, "raiseFieldError2")
  useMagic(p, "makeNimstrLit")
  useMagic(p, "reprDiscriminant") # no need to offset by firstOrd unlike for cgen
  lineF(p, "if ($1[$2]$3undefined) { raiseFieldError2(makeNimstrLit($4), reprDiscriminant($2, $5)); }$n",
    setx.res, val.rdLoc, if invert: ~"!==" else: ~"===",
    makeJSString(e[4].strVal), genTypeInfo(p, e[2].typ))

proc genArrayAddr(p: PProc, n: CgNode, r: var TCompRes) =
  var
    a, b: TCompRes
    first: Int128
  r.typ = etyBaseIndex
  let m = if n.kind == cnkHiddenAddr: n.operand else: n
  gen(p, m[0], a)
  gen(p, m[1], b)
  #internalAssert p.config, a.typ != etyBaseIndex and b.typ != etyBaseIndex
  let x = a.rdLoc
  r.address = x
  var typ = skipTypes(m[0].typ, abstractPtrs)
  if typ.kind == tyArray:
    first = firstOrd(p.config, typ[0])
  if first != 0:
    r.res = "($1) - ($2)" % [b.res, rope(first)]
  else:
    r.res = b.res
  r.kind = resExpr

proc genArrayAccess(p: PProc, n: CgNode, r: var TCompRes) =
  let ty = skipTypes(n[0].typ, abstractVar)
  internalAssert(p.config,
                 ty.kind in {tyArray, tyOpenArray, tySequence, tyString,
                             tyCstring, tyVarargs},
                 n.info,
                 "got " & $ty.kind)

  genArrayAddr(p, n, r)
  r.typ = mapType(n.typ)
  p.config.internalAssert(r.res != "", n.info, "genArrayAccess")
  if ty.kind == tyCstring:
    r.res = "$1.charCodeAt($2)" % [r.address, r.res]
  elif r.typ == etyBaseIndex:
    let x = r.rdLoc
    r.address = "$1[0]" % [x]
    r.res = "$1[1]" % [x]
  else:
    r.res = "$1[$2]" % [r.address, r.res]
  r.kind = resExpr

func isBoxedPointer(v: Loc): bool =
  ## Returns whether `v` stores a pointer value boxed in an array. If not,
  ## the value is stored as two variables (base and index).
  stfBoxed in v.storage

template isIndirect(x: PSym): bool =
  let v = x
  (sfGlobal in v.flags and
    #(mapType(v.typ) != etyObject) and
    {sfImportc, sfExportc} * v.flags == {} and
    v.kind notin {skProc, skFunc, skConverter, skMethod, skIterator,
                  skConst, skLet})

template isIndirect(x: Loc): bool =
  stfIndirect in x.storage

proc addrLoc(s: Loc, r: var TCompRes) =
  ## Generates the code for taking the address of the location identified by
  ## symbol node `n`
  if true:
    r.kind = resExpr
    r.typ = etyBaseIndex
    r.res = "0"
    if isIndirect(s):
      r.address = s.name
    elif mapType(s.typ) == etyBaseIndex and not isBoxedPointer(s):
      # box the separate base+index into an array first
      r.address = "[[$1, $1_Idx]]" % s.name
    else:
      # something that doesn't directly support having its address
      # taken (e.g. imported variable, parameter, let, etc.)
      r.address = "[$1]" % s.name

proc genAddr(p: PProc, n: CgNode, r: var TCompRes) =
  ## Dispatches to the appropriate procedure for generating the address-of
  ## operation based on the kind of node `n`
  case n.kind
  of cnkLocal:
    addrLoc(p.locals[n.local], r)
  of cnkConst:
    addrLoc(p.g.consts[n.cnst], r)
  of cnkGlobal:
    addrLoc(p.g.globals[n.global], r)
  of cnkFieldAccess, cnkTupleAccess:
    genFieldAddr(p, n, r)
  of cnkArrayAccess:
    genArrayAddr(p, n, r)
  of cnkDerefView, cnkDeref:
    # attemping to take the address of a deref expression -> skip both the
    # addr and deref
    gen(p, n.operand, r)
  of cnkLvalueConv:
    # conversion between lvalues of different underlying type is not possible,
    # so we simply skip the conversion and apply the operation to the source
    # expression
    genAddr(p, n.operand, r)
  of cnkObjUpConv, cnkObjDownConv:
    # object up-/down-conversions are no-ops
    genAddr(p, n.operand, r)
  of cnkStmtListExpr:
    for i in 0..<n.len-1:
      genStmt(p, n[i])

    genAddr(p, n[^1], r)
  else:
    internalError(p.config, n.info, "genAddr: " & $n.kind)

proc accessLoc(s: Loc, r: var TCompRes) =
    let k = mapType(s.typ)
    if k == etyBaseIndex:
      r.typ = etyBaseIndex
      if isBoxedPointer(s):
        if isIndirect(s):
          r.address = "$1[0][0]" % [s.name]
          r.res = "$1[0][1]" % [s.name]
        else:
          r.address = "$1[0]" % [s.name]
          r.res = "$1[1]" % [s.name]
      else:
        r.address = s.name
        r.res = s.name & "_Idx"
    elif isIndirect(s):
      r.res = "$1[0]" % [s.name]
    else:
      r.res = s.name

proc genDeref(p: PProc, n: CgNode, r: var TCompRes) =
  let it = n.operand
  let t = mapType(it.typ)
  if t == etyObject or it.typ.kind == tyLent:
    gen(p, it, r)
  else:
    var a: TCompRes
    gen(p, it, a)
    r.kind = a.kind
    r.typ = mapType(n.typ)
    if r.typ == etyBaseIndex:
      let tmp = p.getTemp
      r.address = "($1 = $2, $1)[0]" % [tmp, a.rdLoc]
      r.res = "$1[1]" % [tmp]
      r.tmpLoc = tmp
    elif a.typ == etyBaseIndex:
      if a.tmpLoc != "":
        r.tmpLoc = a.tmpLoc
      r.res = a.rdLoc
    else:
      internalError(p.config, n.info, "genDeref")

proc genArgNoParam(p: PProc, n: CgNode, r: var TCompRes) =
  var a: TCompRes
  gen(p, n, a)
  if a.typ == etyBaseIndex:
    r.res.add(a.address)
    r.res.add(", ")
    r.res.add(a.res)
  else:
    r.res.add(a.res)

proc genArg(p: PProc, n: CgNode, param: PSym, r: var TCompRes; emitted: ptr int = nil) =
  var a: TCompRes
  gen(p, n, a)
  if skipTypes(param.typ, abstractVar).kind in {tyOpenArray, tyVarargs} and
      a.typ == etyBaseIndex:
    r.res.add("$1[$2]" % [a.address, a.res])
  elif a.typ == etyBaseIndex:
    r.res.add(a.address)
    r.res.add(", ")
    r.res.add(a.res)
    if emitted != nil: inc emitted[]
  else:
    r.res.add(a.res)

proc genArgs(p: PProc, n: CgNode, r: var TCompRes; start=1) =
  r.res.add("(")
  var hasArgs = false

  var typ = skipTypes(n[0].typ, abstractInst)
  assert(typ.kind == tyProc)
  assert(typ.len == typ.n.len)
  var emitted = start-1

  for i in start..<(1 + numArgs(n)):
    let it = n[i]
    var paramType: PNode = nil
    if i < typ.len:
      assert(typ.n[i].kind == nkSym)
      paramType = typ.n[i]
      if paramType.typ.isCompileTimeOnly: continue

    if hasArgs: r.res.add(", ")
    if paramType.isNil:
      genArgNoParam(p, it, r)
    else:
      genArg(p, it, paramType.sym, r, addr emitted)
    inc emitted
    hasArgs = true
  r.res.add(")")
  r.kind = resExpr

proc genOtherArg(p: PProc; n: CgNode; i: int; typ: PType;
                 generated: var int; r: var TCompRes) =
  if i >= numArgs(n) + 1:
    globalReport(p.config, n.info, semReportCountMismatch(
      rsemExpectedParameterForJsPattern,
      expected = i,
      got = numArgs(n)))

  let it = n[i]
  var paramType: PNode = nil
  if i < typ.len:
    assert(typ.n[i].kind == nkSym)
    paramType = typ.n[i]
    if paramType.typ.isCompileTimeOnly: return
  if paramType.isNil:
    genArgNoParam(p, it, r)
  else:
    genArg(p, it, paramType.sym, r)
  inc generated

proc genPatternCall(p: PProc; n: CgNode; pat: string; typ: PType;
                    r: var TCompRes) =
  var i = 0
  var j = 1
  r.kind = resExpr
  while i < pat.len:
    case pat[i]
    of '@':
      var generated = 0
      for k in j..<(1 + numArgs(n)):
        if generated > 0: r.res.add(", ")
        genOtherArg(p, n, k, typ, generated, r)
      inc i
    of '#':
      var generated = 0
      genOtherArg(p, n, j, typ, generated, r)
      inc j
      inc i
    of '\31':
      # unit separator
      r.res.add("#")
      inc i
    of '\29':
      # group separator
      r.res.add("@")
      inc i
    else:
      let start = i
      while i < pat.len:
        if pat[i] notin {'@', '#', '\31', '\29'}: inc(i)
        else: break
      if i - 1 >= start:
        r.res.add(substr(pat, start, i - 1))

proc genInfixCall(p: PProc, n: CgNode, r: var TCompRes) =
  # don't call '$' here for efficiency:
  let f = p.g.env[n[0].prc]
  assert sfInfixCall in f.flags
  if true:
    let pat = f.extname
    internalAssert p.config, pat.len > 0
    if pat.contains({'#', '(', '@'}):
      var typ = skipTypes(n[0].typ, abstractInst)
      assert(typ.kind == tyProc)
      genPatternCall(p, n, pat, typ, r)
      return
  if numArgs(n) != 0:
    gen(p, n[1], r)
    if r.typ == etyBaseIndex:
      p.config.internalAssert(r.address != "", n.info, "cannot invoke with infix syntax")
      r.res = "$1[$2]" % [r.address, r.res]
      r.address = ""
      r.typ = etyNone
    r.res.add(".")
  var op: TCompRes
  gen(p, n[0], op)
  r.res.add(op.res)
  genArgs(p, n, r, 2)

proc genCall(p: PProc, n: CgNode, r: var TCompRes) =
  gen(p, n[0], r)
  genArgs(p, n, r)
  if n.typ != nil:
    let t = mapType(n.typ)
    if t == etyBaseIndex:
      let tmp = p.getTemp
      r.address = "($1 = $2, $1)[0]" % [tmp, r.rdLoc]
      r.res = "$1[1]" % [tmp]
      r.tmpLoc = tmp
      r.typ = t

proc genEcho(p: PProc, n: CgNode, r: var TCompRes) =
  useMagic(p, "toJSStr") # Used in rawEcho
  useMagic(p, "rawEcho")
  r.res.add("rawEcho(")
  # the first argument is a literal type that we don't need
  for i in 2..<(1 + numArgs(n)):
    let it = n[i]
    if it.typ.isCompileTimeOnly: continue
    if i > 2: r.res.add(", ")
    genArgNoParam(p, it, r)
  r.res.add(")")
  r.kind = resExpr

proc putToSeq(s: string, indirect: bool): Rope =
  result = rope(s)
  if indirect: result = "[$1]" % [result]

proc createVar(p: PProc, typ: PType, indirect: bool): Rope
proc createRecordVarAux(p: PProc, rec: PNode, excludedFieldIDs: IntSet, output: var Rope) =
  case rec.kind
  of nkRecList:
    for i in 0..<rec.len:
      createRecordVarAux(p, rec[i], excludedFieldIDs, output)
  of nkRecCase:
    createRecordVarAux(p, rec[0], excludedFieldIDs, output)
    for i in 1..<rec.len:
      createRecordVarAux(p, lastSon(rec[i]), excludedFieldIDs, output)
  of nkSym:
    # Do not produce code for void types
    if isEmptyType(rec.sym.typ): return
    if rec.sym.id notin excludedFieldIDs:
      if output.len > 0: output.add(", ")
      output.addf("$#: ", [mangleName(p.module, rec.sym)])
      output.add(createVar(p, rec.sym.typ, false))
  else: internalError(p.config, rec.info, "createRecordVarAux")

proc createObjInitList(p: PProc, typ: PType, excludedFieldIDs: IntSet, output: var Rope) =
  var t = typ
  if objHasTypeField(t):
    if output.len > 0: output.add(", ")
    output.addf("m_type: $1", [genTypeInfo(p, t)])
  while t != nil:
    t = t.skipTypes(skipPtrs)
    createRecordVarAux(p, t.n, excludedFieldIDs, output)
    t = t[0]

proc arrayTypeForElemType(typ: PType): string =
  # XXX This should also support tyEnum and tyBool
  case typ.kind
  of tyInt, tyInt32: "Int32Array"
  of tyInt16: "Int16Array"
  of tyInt8: "Int8Array"
  of tyUInt, tyUInt32: "Uint32Array"
  of tyUInt16: "Uint16Array"
  of tyUInt8: "Uint8Array"
  of tyFloat32: "Float32Array"
  of tyFloat64, tyFloat: "Float64Array"
  else: ""

proc createVar(p: PProc, typ: PType, indirect: bool): Rope =
  var t = skipTypes(typ, abstractInst)
  case t.kind
  of tyInt..tyInt64, tyUInt..tyUInt64, tyEnum, tyChar:
    if t.sym.extname == "bigint":
      result = putToSeq("0n", indirect)
    else:
      result = putToSeq("0", indirect)
  of tyFloat..tyFloat64:
    result = putToSeq("0.0", indirect)
  of tyRange, tyGenericInst, tyAlias, tySink, tyLent:
    result = createVar(p, lastSon(typ), indirect)
  of tySet:
    result = putToSeq("{}", indirect)
  of tyBool:
    result = putToSeq("false", indirect)
  of tyNil:
    result = putToSeq("null", indirect)
  of tyArray:
    let length = toInt(lengthOrd(p.config, t))
    let e = elemType(t)
    let jsTyp = arrayTypeForElemType(e)
    if jsTyp.len > 0:
      result = "new $1($2)" % [rope(jsTyp), rope(length)]
    elif length > 32:
      useMagic(p, "arrayConstr")
      # XXX: arrayConstr depends on nimCopy. This line shouldn't be necessary.
      useMagic(p, "nimCopy")
      result = "arrayConstr($1, $2, $3)" % [rope(length),
          createVar(p, e, false), genTypeInfo(p, e)]
    else:
      result = rope("[")
      var i = 0
      while i < length:
        if i > 0: result.add(", ")
        result.add(createVar(p, e, false))
        inc(i)
      result.add("]")
    if indirect: result = "[$1]" % [result]
  of tyTuple:
    result = rope("{")
    for i in 0..<t.len:
      if i > 0: result.add(", ")
      result.addf("Field$1: $2", [i.rope,
            createVar(p, t[i], false)])
    result.add("}")
    if indirect: result = "[$1]" % [result]
  of tyObject:
    var initList: Rope
    createObjInitList(p, t, initIntSet(), initList)
    result = ("({$1})") % [initList]
    if indirect: result = "[$1]" % [result]
  of tyVar, tyPtr, tyRef, tyPointer:
    if mapType(t) == etyBaseIndex:
      result = putToSeq("[null, 0]", indirect)
    else:
      result = putToSeq("null", indirect)
  of tySequence, tyString:
    result = putToSeq("[]", indirect)
  of tyCstring, tyProc, tyOpenArray:
    result = putToSeq("null", indirect)
  of tyStatic:
    p.config.internalAssert(t.n != nil, "createVar: " & $t.kind)
    result = createVar(p, lastSon t, indirect)
  of tyUserTypeClasses:
    p.config.internalAssert(t.isResolvedUserTypeClass)
    result = createVar(p, lastSon t, indirect)
  else:
    internalError(p.config, "createVar: " & $t.kind)
    result = ""

template returnType: untyped = ~""

proc storage(flags: TSymFlags, kind: TSymKind, addrTaken: bool): StorageFlags =
  ## Computes and returns based on `flags` and `kind` the storage flags
  ## for a location. `addrTaken` signals whether the location has its address
  ## taken at some point.
  if sfGlobal in flags or addrTaken:
    if kind != skParam:
      result.incl stfBoxed

    if kind notin {skConst, skLet, skParam} and
       {sfImportc, sfExportc} * flags == {}:
      result.incl stfIndirect

proc setupLocalLoc(p: PProc, id: LocalId, kind: TSymKind; name = "") =
  ## Sets up the ``Loc`` for the local with `id`. `kind` is used for
  ## computing the storage flags and a non-empty `name` overrides the
  ## mangled name.
  var loc = Loc(name: mangleName(p.fullBody[id], id),
                typ: p.fullBody[id].typ,
                storage: storage(p.fullBody[id].flags, kind,
                                 id in p.addrTaken))

  if name != "":
    # override with the provided name
    loc.name = name

  p.locals[id] = loc

proc defineGlobal*(globals: PGlobals, m: BModule, id: GlobalId) =
  ## Emits the definition for the single global `v` into the top-level section,
  ## with `m` being the module the global belongs to. Also sets up the
  ## symbol's JavaScript name.
  let p = newInitProc(globals, m)
  let v = globals.env[id]
  let name = mangleName(m, v)
  if exfNoDecl notin v.extFlags and sfImportc notin v.flags:
    lineF(p, "var $1 = $2;$n", [name, createVar(p, v.typ, isIndirect(v))])

  globals.globals[id] = Loc(name: name, typ: v.typ,
                            storage: storage(v.flags, v.kind, false))
  # add to the top-level section:
  globals.code.add(p.body)

proc genVarInit(p: PProc, typ: PType, varName: string, storage: StorageFlags,
                n: CgNode) =
  var
    a: TCompRes
    s: Rope

  if n.kind == cnkEmpty:
    if mapType(typ) == etyBaseIndex and (stfBoxed notin storage):
      lineF(p, "var $1 = null;$n", [varName])
      lineF(p, "var $1_Idx = 0;$n", [varName])
    else:
      let val = createVar(p, typ, stfIndirect in storage)
      lineF(p, "var $1 = $2;$n", [varName, val])
  else:
    gen(p, n, a)
    case mapType(typ)
    of etyObject, etySeq:
      if needsNoCopy(p, n) or classifyBackendView(typ) == bvcSequence:
        s = a.res
      else:
        useMagic(p, "nimCopy")
        s = "nimCopy(null, $1, $2)" % [a.res, genTypeInfo(p, n.typ)]
    of etyBaseIndex:
      p.config.internalAssert(a.typ == etyBaseIndex, n.info)
      if stfBoxed in storage:
        s = "[$1, $2]" % [a.address, a.res]
      else:
        lineF(p, "var $1 = $2, $1_Idx = $3;$n",
                 [varName, a.address, a.res])
        # exit early because we've already emitted the definition
        return
    else:
      s = a.res
    if stfIndirect in storage:
      lineF(p, "var $1 = [$2];$n", [varName, s])
    else:
      lineF(p, "var $1 = $2;$n", [varName, s])

proc genDef(p: PProc, it: CgNode) =
  let
    id   = it[0].local
    kind = (if p.fullBody[id].isImmutable: skLet else: skVar)
  setupLocalLoc(p, id, kind)
  genLineDir(p, it)
  genVarInit(p, p.locals[id].typ, p.locals[id].name, p.locals[id].storage,
             it[1])

proc genConstant*(g: PGlobals, m: BModule, id: ConstId) =
  let
    c = g.env[id]
    name = mangleName(m, c)
    storage = storage({}, skConst, false)

  if exfNoDecl notin c.extFlags:
    var p = newInitProc(g, m)
    #genLineDir(p, c.ast)
    genVarInit(p, c.typ, name, storage,
               translate(g.env[g.env.dataFor(id)]))
    g.constants.add(p.body)

  # all constants need a name:
  g.consts[id] = Loc(name: name, typ: c.typ, storage: storage)

proc genNew(p: PProc, n: CgNode, r: var TCompRes) =
  ## Updates `r` with the result of a ``new`` magic invocation.
  let t = skipTypes(n.typ, abstractInst)
  assert t.kind == tyRef

  r.kind = resVal
  r.typ = mapType(t)
  if r.typ == etyObject:
    # the underlying type has reference semantics already, no
    # boxing is needed
    r.res = createVar(p, t.base, false)
  else:
    r.address = "[$1]" % createVar(p, t.base, false)
    r.res = "0"

proc genNewSeq(p: PProc, n: CgNode) =
  var x, y: TCompRes
  gen(p, n[1], x)
  gen(p, n[2], y)
  let t = skipTypes(n[1].typ, abstractVar)[0]
  lineF(p, "$1 = new Array($2); for (var i = 0 ; i < $2 ; ++i) { $1[i] = $3; }", [
    x.rdLoc, y.rdLoc, createVar(p, t, false)])

proc genOrd(p: PProc, n: CgNode, r: var TCompRes) =
  case skipTypes(n[1].typ, abstractVar + abstractRange).kind
  of tyEnum, tyInt..tyUInt64, tyChar: gen(p, n[1], r)
  of tyBool: unaryExpr(p, n, r, "", "($1 ? 1 : 0)")
  else: internalError(p.config, n.info, "genOrd")

proc genConStrStr(p: PProc, n: CgNode, r: var TCompRes) =
  var a: TCompRes

  gen(p, n[1], a)
  r.kind = resExpr
  if skipTypes(n[1].typ, abstractVarRange).kind == tyChar:
    r.res.add("[$1].concat(" % [a.res])
  else:
    r.res.add("($1 || []).concat(" % [a.res])

  for i in 2..<n.len - 1:
    gen(p, n[i], a)
    if skipTypes(n[i].typ, abstractVarRange).kind == tyChar:
      r.res.add("[$1]," % [a.res])
    else:
      r.res.add("$1 || []," % [a.res])

  gen(p, n[^1], a)
  if skipTypes(n[^1].typ, abstractVarRange).kind == tyChar:
    r.res.add("[$1])" % [a.res])
  else:
    r.res.add("$1 || [])" % [a.res])

proc genReprAux(p: PProc, n: CgNode, r: var TCompRes, magic: string; typ = "") =
  useMagic(p, magic)
  r.res.add(magic & "(")
  var a: TCompRes

  gen(p, n[1], a)
  if magic == "reprAny":
    # the pointer argument in reprAny is expandend to
    # (pointedto, pointer), so we need to fill it
    if a.address == "":
      r.res.add(a.res)
      r.res.add(", null")
    else:
      r.res.add("$1, $2" % [a.address, a.res])
  else:
    r.res.add(a.res)

  if typ != "":
    r.res.add(", ")
    r.res.add(typ)
  r.res.add(")")

proc genRepr(p: PProc, n: CgNode, r: var TCompRes) =
  let t = skipTypes(n[1].typ, abstractVarRange)
  case t.kind:
  of tyInt..tyInt64, tyUInt..tyUInt64:
    genReprAux(p, n, r, "reprInt")
  of tyChar:
    genReprAux(p, n, r, "reprChar")
  of tyBool:
    genReprAux(p, n, r, "reprBool")
  of tyFloat..tyFloat64:
    genReprAux(p, n, r, "reprFloat")
  of tyString:
    genReprAux(p, n, r, "reprStr")
  of tyEnum, tyOrdinal:
    genReprAux(p, n, r, "reprEnum", genTypeInfo(p, t))
  of tySet:
    genReprAux(p, n, r, "reprSet", genTypeInfo(p, t))
  of tyEmpty, tyVoid:
    localReport(p.config, n.info, reportSem rsemUnexpectedVoidType)
  of tyPointer:
    genReprAux(p, n, r, "reprPointer")
  of tyOpenArray, tyVarargs:
    genReprAux(p, n, r, "reprJSONStringify")
  else:
    genReprAux(p, n, r, "reprAny", genTypeInfo(p, t))
  r.kind = resExpr

proc genOf(p: PProc, n: CgNode, r: var TCompRes) =
  var x: TCompRes
  let t = skipTypes(n[2].typ,
                    abstractVarRange+{tyRef, tyPtr, tyLent, tyTypeDesc})
  gen(p, n[1], x)
  if tfFinal in t.flags:
    r.res = "($1.m_type == $2)" % [x.res, genTypeInfo(p, t)]
  else:
    useMagic(p, "isObj")
    r.res = "isObj($1.m_type, $2)" % [x.res, genTypeInfo(p, t)]
  r.kind = resExpr

proc genDefault(p: PProc, n: CgNode; r: var TCompRes) =
  r.typ = mapType(n.typ)
  r.kind = resExpr
  case r.typ
  of etyBaseIndex:
    r.address = "null"
    r.res = "0"
  else:
    r.res = createVar(p, n.typ, indirect = false)

proc genReset(p: PProc, n: CgNode) =
  var x: TCompRes
  useMagic(p, "genericReset")
  gen(p, n[1], x)
  if x.typ == etyBaseIndex:
    lineF(p, "$1 = null, $2 = 0;$n", [x.address, x.res])
  else:
    lineF(p, "$1 = genericReset($1, $2);$n", [x.rdLoc,
                  genTypeInfo(p, n[1].typ)])

proc genMove(p: PProc; n: CgNode; r: var TCompRes) =
  var a: TCompRes
  r.kind = resVal
  r.res = p.getTemp()
  gen(p, n[1], a)
  lineF(p, "$1 = $2;$n", [r.rdLoc, a.rdLoc])
  genReset(p, n)
  #lineF(p, "$1 = $2;$n", [dest.rdLoc, src.rdLoc])

proc genJSArrayConstr(p: PProc, n: CgNode, r: var TCompRes) =
  var a: TCompRes
  r.res = rope("[")
  r.kind = resExpr
  for i in 0 ..< n.len:
    if i > 0: r.res.add(", ")
    gen(p, n[i], a)
    if a.typ == etyBaseIndex:
      r.res.addf("[$1, $2]", [a.address, a.res])
    else:
      if not needsNoCopy(p, n[i]):
        let typ = n[i].typ.skipTypes(abstractInst)
        useMagic(p, "nimCopy")
        a.res = "nimCopy(null, $1, $2)" % [a.rdLoc, genTypeInfo(p, typ)]
      r.res.add(a.res)
  r.res.add("]")

proc genRangeChck(p: PProc, n: CgNode, r: var TCompRes)

proc genMagic(p: PProc, n: CgNode, r: var TCompRes) =
  let op = getCalleeMagic(p.g.env, n[0])
  case op
  of mAddI..mUnaryMinusI64, mNot..mUnaryPlusF64:
    arith(p, n, r, op)
  of mCharToStr:
    unaryExpr(p, n, r, "nimCharToStr", "nimCharToStr($1)")
  of mBoolToStr:
    unaryExpr(p, n, r, "nimBoolToStr", "nimBoolToStr($1)")
  of mCStrToStr:
    unaryExpr(p, n, r, "cstrToNimstr", "cstrToNimstr($1)")
  of mStrToCStr:
    unaryExpr(p, n, r, "toJSStr", "toJSStr($1)")
  of mIsolate:
    let x = gen(p, n[1])
    r.res = rdLoc(x)
    r.typ = x.typ
    r.kind = resExpr
    # XXX: this implementation:
    #      * has inconsistent behaviour with the other backends. There, a
    #        procedure call is made (resulting in a full copy)
    #      * is wrong: ``rdLoc`` dereferences pointers
  of mRepr: genRepr(p, n, r)
  of mAppendStrCh:
    binaryExpr(p, n, r, "addChar",
        "addChar($1, $2);")
  of mAppendStrStr:
    var lhs, rhs: TCompRes
    gen(p, n[1], lhs)
    gen(p, n[2], rhs)

    if skipTypes(n[1].typ, abstractVarRange).kind == tyCstring:
      r.res = "if (null != $1) { if (null == $2) $2 = $1; else $2 += $1; }" %
        [rhs.rdLoc, lhs.rdLoc]
    else:
      r.res = "$1.push.apply($1, $2);" % [lhs.rdLoc, rhs.rdLoc]
    r.kind = resExpr
  of mAppendSeqElem:
    var x, y: TCompRes
    gen(p, n[1], x)
    gen(p, n[2], y)
    if mapType(n[2].typ) == etyBaseIndex:
      let c = "[$1, $2]" % [y.address, y.res]
      r.res = "$1.push($2);" % [x.rdLoc, c]
    elif needsNoCopy(p, n[2]):
      r.res = "$1.push($2);" % [x.rdLoc, y.rdLoc]
    else:
      useMagic(p, "nimCopy")
      let c = getTemp(p, defineInLocals=false)
      lineF(p, "var $1 = nimCopy(null, $2, $3);$n",
            [c, y.rdLoc, genTypeInfo(p, n[2].typ)])
      r.res = "$1.push($2);" % [x.rdLoc, c]
    r.kind = resExpr
  of mConStrStr:
    genConStrStr(p, n, r)
  of mEqStr:
    binaryExpr(p, n, r, "eqStrings", "eqStrings($1, $2)")
  of mLeStr:
    binaryExpr(p, n, r, "cmpStrings", "(cmpStrings($1, $2) <= 0)")
  of mLtStr:
    binaryExpr(p, n, r, "cmpStrings", "(cmpStrings($1, $2) < 0)")
  of mIsNil:
    # we want to accept undefined, so we ==
    if mapType(n[1].typ) != etyBaseIndex:
      unaryExpr(p, n, r, "", "($1 == null)")
    else:
      var x: TCompRes
      gen(p, n[1], x)
      r.res = "($# == null && $# === 0)" % [x.address, x.res]
  of mEnumToStr: genRepr(p, n, r)
  of mNew: genNew(p, n, r)
  of mChr: gen(p, n[1], r)
  of mArrToSeq:
    # only array literals doesn't need copy
    if n[1].kind == cnkArrayConstr:
      genJSArrayConstr(p, n[1], r)
    else:
      var x: TCompRes
      gen(p, n[1], x)
      useMagic(p, "nimCopy")
      r.res = "nimCopy(null, $1, $2)" % [x.rdLoc, genTypeInfo(p, n.typ)]
  of mDestroy, mTrace: discard "ignore calls to the default destructor"
  of mOrd: genOrd(p, n, r)
  of mLengthStr, mLengthSeq, mLengthOpenArray, mLengthArray:
    var x: TCompRes
    gen(p, n[1], x)
    if skipTypes(n[1].typ, abstractInst).kind == tyCstring:
      r.res = "(($1) == null ? 0 : ($1).length)" % [x.rdLoc]
    else:
      r.res = "($1).length" % [x.rdLoc]
    r.kind = resExpr
  of mHigh:
    var x: TCompRes
    gen(p, n[1], x)
    if skipTypes(n[1].typ, abstractInst).kind == tyCstring:
      r.res = "(($1) == null ? -1 : ($1).length - 1)" % [x.rdLoc]
    else:
      r.res = "($1).length - 1" % [x.rdLoc]
    r.kind = resExpr
  of mSetLengthStr:
    binaryExpr(p, n, r, "mnewString", "($1.length = $2)")
  of mSetLengthSeq:
    var x, y: TCompRes
    gen(p, n[1], x)
    gen(p, n[2], y)
    let t = skipTypes(n[1].typ, abstractVar)[0]
    r.res = """if ($1.length < $2) { for (var i = $1.length ; i < $2 ; ++i) $1.push($3); }
               else { $1.length = $2; }""" % [x.rdLoc, y.rdLoc, createVar(p, t, false)]
    r.kind = resExpr
  of mCard: unaryExpr(p, n, r, "SetCard", "SetCard($1)")
  of mLtSet: binaryExpr(p, n, r, "SetLt", "SetLt($1, $2)")
  of mLeSet: binaryExpr(p, n, r, "SetLe", "SetLe($1, $2)")
  of mEqSet: binaryExpr(p, n, r, "SetEq", "SetEq($1, $2)")
  of mMulSet: binaryExpr(p, n, r, "SetMul", "SetMul($1, $2)")
  of mPlusSet: binaryExpr(p, n, r, "SetPlus", "SetPlus($1, $2)")
  of mMinusSet: binaryExpr(p, n, r, "SetMinus", "SetMinus($1, $2)")
  of mIncl: binaryExpr(p, n, r, "", "$1[$2] = true")
  of mExcl: binaryExpr(p, n, r, "", "delete $1[$2]")
  of mInSet:
    binaryExpr(p, n, r, "", "($1[$2] != undefined)")
  of mNewSeq: genNewSeq(p, n)
  of mNewSeqOfCap: unaryExpr(p, n, r, "", "[]")
  of mOf: genOf(p, n, r)
  of mDefault: genDefault(p, n, r)
  of mWasMoved: genReset(p, n)
  of mEcho: genEcho(p, n, r)
  of mNLen..mNError:
    localReport(p.config, n.info, reportSym(
      rsemConstExpressionExpected, p.env[n[0].prc]))

  of mNewString: unaryExpr(p, n, r, "mnewString", "mnewString($1)")
  of mNewStringOfCap:
    unaryExpr(p, n, r, "mnewString", "mnewString(0)")
  of mAbsI, mDotDot:
    genCall(p, n, r)
  of mParseBiggestFloat:
    useMagic(p, "nimParseBiggestFloat")
    genCall(p, n, r)
  of mMove:
    genMove(p, n, r)
  # of mAccessEnv:
  #   unaryExpr(p, n, r, "accessEnv", "accessEnv($1)")
  of mFinished:
    # access the ``:state`` field of the environment and check if its value is
    # less than zero
    var x: TCompRes
    gen(p, n[1], x)
    # XXX: the implementation is a hack -- it makes a lot of implicit
    #      assumptions and is thus very brittle. However, don't attempt to
    #      fix it here; implement the lowering of the ``mFinished`` magic as a
    #      MIR pass that is used for all backends
    r.res = "($1.env.$2 < 0)" % [x.rdLoc, mangleJs(":state")]
    r.kind = resExpr
  of mChckRange:
    genRangeChck(p, n, r)
  of mChckNaN:
    discard "implementation is missing"
  of mChckIndex:
    let
      first = firstOrd(p.config, n[1].typ)
      arr = gen(p, n[1])
      idx = gen(p, n[2])

    useMagic(p, "chckIndx")
    if first == 0:
      lineF(p, "(chckIndx($2, 0, ($1).length - 1));$n",
            [rdLoc(arr), rdLoc(idx)])
    else:
      # can only be a statically-sized array
      lineF(p, "(chckIndx($1, $2, $3));$n",
            [rdLoc(idx), rope(first), rope(lastOrd(p.config, n[1].typ))])
  of mChckBounds:
    let
      first = firstOrd(p.config, n[1].typ)
      arr = gen(p, n[1])
      lo = gen(p, n[2])
      hi = gen(p, n[3])

    useMagic(p, "chckBounds")
    if first == 0:
      lineF(p, "chckBounds($2, $3, 0, ($1).length - 1);$n",
            [rdLoc(arr), rdLoc(lo), rdLoc(hi)])
    else:
      # can only be a statically-sized array
      lineF(p, "(chckBounds($1, $2, $3, $4));$n",
            [rdLoc(lo), rdLoc(hi), rope(first),
             rope(lastOrd(p.config, n[1].typ))])
  of mChckField:
    genFieldCheck(p, n)
  of mChckObj:
    let x = gen(p, n[1])
    useMagic(p, "chckObj")
    # the nil-check is expected to have taken place already
    lineF(p, "chckObj($1.m_type, $2);$n",
          [rdLoc(x), genTypeInfo(p, n[2].typ)])
  else:
    genCall(p, n, r)
    #else internalError(p.config, e.info, 'genMagic: ' + magicToStr[op]);

proc genSetConstr(p: PProc, n: CgNode, r: var TCompRes) =
  var
    a, b: TCompRes
  useMagic(p, "setConstr")
  r.res = rope("setConstr(")
  r.kind = resExpr
  for i in 0..<n.len:
    if i > 0: r.res.add(", ")
    var it = n[i]
    if it.kind == cnkRange:
      gen(p, it[0], a)
      gen(p, it[1], b)

      if it[0].typ.kind == tyBool:
        r.res.addf("$1, $2", [a.res, b.res])
      else:
        r.res.addf("[$1, $2]", [a.res, b.res])
    else:
      gen(p, it, a)
      r.res.add(a.res)
  r.res.add(")")

proc genArrayConstr(p: PProc, n: CgNode, r: var TCompRes) =
  ## Constructs array or sequence.
  ## Nim array of uint8..uint32, int8..int32 maps to JS typed arrays.
  ## Nim sequence maps to JS array.
  var t = skipTypes(n.typ, abstractInst)
  let e = elemType(t)
  let jsTyp = arrayTypeForElemType(e)
  if skipTypes(n.typ, abstractVarRange).kind != tySequence and jsTyp.len > 0:
    # generate typed array
    # for example Nim generates `new Uint8Array([1, 2, 3])` for `[byte(1), 2, 3]`
    # TODO use `set` or loop to initialize typed array which improves performances in some situations
    var a: TCompRes
    r.res = "new $1([" % [rope(jsTyp)]
    r.kind = resExpr
    for i in 0 ..< n.len:
      if i > 0: r.res.add(", ")
      gen(p, n[i], a)
      r.res.add(a.res)
    r.res.add("])")
  else:
    genJSArrayConstr(p, n, r)

proc genTupleConstr(p: PProc, n: CgNode, r: var TCompRes) =
  var a: TCompRes
  r.res = rope("{")
  r.kind = resExpr
  for i in 0..<n.len:
    if i > 0: r.res.add(", ")
    var it = n[i]
    gen(p, it, a)
    let typ = it.typ.skipTypes(abstractInst)
    if a.typ == etyBaseIndex:
      r.res.addf("Field$#: [$#, $#]", [i.rope, a.address, a.res])
    else:
      if not needsNoCopy(p, it):
        useMagic(p, "nimCopy")
        a.res = "nimCopy(null, $1, $2)" % [a.rdLoc, genTypeInfo(p, typ)]
      r.res.addf("Field$#: $#", [i.rope, a.res])
  r.res.add("}")

proc genObjConstr(p: PProc, n: CgNode, r: var TCompRes) =
  var a: TCompRes
  r.kind = resExpr
  var initList : Rope
  var fieldIDs = initIntSet()
  for i in 0..<n.len:
    if i > 0: initList.add(", ")
    var it = n[i]
    internalAssert p.config, it.kind == cnkBinding
    let val = it[1]
    gen(p, val, a)
    let f = it[0].field
    let name = ensureMangledName(p, f)
    fieldIDs.incl(lookupFieldAgain(n.typ, f).id)

    let typ = val.typ.skipTypes(abstractInst)
    if a.typ == etyBaseIndex:
      initList.addf("$#: [$#, $#]", [name, a.address, a.res])
    else:
      if not needsNoCopy(p, val):
        useMagic(p, "nimCopy")
        a.res = "nimCopy(null, $1, $2)" % [a.rdLoc, genTypeInfo(p, typ)]
      initList.addf("$#: $#", [name, a.res])
  let t = skipTypes(n.typ, abstractInst + skipPtrs)
  createObjInitList(p, t, fieldIDs, initList)
  r.res = ("{$1}") % [initList]

proc genConv(p: PProc, n: CgNode, r: var TCompRes) =
  var dest = skipTypes(n.typ, abstractVarRange)
  var src = skipTypes(n.operand.typ, abstractVarRange)
  gen(p, n.operand, r)
  if dest.kind == src.kind:
    # no-op conversion
    return
  let toInt = (dest.kind in tyInt..tyInt32)
  let fromInt = (src.kind in tyInt..tyInt32)
  let toUint = (dest.kind in tyUInt..tyUInt32)
  let fromUint = (src.kind in tyUInt..tyUInt32)
  if toUint and (fromInt or fromUint):
    let trimmer = unsignedTrimmer(dest.size)
    r.res = "($1 $2)" % [r.res, trimmer]
  elif dest.kind == tyBool:
    r.res = "(!!($1))" % [r.res]
    r.kind = resExpr
  elif toInt:
    r.res = "(($1) | 0)" % [r.res]
  else:
    # TODO: What types must we handle here?
    discard

proc downConv(p: PProc, n: CgNode, r: var TCompRes) =
  gen(p, n.operand, r)        # XXX

proc genRangeChck(p: PProc, n: CgNode, r: var TCompRes) =
  var a, b: TCompRes
  gen(p, n[1], r)
  if true:
    gen(p, n[2], a)
    gen(p, n[3], b)
    useMagic(p, "chckRange")
    r.res = "chckRange($1, $2, $3)" % [r.res, a.res, b.res]
    r.kind = resExpr

proc frameCreate(p: PProc; procname, filename: Rope): Rope =
  const frameFmt =
    "var F = {procname: $1, prev: framePtr, filename: $2, line: 0};$n"

  result = p.indentLine(frameFmt % [procname, filename])
  result.add p.indentLine(ropes.`%`("framePtr = F;$n", []))

proc frameDestroy(p: PProc): Rope =
  result = p.indentLine rope(("framePtr = F.prev;") & "\L")

proc genProcBody(p: PProc, prc: PSym): Rope =
  if hasFrameInfo(p):
    let name =
      if sfModuleInit in prc.flags:
        makeJSString("module " & p.module.module.name.s)
      else:
        makeJSString(prc.owner.name.s & '.' & prc.name.s)

    result = frameCreate(p,
              name,
              makeJSString(toFilenameOption(p.config, prc.info.fileIndex, foStacktrace)))
  else:
    result = ""

  result.add(p.body)
  if prc.typ.callConv == ccSysCall:
    result = ("try {$n$1} catch (e) {$n" &
      " alert(\"Unhandled exception:\\n\" + e.message + \"\\n\"$n}") % [result]
  if hasFrameInfo(p):
    result.add(frameDestroy(p))

proc optionalLine(p: Rope): Rope =
  if p == "":
    return p
  else:
    return p & "\L"

proc startProc*(g: PGlobals, module: BModule, id: ProcedureId,
                body: sink Body): PProc =
  let prc = g.env[id]
  let p = newProc(g, module, prc, prc.options)
  p.fullBody = body

  synchronize(p.locals, p.fullBody.locals)
  if p.fullBody.code != nil:
    analyseIfAddressTaken(p.fullBody.code, p.addrTaken)

  # make sure the procedure has a mangled name:
  discard ensureMangledName(p, id)

  # setup the loc for the the result variable:
  if prc.typ[0] != nil and sfPure notin prc.flags:
    setupLocalLoc(p, resultId, skResult)

  # setup the locs for the parameters:
  for i in 1..<prc.typ.n.len:
    let param = prc.typ.n[i].sym
    if not isCompileTimeOnly(param.typ):
      setupLocalLoc(p, LocalId(i), skParam)

  if prc.typ.callConv == ccClosure:
    # set the name for the hidden environment parameter. When creating a
    # closure object, the environment is bound to the ``this`` argument of the
    # function, hence using ``this`` for the name
    assert prc.ast[paramsPos].lastSon.kind == nkSym,
           "the hidden parameter is missing"
    let s = prc.ast[paramsPos].lastSon.sym
    # parameter IDs start at 1
    setupLocalLoc(p, LocalId(s.position + 1), skParam, "this")

  result = p

proc finishProc*(p: PProc): string =
  let
    prc = p.prc

  var
    returnStmt = ""
    resultAsgn = ""

  if prc.typ[0] != nil and sfPure notin prc.flags:
    let
      loc {.cursor.} = p.locals[resultId]
      mname = loc.name
    if mapType(loc.typ) == etyBaseIndex and (stfBoxed notin loc.storage):
      resultAsgn = p.indentLine(("var $# = null;$n") % [mname])
      resultAsgn.add p.indentLine("var $#_Idx = 0;$n" % [mname])
    else:
      let resVar = createVar(p, loc.typ, isIndirect(loc))
      resultAsgn = p.indentLine(("var $# = $#;$n") % [mname, resVar])
    var a: TCompRes
    accessLoc(loc, a)
    if a.typ == etyBaseIndex:
      returnStmt = "return [$#, $#];$n" % [a.address, a.res]
    else:
      returnStmt = "return $#;$n" % [a.res]

  if optLineDir in p.config.options:
    result = lineDir(p.config, prc.info, toLinenumber(prc.info))

  let
    name   = p.g.procs[p.env.procedures[prc]]
    header = generateHeader(toOpenArray(p.locals.base, 1, prc.typ.len-1))

  var def: Rope
  if not prc.constraint.isNil:
    def = runtimeFormat(prc.constraint.strVal & " {$n$#$#$#$#",
            [ returnType,
              name,
              header,
              optionalLine(p.defs),
              optionalLine(resultAsgn),
              optionalLine(genProcBody(p, prc)),
              optionalLine(p.indentLine(returnStmt))])
  else:
    # if optLineDir in p.config.options:
      # result.add(~"\L")

    def = "\Lfunction $#($#) {$n$#$#$#$#" %
            [ name,
              header,
              optionalLine(p.defs),
              optionalLine(resultAsgn),
              optionalLine(genProcBody(p, prc)),
              optionalLine(p.indentLine(returnStmt))]

  dec p.extraIndent
  result.add p.indentLine(def)
  result.add p.indentLine(~"}$n")

  #if gVerbosity >= 3:
  #  echo "END   generated code for: " & prc.name.s

proc genStmts(p: PProc, stmts: openArray[CgNode]) =
  let structs = toStructureList(stmts)
  if structs.len == 0:
    # there are no control-flow constructs in the body
    for it in stmts.items:
      genStmt(p, it)
    return

  template gen(a, b: int) =
    for i in a..<b:
      genStmt(p, stmts[i])

  # generate code for the statements up to the first control-flow construct:
  gen(0, structs[0].stmt)

  # code generation is driven by the control-flow constructs. Indentation is
  # also (mostly) managed here
  for i, it in structs.pairs:
    case it.kind
    of stkTry:
      startBlock(p, "try {$n", [])
      gen(it.stmt, structs[i+1].stmt)
    of stkBlock:
      startBlock(p, "Label$1: {$n", [$it.label.int])
      gen(it.stmt, structs[i+1].stmt)
    of stkStructStart:
      # indentation is managed by ``genStmt`` here
      gen(it.stmt, structs[i+1].stmt)
    of stkCatch:
      endBlock(p)
      startBlock(p, "catch(EXCEPTION) {$n")
      gen(it.stmt, structs[i+1].stmt)
    of stkFinally:
      endBlock(p)
      startBlock(p, "finally {$n", [])
      gen(it.stmt, structs[i+1].stmt)
    of stkTerminator:
      genStmt(p, stmts[it.stmt])
      # the statements immediately following the terminator are dead code,
      # ignore them
    of stkEnd:
      endBlock(p)
      # an 'end' can be the last item in the list
      gen(it.stmt):
        if i < structs.high: structs[i+1].stmt
        else:                stmts.len

proc genProc*(g: PGlobals, module: BModule, id: ProcedureId,
              body: sink Body): Rope =
  var p = startProc(g, module, id, body)
  p.nested: genStmts(p, p.fullBody.code.kids)
  result = finishProc(p)

proc genPartial*(p: PProc, n: CgNode) =
  ## Generates the JavaScript code for `n` and appends the result to `p`. This
  ## is intended for CG IR that wasn't already available when calling
  ## `startProc`.
  synchronize(p.locals, p.fullBody.locals)
  analyseIfAddressTaken(p.fullBody.code, p.addrTaken)
  genStmts(p, n.kids)

proc rdData(p: PProc, data: DataId, typ: PType): TCompRes =
  ## Returns the loc for the `data` of type `typ`. Emits the definition for
  ## `data` if it hasn't been already.
  if not containsOrIncl(p.g.dataGenerated, data.int):
    let val = gen(p, translate(p.env[data]))
    # emit the definition into the constants section:
    p.g.constants.addf("var Data$1 = $2;$n", [$ord(data), val.res])

  TCompRes(res: "Data$1" % [$ord(data)],
           typ: mapType(typ),
           kind: resExpr)

proc genStmt(p: PProc, n: CgNode) =
  var r: TCompRes
  gen(p, n, r)
  if r.res != "": lineF(p, "$#;$n", [r.res])

proc genCast(p: PProc, n: CgNode, r: var TCompRes) =
  var dest = skipTypes(n.typ, abstractVarRange)
  var src = skipTypes(n.operand.typ, abstractVarRange)
  gen(p, n.operand, r)
  if dest.kind == src.kind:
    # no-op conversion
    return
  let toInt = (dest.kind in tyInt..tyInt32)
  let toUint = (dest.kind in tyUInt..tyUInt32)
  let fromInt = (src.kind in tyInt..tyInt32)
  let fromUint = (src.kind in tyUInt..tyUInt32)

  if toUint and (fromInt or fromUint):
    let trimmer = unsignedTrimmer(dest.size)
    r.res = "($1 $2)" % [r.res, trimmer]
  elif toInt and (fromInt or fromUint):
    if fromInt:
      return
    elif fromUint:
      if src.size == 4 and dest.size == 4:
        # XXX prevent multi evaluations
        r.res = "($1 | 0)" % [r.res]
      else:
        let trimmer = unsignedTrimmer(dest.size)
        let minuend = case dest.size
          of 1: "0xfe"
          of 2: "0xfffe"
          of 4: "0xfffffffe"
          else: ""
        r.res = "($1 - ($2 $3))" % [rope minuend, r.res, trimmer]
  elif dest.kind == tyPointer:
    # cast into pointer
    if r.typ == etyBaseIndex:
      discard "already a fat pointer, do nothing"
    else:
      r.address = r.res
      r.res = ~"null"
      r.typ = etyBaseIndex
  elif src.kind == tyPointer:
    # cast from pointer
    let d = mapType(dest)
    if d == etyBaseIndex:
      discard "already a fat pointer, do nothing"
    else:
      r.res = r.address
      r.address = "" # clear out the address
      r.typ = d

proc gen(p: PProc, n: CgNode, r: var TCompRes) =
  r.typ = etyNone
  if r.kind != resCallee: r.kind = resNone
  #r.address = nil
  r.res = ""

  case n.kind
  of cnkProc:
    let s = p.env[n.prc]
    if sfCompileTime in s.flags:
      localReport(p.config, n.info, reportSym(
        rsemCannotCodegenCompiletimeProc, s))

    r.res = ensureMangledName(p, n.prc)
  of cnkConst:
    if isAnon(n.cnst):
      r = rdData(p, p.env.dataFor(n.cnst), n.typ)
    else:
      # XXX: properly use ``accessLoc``
      r.res = p.g.consts[n.cnst].name
  of cnkGlobal:
    accessLoc(p.g.globals[n.global], r)
  of cnkLocal:
    accessLoc(p.locals[n.local], r)
  of cnkIntLit, cnkUIntLit:
    r.res = intLiteral(getInt(n), n.typ)
    r.kind = resExpr
  of cnkNilLit:
    if mapType(n.typ) == etyBaseIndex:
      r.typ = etyBaseIndex
      r.address = rope"null"
      r.res = rope"0"
      r.kind = resExpr
    else:
      r.res = rope"null"
      r.kind = resExpr
  of cnkStrLit:
    if skipTypes(n.typ, abstractVarRange).kind == tyString:
      if n.strVal.len != 0:
        useMagic(p, "makeNimstrLit")
        r.res = "makeNimstrLit($1)" % [makeJSString(n.strVal)]
      else:
        r.res = rope"[]"
    else:
      r.res = makeJSString(n.strVal, false)
    r.kind = resExpr
  of cnkFloatLit:
    let f = n.floatVal
    case classify(f)
    of fcNan:
      if signbit(f):
        r.res = rope"-NaN"
      else:
        r.res = rope"NaN"
    of fcNegZero:
      r.res = rope"-0.0"
    of fcZero:
      r.res = rope"0.0"
    of fcInf:
      r.res = rope"Infinity"
    of fcNegInf:
      r.res = rope"-Infinity"
    else:
      if n.typ.skipTypes(abstractRange).kind == tyFloat32:
        r.res.addFloatRoundtrip(f.float32)
      else:
        r.res.addFloatRoundtrip(f)
    r.kind = resExpr
  of cnkCall, cnkCheckedCall:
    if isEmptyType(n.typ):
      genLineDir(p, n)
    if getCalleeMagic(p.g.env, n[0]) != mNone:
      genMagic(p, n, r)
    elif n[0].kind == cnkProc and sfInfixCall in p.env[n[0].prc].flags:
      genInfixCall(p, n, r)
    else:
      genCall(p, n, r)
  of cnkNeg:
    let x = gen(p, n[0])
    r.res = "(-$1)" % rdLoc(x)
    r.typ = mapType(n.typ)
    r.kind = resExpr
  of cnkAdd: binaryExpr(p, n[0], n[1], r, "($1 + $2)")
  of cnkSub: binaryExpr(p, n[0], n[1], r, "($1 - $2)")
  of cnkMul: binaryExpr(p, n[0], n[1], r, "($1 * $2)")
  of cnkDiv:
    if mapType(n.typ) == etyFloat:
      binaryExpr(p, n[0], n[1], r, "($1 / $2)")
    else:
      binaryExpr(p, n[0], n[1], r, "Math.trunc($1 / $2)")
  of cnkModI: binaryExpr(p, n[0], n[1], r, "Math.trunc($1 % $2)")
  of cnkClosureConstr:
    useMagic(p, "makeClosure")
    var tmp1, tmp2: TCompRes
    gen(p, n[0], tmp1)
    gen(p, n[1], tmp2)
    r.res = "makeClosure($1, $2)" % [tmp1.rdLoc, tmp2.rdLoc]
    r.kind = resExpr
  of cnkSetConstr: genSetConstr(p, n, r)
  of cnkArrayConstr: genArrayConstr(p, n, r)
  of cnkTupleConstr: genTupleConstr(p, n, r)
  of cnkObjConstr: genObjConstr(p, n, r)
  of cnkHiddenConv, cnkConv: genConv(p, n, r)
  of cnkLvalueConv:
    # non-object lvalue conversion are irrelevant to the JS backend
    gen(p, n.operand, r)
  of cnkToSlice:
    if n.len == 1:
      gen(p, n[0], r)
    else:
      # arr.slice([begin[, end]]): 'end' is exclusive
      var x, y, z: TCompRes
      gen(p, n[0], x)
      gen(p, n[1], y)
      gen(p, n[2], z)
      r.res = "($1.slice($2, $3 + 1))" % [x.rdLoc, y.rdLoc, z.rdLoc]
      r.kind = resExpr
  of cnkAddr, cnkHiddenAddr:
    if n.typ.kind == tyLent or mapType(n.typ) != etyBaseIndex:
      # the operation doesn't produce an address-like value (e.g. because the
      # operand is a JS object and those already have reference semantics).
      # ``lent T`` types are currently treated as normal, non-owning
      # locations, so the hidden address operation is skipped
      gen(p, n.operand, r)
    else:
      genAddr(p, n.operand, r)
  of cnkDeref, cnkDerefView:
    if n.typ.kind in {tyLent}:
      gen(p, n.operand, r)
    else:
      genDeref(p, n, r)
  of cnkArrayAccess: genArrayAccess(p, n, r)
  of cnkTupleAccess: genFieldAccess(p, n, r)
  of cnkFieldAccess: genFieldAccess(p, n, r)
  of cnkObjDownConv: downConv(p, n, r)
  of cnkObjUpConv: gen(p, n.operand, r)
  of cnkCast: genCast(p, n, r)
  of cnkEmpty: discard
  of cnkType: r.res = genTypeInfo(p, n.typ)
  of cnkDef: genDef(p, n)
  of cnkCaseStmt: genCaseJS(p, n)
  of cnkGotoStmt:
    case n[0].kind
    of cnkLabel:
      lineF(p, "break Label$1;$n", [$n[0].label])
    else:
      # jump directly to the final target. Placement of finallys made sure
      # that those are visited correctly
      lineF(p, "break Label$1;$n", [$n[0][^1].label])
  of cnkLoopJoinStmt:
    startBlock(p, "while (true) {$n")
  of cnkExcept:
    # emit an exception handler
    genExcept(p, n)
  of cnkIfStmt:
    genLineDir(p, n)
    var a: TCompRes
    gen(p, n[0], a)
    startBlock(p, "if ($1) {$n", [rdLoc(a)])
  of cnkFinally:
    # make sure the frame pointer is correct after unwinding
    if hasFrameInfo(p):
      lineF(p, "framePtr = F;$n", [])
  of cnkAsgn: genAsgn(p, n)
  of cnkFastAsgn: genFastAsgn(p, n)
  of cnkVoidStmt:
    genLineDir(p, n)
    var a: TCompRes
    gen(p, n[0], a)
    # wrap the expressions in parentheses so that they're not ambiguous with
    # statements
    if a.typ == etyBaseIndex:
      # make sure to evaluate both the address and index
      lineF(p, "($1); ($2);$n", [a.address, a.res])
    else:
      lineF(p, "($1);$n", [a.res])
  of cnkAsmStmt, cnkEmitStmt: genAsmOrEmitStmt(p, n)
  of cnkRaiseStmt: genRaiseStmt(p, n)
  of cnkJoinStmt, cnkEnd, cnkLoopStmt, cnkContinueStmt:
    discard "terminators or endings for which no special handling is needed"
  of cnkInvalid, cnkMagic, cnkRange, cnkBinding, cnkLeave, cnkTargetList,
     cnkResume, cnkBranch, cnkAstLit, cnkLabel, cnkStmtListExpr, cnkStmtList,
     cnkField, cnkLegacyNodes:
    internalError(p.config, n.info, "gen: unknown node type: " & $n.kind)

proc newModule*(g: ModuleGraph; module: PSym): BModule =
  new(result)
  result.module = module
  result.graph = g
  result.config = g.config

proc genHeader*(): Rope =
  result = rope("""/* Generated by the Nim Compiler v$1 */
    var framePtr = null;
    var excHandler = 0;
    var lastJSError = null;
  """.unindent.format(VersionAsString))

proc genTopLevelStmt*(globals: PGlobals, m: BModule, body: sink Body) =
  m.config.internalAssert(m.module != nil, body.code.info, "genTopLevelStmt")
  var p = newInitProc(globals, m)
  p.fullBody = body
  p.unique = globals.unique
  analyseIfAddressTaken(p.fullBody.code, p.addrTaken)
  genStmts(p, p.fullBody.code.kids)
  p.g.code.add(p.defs)
  p.g.code.add(p.body)

proc wholeCode*(globals: PGlobals): Rope =
  result = globals.typeInfo & globals.constants & globals.code
