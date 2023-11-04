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
    lineinfos,
    astmsgs,
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
    idioms,
    int128,
    nversion,
    ropes
  ],
  compiler/backend/[
    cgir,
    compat
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
                            # (see `maybeMakeTemp`)

  PGlobals* = ref object
    typeInfo, constants*, code*: Rope
    typeInfoGenerated: IntSet
    unique: int    # for temp identifier generation

    names: Table[int, string]
      ## maps a symbol IDs to the symbol's JavaScript name

    extra*: seq[PSym]

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
    beforeRetNeeded: bool
    unique: int    # for temp identifier generation
    blocks: seq[int]
      ## the stack of enclosing blocks, indexed by ``BlockId``. Each entry
      ## stores the number to use for the label name
    extraIndent: int

    locals: OrdinalSeq[LocalId, Loc]
      ## stores all relevant code generator state for the procedure's
      ## locals

const
  sfModuleInit* = sfMainModule
    ## the procedure is the 'init' procedure of a module

  NonMagics* = { mDotDot }
    ## magics that are treated like normal procedures by the code
    ## generator

# forward declarations:
proc setupLocalLoc(p: PProc, id: LocalId, kind: TSymKind; name = "")

template config*(p: PProc): ConfigRef = p.module.config

proc indentLine(p: PProc, r: Rope): Rope =
  result = r
  for i in 0..<p.blocks.len + p.extraIndent:
    prepend(result, rope"  ")

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
    blocks: @[],
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

proc mangledName(p: PProc, s: PSym, info: TLineInfo): lent string =
  ## Returns the cached JavaScript name for `s`.
  p.module.config.internalAssert(s.id in p.g.names, info):
    "symbol has no generated name: " & s.name.s
  result = p.g.names[s.id]

proc ensureMangledName(p: PProc, s: PSym): lent string =
  ## Looks up and returns the mangled name for the non-local symbol
  ## `s`, generating and caching the mangled name if it hasn't been
  ## already.
  let n = addr p.g.names.mgetOrPut(s.id, "")
  if n[].len == 0:
    # the mangled named hasn't been generated yet
    n[] = mangleName(p.module, s)

  result = p.g.names[s.id]

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

proc useMagic(p: PProc, name: string) =
  if name.len == 0: return
  var s = magicsys.getCompilerProc(p.module.graph, name)
  if s != nil:
    p.g.extra.add s
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
  TMagicFrmt = array[0..1, string]
  TMagicOps = array[mAddI..mStrToStr, TMagicFrmt]

const # magic checked op; magic unchecked op;
  jsMagics: TMagicOps = [
    mAddI: ["addInt", ""],
    mSubI: ["subInt", ""],
    mMulI: ["mulInt", ""],
    mDivI: ["divInt", ""],
    mModI: ["modInt", ""],
    mSucc: ["addInt", ""],
    mPred: ["subInt", ""],
    mAddF64: ["", ""],
    mSubF64: ["", ""],
    mMulF64: ["", ""],
    mDivF64: ["", ""],
    mShrI: ["", ""],
    mShlI: ["", ""],
    mAshrI: ["", ""],
    mBitandI: ["", ""],
    mBitorI: ["", ""],
    mBitxorI: ["", ""],
    mMinI: ["nimMin", "nimMin"],
    mMaxI: ["nimMax", "nimMax"],
    mAddU: ["", ""],
    mSubU: ["", ""],
    mMulU: ["", ""],
    mDivU: ["", ""],
    mModU: ["", ""],
    mEqI: ["", ""],
    mLeI: ["", ""],
    mLtI: ["", ""],
    mEqF64: ["", ""],
    mLeF64: ["", ""],
    mLtF64: ["", ""],
    mLeU: ["", ""],
    mLtU: ["", ""],
    mEqEnum: ["", ""],
    mLeEnum: ["", ""],
    mLtEnum: ["", ""],
    mEqCh: ["", ""],
    mLeCh: ["", ""],
    mLtCh: ["", ""],
    mEqB: ["", ""],
    mLeB: ["", ""],
    mLtB: ["", ""],
    mEqRef: ["", ""],
    mLePtr: ["", ""],
    mLtPtr: ["", ""],
    mXor: ["", ""],
    mEqCString: ["", ""],
    mEqProc: ["", ""],
    mUnaryMinusI: ["negInt", ""],
    mUnaryMinusI64: ["negInt64", ""],
    mAbsI: ["absInt", ""],
    mNot: ["", ""],
    mUnaryPlusI: ["", ""],
    mBitnotI: ["", ""],
    mUnaryPlusF64: ["", ""],
    mUnaryMinusF64: ["", ""],
    mCharToStr: ["nimCharToStr", "nimCharToStr"],
    mBoolToStr: ["nimBoolToStr", "nimBoolToStr"],
    mIntToStr: ["cstrToNimstr", "cstrToNimstr"],
    mInt64ToStr: ["cstrToNimstr", "cstrToNimstr"],
    mFloatToStr: ["cstrToNimstr", "cstrToNimstr"],
    mCStrToStr: ["cstrToNimstr", "cstrToNimstr"],
    mStrToStr: ["", ""]]

proc needsTemp(n: CgNode): bool =
  # check if n contains a call to determine
  # if a temp should be made to prevent multiple evals
  case n.kind
  of cnkCall, cnkTupleConstr, cnkObjConstr, cnkArrayConstr, cnkSetConstr:
    result = true
  of cnkAtoms:
    result = false
  of cnkWithOperand:
    result = needsTemp(n.operand)
  else:
    for c in n.items:
      if needsTemp(c):
        return true

proc maybeMakeTemp(p: PProc, n: CgNode; x: TCompRes): tuple[a, tmp: Rope] =
  var
    a = x.rdLoc
    b = a
  if needsTemp(n):
    # if we have tmp just use it
    if x.tmpLoc != "" and (mapType(n.typ) == etyBaseIndex or n.kind in {cnkDerefView, cnkDeref}):
      b = "$1[0][$1[1]]" % [x.tmpLoc]
      (a: a, tmp: b)
    else:
      let tmp = p.getTemp
      b = tmp
      a = "($1 = $2, $1)" % [tmp, a]
      (a: a, tmp: b)
  else:
    (a: a, tmp: b)

proc maybeMakeTempAssignable(p: PProc, n: CgNode; x: TCompRes): tuple[a, tmp: Rope] =
  var
    a = x.rdLoc
    b = a
  if needsTemp(n):
    # if we have tmp just use it
    if x.tmpLoc != "" and (mapType(n.typ) == etyBaseIndex or n.kind in {cnkDerefView, cnkDeref}):
      b = "$1[0][$1[1]]" % [x.tmpLoc]
      result = (a: a, tmp: b)
    elif x.tmpLoc != "" and n.kind == cnkArrayAccess:
      # genArrayAddr
      var
        address, index: TCompRes
        first: Int128
      gen(p, n[0], address)
      gen(p, n[1], index)
      let (m1, tmp1) = maybeMakeTemp(p, n[0], address)
      let typ = skipTypes(n[0].typ, abstractPtrs)
      if typ.kind == tyArray:
        first = firstOrd(p.config, typ[0])
      if optBoundsCheck in p.options:
        useMagic(p, "chckIndx")
        if first == 0: # save a couple chars
          index.res = "chckIndx($1, 0, ($2).length - 1)" % [index.res, tmp1]
        else:
          index.res = "chckIndx($1, $2, ($3).length + ($2) - 1) - ($2)" % [
            index.res, rope(first), tmp1]
      elif first != 0:
        index.res = "($1) - ($2)" % [index.res, rope(first)]
      else:
        discard # index.res = index.res
      let (n1, tmp2) = maybeMakeTemp(p, n[1], index)
      result = (a: "$1[$2]" % [m1, n1], tmp: "$1[$2]" % [tmp1, tmp2])
    # could also put here: nkDotExpr -> genFieldAccess, nkCheckedFieldExpr -> genCheckedFieldOp
    # but the uses of maybeMakeTempAssignable don't need them
    else:
      result = (a: a, tmp: b)
  else:
    result = (a: a, tmp: b)

template binaryExpr(p: PProc, n: CgNode, r: var TCompRes, magic, frmt: string,
                    reassign = false) =
  # $1 and $2 in the `frmt` string bind to lhs and rhs of the expr,
  # if $3 or $4 are present they will be substituted with temps for
  # lhs and rhs respectively
  var x, y: TCompRes
  useMagic(p, magic)
  gen(p, n[1], x)
  gen(p, n[2], y)

  var
    a, tmp = x.rdLoc
    b, tmp2 = y.rdLoc
  when reassign:
    (a, tmp) = maybeMakeTempAssignable(p, n[1], x)
  else:
    when "$3" in frmt: (a, tmp) = maybeMakeTemp(p, n[1], x)
    when "$4" in frmt: (b, tmp2) = maybeMakeTemp(p, n[2], y)

  r.res = frmt % [a, b, tmp, tmp2]
  r.kind = resExpr

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
    let (a, tmp) = maybeMakeTempAssignable(p, n[1], x)
    r.res = "$1 = (($5 $2 $3) $4)" % [a, rope op, y.rdLoc, trimmer, tmp]
  else:
    r.res = "(($1 $2 $3) $4)" % [x.rdLoc, rope op, y.rdLoc, trimmer]
  r.kind = resExpr

template unaryExpr(p: PProc, n: CgNode, r: var TCompRes, magic, frmt: string) =
  # $1 binds to n[1], if $2 is present it will be substituted to a tmp of $1
  useMagic(p, magic)
  gen(p, n[1], r)
  var a, tmp = r.rdLoc
  if "$2" in frmt: (a, tmp) = maybeMakeTemp(p, n[1], r)
  r.res = frmt % [a, tmp]
  r.kind = resExpr

proc arithAux(p: PProc, n: CgNode, r: var TCompRes, op: TMagic) =
  var
    x, y: TCompRes
    xLoc,yLoc: Rope
  let i = ord(optOverflowCheck notin p.options)
  useMagic(p, jsMagics[op][i])
  if n.len > 2:
    gen(p, n[1], x)
    gen(p, n[2], y)
    xLoc = x.rdLoc
    yLoc = y.rdLoc
  else:
    gen(p, n[1], r)
    xLoc = r.rdLoc

  template applyFormat(frmt) =
    r.res = frmt % [xLoc, yLoc]
  template applyFormat(frmtA, frmtB) =
    if i == 0: applyFormat(frmtA) else: applyFormat(frmtB)

  case op:
  of mAddI: applyFormat("addInt($1, $2)", "($1 + $2)")
  of mSubI: applyFormat("subInt($1, $2)", "($1 - $2)")
  of mMulI: applyFormat("mulInt($1, $2)", "($1 * $2)")
  of mDivI: applyFormat("divInt($1, $2)", "Math.trunc($1 / $2)")
  of mModI: applyFormat("modInt($1, $2)", "Math.trunc($1 % $2)")
  of mSucc: applyFormat("addInt($1, $2)", "($1 + $2)")
  of mPred: applyFormat("subInt($1, $2)", "($1 - $2)")
  of mAddF64: applyFormat("($1 + $2)", "($1 + $2)")
  of mSubF64: applyFormat("($1 - $2)", "($1 - $2)")
  of mMulF64: applyFormat("($1 * $2)", "($1 * $2)")
  of mDivF64: applyFormat("($1 / $2)", "($1 / $2)")
  of mShrI: applyFormat("", "")
  of mShlI:
    if n[1].typ.size <= 4:
      applyFormat("($1 << $2)", "($1 << $2)")
    else:
      applyFormat("($1 * Math.pow(2, $2))", "($1 * Math.pow(2, $2))")
  of mAshrI:
    if n[1].typ.size <= 4:
      applyFormat("($1 >> $2)", "($1 >> $2)")
    else:
      applyFormat("Math.floor($1 / Math.pow(2, $2))", "Math.floor($1 / Math.pow(2, $2))")
  of mBitandI: applyFormat("($1 & $2)", "($1 & $2)")
  of mBitorI: applyFormat("($1 | $2)", "($1 | $2)")
  of mBitxorI: applyFormat("($1 ^ $2)", "($1 ^ $2)")
  of mMinI: applyFormat("nimMin($1, $2)", "nimMin($1, $2)")
  of mMaxI: applyFormat("nimMax($1, $2)", "nimMax($1, $2)")
  of mAddU: applyFormat("", "")
  of mSubU: applyFormat("", "")
  of mMulU: applyFormat("", "")
  of mDivU: applyFormat("", "")
  of mModU: applyFormat("($1 % $2)", "($1 % $2)")
  of mEqI: applyFormat("($1 == $2)", "($1 == $2)")
  of mLeI: applyFormat("($1 <= $2)", "($1 <= $2)")
  of mLtI: applyFormat("($1 < $2)", "($1 < $2)")
  of mEqF64: applyFormat("($1 == $2)", "($1 == $2)")
  of mLeF64: applyFormat("($1 <= $2)", "($1 <= $2)")
  of mLtF64: applyFormat("($1 < $2)", "($1 < $2)")
  of mLeU: applyFormat("($1 <= $2)", "($1 <= $2)")
  of mLtU: applyFormat("($1 < $2)", "($1 < $2)")
  of mEqEnum: applyFormat("($1 == $2)", "($1 == $2)")
  of mLeEnum: applyFormat("($1 <= $2)", "($1 <= $2)")
  of mLtEnum: applyFormat("($1 < $2)", "($1 < $2)")
  of mEqCh: applyFormat("($1 == $2)", "($1 == $2)")
  of mLeCh: applyFormat("($1 <= $2)", "($1 <= $2)")
  of mLtCh: applyFormat("($1 < $2)", "($1 < $2)")
  of mEqB: applyFormat("($1 == $2)", "($1 == $2)")
  of mLeB: applyFormat("($1 <= $2)", "($1 <= $2)")
  of mLtB: applyFormat("($1 < $2)", "($1 < $2)")
  of mEqRef: applyFormat("($1 == $2)", "($1 == $2)")
  of mLePtr: applyFormat("($1 <= $2)", "($1 <= $2)")
  of mLtPtr: applyFormat("($1 < $2)", "($1 < $2)")
  of mXor: applyFormat("($1 != $2)", "($1 != $2)")
  of mEqCString: applyFormat("($1 == $2)", "($1 == $2)")
  of mEqProc: applyFormat("($1 == $2)", "($1 == $2)")
  of mUnaryMinusI: applyFormat("negInt($1)", "-($1)")
  of mUnaryMinusI64: applyFormat("negInt64($1)", "-($1)")
  of mAbsI: applyFormat("absInt($1)", "Math.abs($1)")
  of mNot: applyFormat("!($1)", "!($1)")
  of mUnaryPlusI: applyFormat("+($1)", "+($1)")
  of mBitnotI: applyFormat("~($1)", "~($1)")
  of mUnaryPlusF64: applyFormat("+($1)", "+($1)")
  of mUnaryMinusF64: applyFormat("-($1)", "-($1)")
  of mCharToStr: applyFormat("nimCharToStr($1)", "nimCharToStr($1)")
  of mBoolToStr: applyFormat("nimBoolToStr($1)", "nimBoolToStr($1)")
  of mIntToStr: applyFormat("cstrToNimstr(($1) + \"\")", "cstrToNimstr(($1) + \"\")")
  of mInt64ToStr: applyFormat("cstrToNimstr(($1) + \"\")", "cstrToNimstr(($1) + \"\")")
  of mCStrToStr: applyFormat("cstrToNimstr($1)", "cstrToNimstr($1)")
  of mStrToStr, mIsolate: applyFormat("$1", "$1")
  else:
    assert false, $op

proc arith(p: PProc, n: CgNode, r: var TCompRes, op: TMagic) =
  case op
  of mAddU: binaryUintExpr(p, n, r, "+")
  of mSubU: binaryUintExpr(p, n, r, "-")
  of mMulU: binaryUintExpr(p, n, r, "*")
  of mDivU:
    binaryUintExpr(p, n, r, "/")
    if n[1].typ.skipTypes(abstractRange).size == 8:
      r.res = "Math.trunc($1)" % [r.res]
  of mDivI:
    arithAux(p, n, r, op)
  of mModI:
    arithAux(p, n, r, op)
  of mShrI:
    var x, y: TCompRes
    gen(p, n[1], x)
    gen(p, n[2], y)
    r.res = "($1 >>> $2)" % [x.rdLoc, y.rdLoc]
  of mCharToStr, mBoolToStr, mIntToStr, mInt64ToStr, mCStrToStr, mStrToStr, mEnumToStr:
    arithAux(p, n, r, op)
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

proc genRepeatStmt(p: PProc, n: CgNode) =
  internalAssert p.config, isEmptyType(n.typ)
  genLineDir(p, n)
  lineF(p, "while (true) {$n")
  p.nested: genStmt(p, n[0])
  lineF(p, "}$n")

proc genTry(p: PProc, n: CgNode) =
  # code to generate:
  #
  #  ++excHandler;
  #  var tmpFramePtr = framePtr;
  #  try {
  #    stmts;
  #    --excHandler;
  #  } catch (EXCEPTION) {
  #    var prevJSError = lastJSError; lastJSError = EXCEPTION;
  #    framePtr = tmpFramePtr;
  #    --excHandler;
  #    if (e.typ && e.typ == NTI433 || e.typ == NTI2321) {
  #      stmts;
  #    } else if (e.typ && e.typ == NTI32342) {
  #      stmts;
  #    } else {
  #      stmts;
  #    }
  #    lastJSError = prevJSError;
  #  } finally {
  #    framePtr = tmpFramePtr;
  #    stmts;
  #  }
  genLineDir(p, n)
  inc(p.unique)
  var i = 1
  var catchBranchesExist = n.len > 1 and n[i].kind == cnkExcept
  if catchBranchesExist:
    p.body.add("++excHandler;\L")
  var tmpFramePtr = rope"F"
  if optStackTrace notin p.options:
    tmpFramePtr = p.getTemp(true)
    line(p, tmpFramePtr & " = framePtr;\L")
  lineF(p, "try {$n", [])
  genStmt(p, n[0])
  var generalCatchBranchExists = false
  if catchBranchesExist:
    p.body.addf("--excHandler;$n} catch (EXCEPTION) {$n var prevJSError = lastJSError;$n" &
        " lastJSError = EXCEPTION;$n --excHandler;$n", [])
    line(p, "framePtr = $1;$n" % [tmpFramePtr])
  while i < n.len and n[i].kind == cnkExcept:
    if n[i].len == 1:
      # general except section:
      generalCatchBranchExists = true
      if i > 1: lineF(p, "else {$n", [])
      genStmt(p, n[i][0])
      if i > 1: lineF(p, "}$n", [])
    else:
      var orExpr = ""
      var excAlias: CgNode = nil

      useMagic(p, "isObj")
      for j in 0..<n[i].len - 1:
        var throwObj: CgNode
        let it = n[i][j]

        if it.kind == cnkBinding:
          throwObj = it[0]
          excAlias = it[1]
          # If this is a ``except exc as sym`` branch there must be no following
          # nodes
          doAssert orExpr == ""
        else:
          p.config.internalAssert(it.kind == cnkType, n.info, "genTryStmt")
          throwObj = it

        if orExpr != "": orExpr.add("||")
        # Generate the correct type checking code depending on whether this is a
        # NIM-native or a JS-native exception
        # if isJsObject(throwObj.typ):
        if isImportedException(throwObj.typ, p.config):
          orExpr.addf("lastJSError instanceof $1",
            [throwObj.typ.sym.extname])
        else:
          orExpr.addf("isObj(lastJSError.m_type, $1)",
               [genTypeInfo(p, throwObj.typ)])

      if i > 1: line(p, "else ")
      lineF(p, "if (lastJSError && ($1)) {$n", [orExpr])
      # If some branch requires a local alias introduce it here. This is needed
      # since JS cannot do ``catch x as y``.
      if excAlias != nil:
        setupLocalLoc(p, excAlias.local, skVar)
        lineF(p, "var $1 = lastJSError;$n", p.locals[excAlias.local].name)
      genStmt(p, n[i][^1])
      lineF(p, "}$n", [])
    inc(i)
  if catchBranchesExist:
    if not generalCatchBranchExists:
      useMagic(p, "reraiseException")
      line(p, "else {\L")
      line(p, "\treraiseException();\L")
      line(p, "}\L")
    lineF(p, "lastJSError = prevJSError;$n")
  line(p, "} finally {\L")
  line(p, "framePtr = $1;$n" % [tmpFramePtr])
  if i < n.len and n[i].kind == cnkFinally:
    genStmt(p, n[i][0])
  line(p, "}\L")

proc genRaiseStmt(p: PProc, n: CgNode) =
  if n[0].kind != cnkEmpty:
    var a: TCompRes
    gen(p, n[0], a)
    let typ = skipTypes(n[0].typ, abstractPtrs)
    genLineDir(p, n)
    useMagic(p, "raiseException")
    lineF(p, "raiseException($1, $2);$n",
             [a.rdLoc, makeJSString(typ.sym.name.s)])
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
        genStmt(p, lastSon(it))
        lineF(p, "break;$n", [])
    else:
      lineF(p, "default: $n", [])
      p.nested:
        genStmt(p, it[0])
        lineF(p, "break;$n", [])
  lineF(p, "}$n", [])

proc genBlock(p: PProc, n: CgNode) =
  inc(p.unique)
  let labl = p.unique
  lineF(p, "Label$1: {$n", [labl.rope])
  p.blocks.add labl # push a new block
  genStmt(p, n[1])
  p.blocks.setLen(p.blocks.len - 1) # pop the block from the stack
  lineF(p, "}$n", [labl.rope])

proc genBreakStmt(p: PProc, n: CgNode) =
  genLineDir(p, n)
  lineF(p, "break Label$1;$n", [$p.blocks[n[0].label.int]])

proc genAsmOrEmitStmt(p: PProc, n: CgNode) =
  genLineDir(p, n)
  p.body.add p.indentLine("")
  for i in 0..<n.len:
    let it = n[i]
    case it.kind
    of cnkStrLit:
      p.body.add(it.strVal)
    of cnkSym, cnkLocal:
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

proc genIf(p: PProc, n: CgNode) =
  let it = n

  var cond: TCompRes
  p.nested: gen(p, it[0], cond)
  lineF(p, "if ($1) {$n", [cond.rdLoc])
  genStmt(p, it[1])
  lineF(p, "}$n", [])

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
  nodeKindsNeedNoCopy = cnkLiterals + {cnkStringToCString,
    cnkObjConstr, cnkTupleConstr, cnkArrayConstr,
    cnkCStringToString, cnkCall}

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
      # supports proc getF(): var T
      if x.kind in {cnkDerefView, cnkDeref} and x.operand.kind == cnkCall:
          lineF(p, "nimCopy($1, $2, $3);$n",
                [a.res, b.res, genTypeInfo(p, x.typ)])
      else:
        lineF(p, "$1 = nimCopy($1, $2, $3);$n",
              [a.res, b.res, genTypeInfo(p, x.typ)])
  of etyBaseIndex:
    if a.typ != etyBaseIndex or b.typ != etyBaseIndex:
      if y.kind == cnkCall:
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
  of cnkSym:    result = f.sym.position
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
    p.config.internalAssert(b[1].kind == cnkSym, b[1].info, "genFieldAddr")
    r.res = makeJSString(ensureMangledName(p, b[1].sym))
  else:
    unreachable(n.kind)
  internalAssert p.config, a.typ != etyBaseIndex
  r.address = a.res
  r.kind = resExpr

proc genFieldAccess(p: PProc, n: CgNode, r: var TCompRes) =
  gen(p, n[0], r)
  r.typ = mapType(n.typ)

  template mkTemp(i: int) =
    if r.typ == etyBaseIndex:
      if needsTemp(n[i]):
        let tmp = p.getTemp
        r.address = "($1 = $2, $1)[0]" % [tmp, r.res]
        r.res = "$1[1]" % [tmp]
        r.tmpLoc = tmp
      else:
        r.address = "$1[0]" % [r.res]
        r.res = "$1[1]" % [r.res]
  case n.kind
  of cnkTupleAccess:
    r.res = ("$1.Field$2") %
        [r.res, getFieldPosition(p, n[1]).rope]
    mkTemp(0)
  of cnkFieldAccess:
    p.config.internalAssert(n[1].kind == cnkSym, n[1].info, "genFieldAccess")
    r.res = "$1.$2" % [r.res, ensureMangledName(p, n[1].sym)]
    mkTemp(1)
  else:
    unreachable(n.kind)
  r.kind = resExpr

proc genAddr(p: PProc, n: CgNode, r: var TCompRes)

proc genCheckedFieldOp(p: PProc, n: CgNode, takeAddr: bool, r: var TCompRes) =
  internalAssert p.config, n.kind == cnkCheckedFieldAccess
  # nkDotExpr to access the requested field
  let accessExpr = n[0]
  # nkCall to check if the discriminant is valid
  var checkExpr = n[1]

  let negCheck = checkExpr[0].magic == mNot
  if negCheck:
    checkExpr = checkExpr[^1]

  # Field symbol
  var field = accessExpr[1].sym
  internalAssert p.config, field.kind == skField
  # Discriminant symbol
  let disc = checkExpr[2].sym
  internalAssert p.config, disc.kind == skField

  var setx: TCompRes
  gen(p, checkExpr[1], setx)

  var obj: TCompRes
  gen(p, accessExpr[0], obj)
  # Avoid evaluating the LHS twice (one to read the discriminant and one to read
  # the field)
  let tmp = p.getTemp()
  lineF(p, "var $1 = $2;$n", tmp, obj.res)

  useMagic(p, "raiseFieldError2")
  useMagic(p, "makeNimstrLit")
  useMagic(p, "reprDiscriminant") # no need to offset by firstOrd unlike for cgen
  let msg = genFieldDefect(p.config, field.name.s, disc)
  lineF(p, "if ($1[$2.$3]$4undefined) { raiseFieldError2(makeNimstrLit($5), reprDiscriminant($2.$3, $6)); }$n",
    setx.res, tmp, ensureMangledName(p, disc), if negCheck: ~"!==" else: ~"===",
    makeJSString(msg), genTypeInfo(p, disc.typ))

  let name = ensureMangledName(p, field)
  if takeAddr:
    r.typ = etyBaseIndex
    r.res = makeJSString(name)
    r.address = tmp
  else:
    r.typ = etyNone
    r.res = "$1.$2" % [tmp, name]
  r.kind = resExpr

proc genArrayAddr(p: PProc, n: CgNode, r: var TCompRes) =
  var
    a, b: TCompRes
    first: Int128
  r.typ = etyBaseIndex
  let m = if n.kind == cnkHiddenAddr: n.operand else: n
  gen(p, m[0], a)
  gen(p, m[1], b)
  #internalAssert p.config, a.typ != etyBaseIndex and b.typ != etyBaseIndex
  let (x, tmp) = maybeMakeTemp(p, m[0], a)
  r.address = x
  var typ = skipTypes(m[0].typ, abstractPtrs)
  if typ.kind == tyArray:
    first = firstOrd(p.config, typ[0])
  if optBoundsCheck in p.options:
    useMagic(p, "chckIndx")
    if first == 0: # save a couple chars
      r.res = "chckIndx($1, 0, ($2).length - 1)" % [b.res, tmp]
    else:
      r.res = "chckIndx($1, $2, ($3).length + ($2) - 1) - ($2)" % [
        b.res, rope(first), tmp]
  elif first != 0:
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
    if needsTemp(n[0]):
      let tmp = p.getTemp
      r.address = "($1 = $2, $1)[0]" % [tmp, r.rdLoc]
      r.res = "$1[1]" % [tmp]
      r.tmpLoc = tmp
    else:
      let x = r.rdLoc
      r.address = "$1[0]" % [x]
      r.res = "$1[1]" % [x]
  else:
    r.res = "$1[$2]" % [r.address, r.res]
  r.kind = resExpr

func isBoxedPointer(v: PSym): bool =
  ## Returns whether `v` stores a pointer value boxed in an array. If not,
  ## the value is stored as two variables (base and index).
  sfGlobal in v.flags

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

proc addrLoc[T: Loc|PSym](name: string, s: T, r: var TCompRes) =
  ## Generates the code for taking the address of the location identified by
  ## symbol node `n`
  if true:
    r.kind = resExpr
    r.typ = etyBaseIndex
    r.res = "0"
    if isIndirect(s):
      r.address = name
    elif mapType(s.typ) == etyBaseIndex and not isBoxedPointer(s):
      # box the separate base+index into an array first
      r.address = "[[$1, $1_Idx]]" % name
    else:
      # something that doesn't directly support having its address
      # taken (e.g. imported variable, parameter, let, etc.)
      r.address = "[$1]" % name

proc genAddr(p: PProc, n: CgNode, r: var TCompRes) =
  ## Dispatches to the appropriate procedure for generating the address-of
  ## operation based on the kind of node `n`
  case n.kind
  of cnkLocal:
    addrLoc(p.locals[n.local].name, p.locals[n.local], r)
  of cnkSym:
    addrLoc(mangledName(p, n.sym, n.info), n.sym, r)
  of cnkCheckedFieldAccess:
    genCheckedFieldOp(p, n, takeAddr=true, r)
  of cnkFieldAccess, cnkTupleAccess:
    genFieldAddr(p, n, r)
  of cnkArrayAccess:
    genArrayAddr(p, n, r)
  of cnkDerefView, cnkDeref:
    # attemping to take the address of a deref expression -> skip both the
    # addr and deref
    gen(p, n.operand, r)
  of cnkConv:
    # an explicit lvalue conversion. Conversion between lvalues of different
    # underlying type is not possible, so we simply skip the conversion and
    # apply the operation to the source expression
    genAddr(p, n.operand, r)
  of cnkStmtListExpr:
    for i in 0..<n.len-1:
      genStmt(p, n[i])

    genAddr(p, n[^1], r)
  of cnkCall:
    if n.typ.kind == tyOpenArray:
      # 'var openArray' for instance produces an 'addr' but this is harmless:
      # namely toOpenArray(a, 1, 3)
      gen(p, n, r)
    else:
      internalError(p.config, n.info, "genAddr: " & $n.kind)
  else:
    internalError(p.config, n.info, "genAddr: " & $n.kind)

proc accessLoc[T: Loc|PSym](name: string, s: T, r: var TCompRes) =
    let k = mapType(s.typ)
    if k == etyBaseIndex:
      r.typ = etyBaseIndex
      if isBoxedPointer(s):
        if isIndirect(s):
          r.address = "$1[0][0]" % [name]
          r.res = "$1[0][1]" % [name]
        else:
          r.address = "$1[0]" % [name]
          r.res = "$1[1]" % [name]
      else:
        r.address = name
        r.res = name & "_Idx"
    elif isIndirect(s):
      r.res = "$1[0]" % [name]
    else:
      r.res = name

proc genSym(p: PProc, n: CgNode, r: var TCompRes) =
  var s = n.sym
  case s.kind
  of skVar, skLet, skForVar:
    let name = mangledName(p, s, n.info)
    accessLoc(name, s, r)
  of skConst:
    r.res = mangledName(p, s, n.info)
  of skProc, skFunc, skConverter, skMethod, skIterator:
    if sfCompileTime in s.flags:
      localReport(p.config, n.info, reportSym(
        rsemCannotCodegenCompiletimeProc, s))

    r.res = ensureMangledName(p, s)
  else:
    unreachable()
  r.kind = resVal

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
  elif n.typ.kind in {tyVar, tyPtr, tyRef, tyLent} and
      n.kind == cnkCall and mapType(param.typ) == etyBaseIndex:
    # this fixes bug #5608:
    let tmp = getTemp(p)
    r.res.add("($1 = $2, $1[0]), $1[1]" % [tmp, a.rdLoc])
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

  for i in start..<n.len:
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
  if i >= n.len:
    globalReport(p.config, n.info, semReportCountMismatch(
      rsemExpectedParameterForJsPattern,
      expected = i,
      got = n.len - 1))

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
      for k in j..<n.len:
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
  let f = n[0].sym
  assert sfInfixCall in f.flags
  if true:
    let pat = f.extname
    internalAssert p.config, pat.len > 0
    if pat.contains({'#', '(', '@'}):
      var typ = skipTypes(n[0].typ, abstractInst)
      assert(typ.kind == tyProc)
      genPatternCall(p, n, pat, typ, r)
      return
  if n.len != 1:
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
  let n = n[1].skipConv
  internalAssert p.config, n.kind == cnkArrayConstr
  useMagic(p, "toJSStr") # Used in rawEcho
  useMagic(p, "rawEcho")
  r.res.add("rawEcho(")
  for i in 0..<n.len:
    let it = n[i]
    if it.typ.isCompileTimeOnly: continue
    if i > 0: r.res.add(", ")
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

proc storage(flags: TSymFlags, kind: TSymKind): StorageFlags =
  ## Computes and returns based on `flags` and `kind` the storage flags
  ## for a location.
  if {sfAddrTaken, sfGlobal} * flags != {}:
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
                storage: storage(p.fullBody[id].flags, kind))

  if name != "":
    # override with the provided name
    loc.name = name

  p.locals[id] = loc

proc defineGlobal*(globals: PGlobals, m: BModule, v: PSym) =
  ## Emits the definition for the single global `v` into the top-level section,
  ## with `m` being the module the global belongs to. Also sets up the
  ## symbol's JavaScript name.
  let p = newInitProc(globals, m)
  let name = mangleName(m, v)
  if exfNoDecl notin v.extFlags and sfImportc notin v.flags:
    lineF(p, "var $1 = $2;$n", [name, createVar(p, v.typ, isIndirect(v))])

  globals.names[v.id] = name
  # add to the top-level section:
  globals.code.add(p.body)

proc defineGlobals*(globals: PGlobals, m: BModule, vars: openArray[PSym]) =
  ## Emits definitions for the items in `vars` into the top-level section,
  ## with `m` being the module the globals belong to. Also sets up the
  ## JavaScript name for the globals.
  let p = newInitProc(globals, m)
    ## required for emitting code
  for v in vars.items:
    let name = mangleName(m, v)
    if exfNoDecl notin v.extFlags and sfImportc notin v.flags:
      lineF(p, "var $1 = $2;$n", [name, createVar(p, v.typ, isIndirect(v))])

    globals.names[v.id] = name

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

proc genConstant*(g: PGlobals, m: BModule, c: PSym) =
  let name = mangleName(m, c)
  if exfNoDecl notin c.extFlags:
    var p = newInitProc(g, m)
    #genLineDir(p, c.ast)
    genVarInit(p, c.typ, name, storage(c.flags, skConst), translate(c.ast))
    g.constants.add(p.body)

  # all constants need a name:
  g.names[c.id] = name

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
  r.res = createVar(p, n.typ, indirect = false)
  r.kind = resExpr

proc genReset(p: PProc, n: CgNode) =
  var x: TCompRes
  useMagic(p, "genericReset")
  gen(p, n[1], x)
  if x.typ == etyBaseIndex:
    lineF(p, "$1 = null, $2 = 0;$n", [x.address, x.res])
  else:
    let (a, tmp) = maybeMakeTempAssignable(p, n[1], x)
    lineF(p, "$1 = genericReset($3, $2);$n", [a,
                  genTypeInfo(p, n[1].typ), tmp])

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
  let op = getCalleeMagic(n[0])
  case op
  of mAddI..mStrToStr: arith(p, n, r, op)
  of mRepr: genRepr(p, n, r)
  of mAppendStrCh:
    binaryExpr(p, n, r, "addChar",
        "addChar($1, $2);")
  of mAppendStrStr:
    var lhs, rhs: TCompRes
    gen(p, n[1], lhs)
    gen(p, n[2], rhs)

    if skipTypes(n[1].typ, abstractVarRange).kind == tyCstring:
      let (b, tmp) = maybeMakeTemp(p, n[2], rhs)
      r.res = "if (null != $1) { if (null == $2) $2 = $3; else $2 += $3; }" %
        [b, lhs.rdLoc, tmp]
    else:
      let (a, tmp) = maybeMakeTemp(p, n[1], lhs)
      r.res = "$1.push.apply($3, $2);" % [a, rhs.rdLoc, tmp]
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
      let (a, tmp) = maybeMakeTemp(p, n[1], x)
      r.res = "(($1) == null ? 0 : ($2).length)" % [a, tmp]
    else:
      r.res = "($1).length" % [x.rdLoc]
    r.kind = resExpr
  of mHigh:
    var x: TCompRes
    gen(p, n[1], x)
    if skipTypes(n[1].typ, abstractInst).kind == tyCstring:
      let (a, tmp) = maybeMakeTemp(p, n[1], x)
      r.res = "(($1) == null ? -1 : ($2).length - 1)" % [a, tmp]
    else:
      r.res = "($1).length - 1" % [x.rdLoc]
    r.kind = resExpr
  of mInc:
    if n[1].typ.skipTypes(abstractRange).kind in {tyUInt..tyUInt64}:
      binaryUintExpr(p, n, r, "+", true)
    else:
      if optOverflowCheck notin p.options: binaryExpr(p, n, r, "", "$1 += $2")
      else: binaryExpr(p, n, r, "addInt", "$1 = addInt($3, $2)", true)
  of mDec:
    if n[1].typ.skipTypes(abstractRange).kind in {tyUInt..tyUInt64}:
      binaryUintExpr(p, n, r, "-", true)
    else:
      if optOverflowCheck notin p.options: binaryExpr(p, n, r, "", "$1 -= $2")
      else: binaryExpr(p, n, r, "subInt", "$1 = subInt($3, $2)", true)
  of mSetLengthStr:
    binaryExpr(p, n, r, "mnewString", "($1.length = $2)")
  of mSetLengthSeq:
    var x, y: TCompRes
    gen(p, n[1], x)
    gen(p, n[2], y)
    let t = skipTypes(n[1].typ, abstractVar)[0]
    let (a, tmp) = maybeMakeTemp(p, n[1], x)
    let (b, tmp2) = maybeMakeTemp(p, n[2], y)
    r.res = """if ($1.length < $2) { for (var i = $4.length ; i < $5 ; ++i) $4.push($3); }
               else { $4.length = $5; }""" % [a, b, createVar(p, t, false), tmp, tmp2]
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
      rsemConstExpressionExpected, n[0].sym))

  of mNewString: unaryExpr(p, n, r, "mnewString", "mnewString($1)")
  of mNewStringOfCap:
    unaryExpr(p, n, r, "mnewString", "mnewString(0)")
  of mDotDot:
    genCall(p, n, r)
  of mParseBiggestFloat:
    useMagic(p, "nimParseBiggestFloat")
    genCall(p, n, r)
  of mSlice:
    # arr.slice([begin[, end]]): 'end' is exclusive
    var x, y, z: TCompRes
    gen(p, n[1], x)
    gen(p, n[2], y)
    gen(p, n[3], z)
    r.res = "($1.slice($2, $3 + 1))" % [x.rdLoc, y.rdLoc, z.rdLoc]
    r.kind = resExpr
  of mMove:
    genMove(p, n, r)
  of mAccessEnv:
    unaryExpr(p, n, r, "accessEnv", "accessEnv($1)")
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
  # emit better code for constant sets:
  if isDeepConstExpr(n):
    inc(p.g.unique)
    let tmp = rope("ConstSet") & rope(p.g.unique)
    p.g.constants.addf("var $1 = $2;$n", [tmp, r.res])
    r.res = tmp

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
    var f = it[0].sym
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

proc convStrToCStr(p: PProc, n: CgNode, r: var TCompRes) =
  # we do an optimization here as this is likely to slow down
  # much of the code otherwise:
  if n.operand.kind == cnkCStringToString:
    gen(p, n.operand.operand, r)
  else:
    gen(p, n.operand, r)
    p.config.internalAssert(r.res != "", n.info, "convStrToCStr")
    useMagic(p, "toJSStr")
    r.res = "toJSStr($1)" % [r.res]
    r.kind = resExpr

proc convCStrToStr(p: PProc, n: CgNode, r: var TCompRes) =
  # we do an optimization here as this is likely to slow down
  # much of the code otherwise:
  if n.operand.kind == cnkStringToCString:
    gen(p, n.operand.operand, r)
  else:
    gen(p, n.operand, r)
    p.config.internalAssert(r.res != "", n.info, "convCStrToStr")
    useMagic(p, "cstrToNimstr")
    r.res = "cstrToNimstr($1)" % [r.res]
    r.kind = resExpr

proc genReturnStmt(p: PProc, n: CgNode) =
  p.config.internalAssert(p.prc != nil, n.info, "genReturnStmt")
  p.beforeRetNeeded = true
  genLineDir(p, n)
  lineF(p, "break BeforeRet;$n", [])

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
  if p.beforeRetNeeded:
    result.add p.indentLine(~"BeforeRet: {$n")
    result.add p.body
    result.add p.indentLine(~"}$n")
  else:
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

proc startProc*(g: PGlobals, module: BModule, prc: PSym, body: sink Body): PProc =
  let p = newProc(g, module, prc, prc.options)
  p.fullBody = body

  synchronize(p.locals, p.fullBody.locals)

  # make sure the procedure has a mangled name:
  discard ensureMangledName(p, prc)

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
    accessLoc(mname, loc, a)
    if a.typ == etyBaseIndex:
      returnStmt = "return [$#, $#];$n" % [a.address, a.res]
    else:
      returnStmt = "return $#;$n" % [a.res]

  if optLineDir in p.config.options:
    result = lineDir(p.config, prc.info, toLinenumber(prc.info))

  let
    name   = p.g.names[prc.id]
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

proc genProc*(g: PGlobals, module: BModule, prc: PSym,
              body: sink Body): Rope =
  var p = startProc(g, module, prc, body)
  p.nested: genStmt(p, p.fullBody.code)
  result = finishProc(p)

proc genPartial*(p: PProc, n: CgNode) =
  ## Generates the JavaScript code for `n` and appends the result to `p`. This
  ## is intended for CG IR that wasn't already available when calling
  ## `startProc`.
  synchronize(p.locals, p.fullBody.locals)
  genStmt(p, n)

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
  elif toInt:
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
  elif (src.kind == tyPtr and mapType(src) == etyObject) and dest.kind == tyPointer:
    r.address = r.res
    r.res = ~"null"
    r.typ = etyBaseIndex
  elif (dest.kind == tyPtr and mapType(dest) == etyObject) and src.kind == tyPointer:
    r.res = r.address
    r.typ = etyObject

proc gen(p: PProc, n: CgNode, r: var TCompRes) =
  r.typ = etyNone
  if r.kind != resCallee: r.kind = resNone
  #r.address = nil
  r.res = ""

  case n.kind
  of cnkSym:
    genSym(p, n, r)
  of cnkLocal:
    accessLoc(p.locals[n.local].name, p.locals[n.local], r)
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
  of cnkCall:
    if isEmptyType(n.typ):
      genLineDir(p, n)
    if getCalleeMagic(n[0]) != mNone:
      genMagic(p, n, r)
    elif n[0].kind == cnkSym and sfInfixCall in n[0].sym.flags and
        n.len >= 1:
      genInfixCall(p, n, r)
    else:
      genCall(p, n, r)
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
  of cnkCheckedFieldAccess: genCheckedFieldOp(p, n, takeAddr=false, r)
  of cnkObjDownConv: downConv(p, n, r)
  of cnkObjUpConv: gen(p, n.operand, r)
  of cnkCast: genCast(p, n, r)
  of cnkStringToCString: convStrToCStr(p, n, r)
  of cnkCStringToString: convCStrToStr(p, n, r)
  of cnkEmpty: discard
  of cnkType: r.res = genTypeInfo(p, n.typ)
  of cnkStmtList, cnkStmtListExpr:
    # this shows the distinction is nice for backends and should be kept
    # in the frontend
    let isExpr = not isEmptyType(n.typ)
    for i in 0..<n.len - isExpr.ord:
      genStmt(p, n[i])
    if isExpr:
      gen(p, lastSon(n), r)
  of cnkBlockStmt: genBlock(p, n)
  of cnkIfStmt: genIf(p, n)
  of cnkRepeatStmt: genRepeatStmt(p, n)
  of cnkDef: genDef(p, n)
  of cnkCaseStmt: genCaseJS(p, n)
  of cnkReturnStmt: genReturnStmt(p, n)
  of cnkBreakStmt: genBreakStmt(p, n)
  of cnkAsgn: genAsgn(p, n)
  of cnkFastAsgn: genFastAsgn(p, n)
  of cnkVoidStmt:
    genLineDir(p, n)
    var a: TCompRes
    gen(p, n[0], a)
    # the expression might be something not usable as an expression (e.g.,
    # object construction), so use a var statement
    lineF(p, "var _ = $1;$n", [rdLoc(a)])
  of cnkAsmStmt, cnkEmitStmt: genAsmOrEmitStmt(p, n)
  of cnkTryStmt: genTry(p, n)
  of cnkRaiseStmt: genRaiseStmt(p, n)
  of cnkPragmaStmt: discard
  of cnkInvalid, cnkMagic, cnkRange, cnkBinding, cnkExcept, cnkFinally,
     cnkBranch, cnkAstLit, cnkLabel:
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
  genStmt(p, p.fullBody.code)
  p.g.code.add(p.defs)
  p.g.code.add(p.body)

proc wholeCode*(globals: PGlobals): Rope =
  result = globals.typeInfo & globals.constants & globals.code
