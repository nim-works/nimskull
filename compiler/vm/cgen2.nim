## `vmir`-based C code-generator. Separated into two phases:
## * code-gen: IR -> CAst, CDecl
## * emit: write CAst and CDecl to file

import
  std/[
    hashes,
    packedsets,
    sets,
    strformat,
    tables
  ],

  compiler/ast/[
    ast_types,
    ast_query,
    wordrecg
  ],
  compiler/backend/[
    ccgutils
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/ic/[
    bitabs
  ],
  compiler/utils/[
    int128,
    pathutils,
    ropes
  ],
  compiler/vm/[
    irtypes,
    vmir,
    irdbg
  ]

import std/options as stdoptions

from compiler/vm/vmdef import unreachable
from compiler/vm/vmaux import getEnvParam

from compiler/sem/rodutils import toStrMaxPrecision

from compiler/vm/irpasses import computeTypes, PassError

type
  TypeKey = TypeId

  TypeSet = Table[int, TypeId] # TODO: only store unique types?
  SymSet = Table[int, SymId] # TODO: use a `HashSet`

  LocalGenCtx = object
    ## Mutable environmental state that only applies to the current item
    ## (routine) that is being code-gen'ed

  ModuleCtx = object
    ## Environmental state that is local to the module the IR being processed
    ## lies in.

    types: PackedSet[TypeId] # all used type for the module

    syms: PackedSet[SymId] ## all used symbols that need to be declared in the C code. # TODO: should be a SymSet
    funcs: PackedSet[ProcId] ## all used functions that need to be declared in the C code

    # TODO: header paths can be very regular. Maybe a CritBitTree[void} would make sense here?
    # TODO: the header includes are currently emitted in an arbitrary order, is that okay? (check the old cgen)
    headers: HashSet[string] ## all headers the module depends on

  # TODO: rename
  FuncId = distinct uint32


  CAstNodeKind = enum
    ## Syntactic node
    # TODO: reorder
    cnkError # XXX: temporary. used to encode missing code-generator logic in the output
    cnkStmtList

    cnkIf
    cnkWhile
    cnkSwitch
    cnkCase
    cnkCall
    cnkReturn
    cnkGoto

    cnkBraced # braced anonymous initializer

    cnkCast

    cnkLabel # a "label x"

    cnkDotExpr

    cnkCharLit
    cnkStrLit # string literal
    cnkIntLit ## a 64-bit *unsigned* integer literal. ``a`` stores the high and
              ## ``b`` the low bits
    cnkFloat32Lit ## stores a 32-bit float value
    cnkFloat64Lit ## stores a 64-bit float value

    cnkDef
    cnkType
    cnkTernary
    cnkInfix
    cnkPrefix
    cnkIdent
    cnkBracket

  # XXX: hmm, 3 byte wasted for padding
  CAst = seq[tuple[kind: CAstNodeKind, a, b: uint32]]

  # TODO: use the correct names for the syntax constructs
  CDeclAstNodeKind = enum
    ## Basic AST for describing both the content of structs and types in C.
    ## Not really a syntax tree.
    cdnkStruct
    cdnkField # type-decl + ident
    cdnkUnion

    cdnkEmpty

    cdnkType # references another type via a ``CTypeId``
    cdnkWeakType # a "weak" reference to a type, meaning that a definition of
                 # the type doesn't have to be present in the translation unit

    cdnkIntLit # a unsigned integer literal (`a` encodes the low and `b` the high bits)

    cdnkFuncPtr # function-ptr type decl
    cdnkPtr # XXX: strictly speaking, the `*` is part of the declarator and not of the specifier
    cdnkBracket
    cdnkIdent

  CDecl = seq[tuple[kind: CDeclAstNodeKind, a, b: uint32]]

  CTypeDesc = distinct CDecl


  CTypeId = TypeId

  CIdent = LitId ## An identifier in the generated code

  CTypeInfo = object
    decl: CDecl
    name: CIdent #

  CProcHeader = object
    ident: CIdent

    returnType: CTypeId
    args: seq[tuple[typ: CTypeId, name: CIdent]]

  IdentCache = BiTable[string]

  CTypeNode = object
    # TODO: encode whether the type is a pointer via the `name` field
    case isPtr: bool
    of true: nil
    of false:
      name: CIdent

    isImmutable: bool # TODO: merge both bools into a `set`
    isVolatile: bool

  GlobalGenCtx* = object
    ## Environment state that applies to all to all code, independent from
    ## which routine or module the code is in.

    # TODO: BiTable might not be the best choice. It recomputes the hash for the
    #       key on each entry comparision, since it doesn't store key hashes.
    #       `idents.IdentCache` could be used here, but it's very tailored to Nim (and
    #       also a `ref` type). Maybe a hash-table overlay (see
    #       ``vmdef.TypeTable``) is a better choice here
    idents: IdentCache # identifiers used in the generated C-code
    strings: BiTable[string]

    rttiV1: Table[TypeKey, CIdent] # run-time-type-information requested by the IR being processed.
    rttiV2: Table[TypeKey, CIdent]

    funcMap: Table[int, int] ## symbol-id -> index into `procs` # TODO: a table is maybe the wrong data structure here.
    funcs: seq[CProcHeader]

    symIdents: seq[CIdent] # maps each symbol *index* to an identifier
    fieldIdents: seq[CIdent] # maps each field *index* to an identifier

    ctypes: seq[CTypeInfo] #

  NameScope = object
    ## Stores all identifiers defined in a C scope. Used for resolving name conflicts
    # XXX: use a different name?
    idents: PackedSet[CIdent]

  CAstBuilder = object
    ast: CAst

const VoidCType = CTypeId(0)
const StringCType = CTypeId(1)

const InvalidCIdent = CIdent(0) # warning: this depends on a implementation detail of `BiTable`

#[
func hash(a: TypeKey): Hash =
  hash(TypeId(a).itemId)
func `==`(a, b: TypeKey): bool =
  a.PType.itemId == b.PType.itemId
]#

func `==`(a, b: CTypeId): bool {.borrow.}

func formatHexChar(dst: var openArray[char], pos: int, x: uint8) =
  const Chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                 'a', 'b', 'c', 'd', 'e', 'f']
  dst[pos + 0] = Chars[x shr 4]
  dst[pos + 1] = Chars[x and 0x0F]

func iface(syms: SymbolEnv, id: SymId): PSym =
  # XXX: temporary solution
  let orig = syms.orig.getOrDefault(id)
  if orig != nil and sfImportc in orig.flags:
    result = orig

func mangledName(sym: PSym): string =
  # TODO: cache the mangled names (and don't use TLoc for it!)
  # TODO: implement
  if {sfImportc, sfExportc} * sym.flags != {}:
    $sym.loc.r
  else:
    fmt"{sym.owner.name.s}__{mangle(sym.name.s)}_{sym.typ.id}"

# XXX: shared with ``ccgtypes``
func isKeyword(w: PIdent): bool =
  ## Tests if the identifier is a keyword in C
  result = w.id in {ccgKeywordsLow..ccgKeywordsHigh, ord(wInline)}

func mangledName(w: PIdent): string =
  if isKeyword(w):
    fmt"{w.s}_0"
  else:
    mangle(w.s)

func mangledName(d: DeclarationV2): string =
  # XXX: temporary
  if d.forceName:
    d.name.s
  else:
    mangledName(d.name)

func mangledName(d: DeclarationV2, id: uint32): string =
  # XXX: temporary
  if d.forceName:
    d.name.s
  else:
    fmt"{mangle(d.name.s)}_{id}"

func mangledName(procs: ProcedureEnv, id: ProcId): string =
  let decl = procs[id].decl
  if decl.forceName:
    decl.name.s
  else:
    # XXX: temporary fix in order to make overloading work
    fmt"{mangle(decl.name.s)}_{id.uint32}"

const BaseName = "Sup" ## the name of the field for the base type

const ArrayInnerName = "arr"

func add(decl: var CDecl, k: CDeclAstNodeKind; a, b: uint32 = 0) =
  decl.add((k, a, 0'u32))

func addField(decl: var CDecl, typ: CTypeId, name: CIdent) =
  decl.add cdnkField
  decl.add cdnkType, typ.uint32
  decl.add cdnkIdent, name.uint32

func addField(decl: var CDecl, cache: var IdentCache, typ: CTypeId, name: sink string) {.inline.} =
  decl.addField(typ, cache.getOrIncl(name))

func addIntLit(decl: var CDecl, i: uint64) {.inline.} =
  decl.add cdnkIntLit, uint32(i and 0xFFFFFFFF'u64), uint32(i shr 32)

type CTypeMap = Table[TypeKey, CTypeId]

type TypeGenCtx = object
  # inherited state
  cache: IdentCache
  env: ptr IrEnv
  fieldIdents: seq[CIdent] # mutated

  # non-inherited state
  weakTypes: set[TypeNodeKind] # the set of types that can be turned into forward declarations when declared as a pointer

func requestType(c: var TypeGenCtx, t: TypeId): CTypeId =
  ## Requests the type-id for `t`. If the c-type for `t` doesn't exist yet, a
  ## slot for it is reserved and it's added to the `c.forwared` list
  CTypeId(t)

func genRecordNode(c: var TypeGenCtx, decl: var CDecl, i: var RecordNodeIndex, fstart: int): int =
  let n = c.env.types[i]
  inc i

  case n.kind
  of rnkList:
    discard "ignore"
    for _ in 0..<n.len:
      result += genRecordNode(c, decl, i, fstart)

  of rnkFields:
    for i in n.a..n.b:
      let f = fstart + i.int
      let
        field = c.env.types.field(f)
        typ = c.requestType(field.typ)

      var ident: CIdent
      if field.sym == NoneDecl:
        # note: ignoring the records field offset means that unnamed
        #       fields in ``object`` types using inheritance won't work
        ident = c.cache.getOrIncl(fmt"Field{i}")
        c.fieldIdents[f] = ident
      else:
        ident = c.fieldIdents[f] # identifier was already created

      decl.addField(typ, ident)

    result = int(n.b - n.a + 1)

  of rnkCase:
    # TODO: properly name the generated fields, unions, and structs
    #[let discrField = c.env.types.field(f)
    decl.addField(c.cache, c.requestType(discrField.typ), c.env.syms[discrField.sym].decl.name)
    ]#

    discard genRecordNode(c, decl, i, fstart) # discriminator; needs to come before the union

    decl.add cdnkUnion, n.len - 1
    decl.add cdnkEmpty
    for _ in 1..<n.len:
      discard genRecordNode(c, decl, i, fstart)

    result = 2 # discriminator field + union

  of rnkBranch:
    let start = decl.len
    decl.add cdnkStruct
    decl.add cdnkEmpty

    let count = genRecordNode(c, decl, i, fstart)
    decl[start].a = count.uint32

    result = 1 # a single struct

  else:
    unreachable(n.kind)


func addWeakType(dest: var CDecl, c: var TypeGenCtx, t: TypeId) =
  # don't use a weak-dependency for ptr-like types
  let kind =
    if c.env.types[t].kind in c.weakTypes: cdnkWeakType
    else: cdnkType

  dest.add kind, c.requestType(t).uint32

func genCTypeDecl(c: var TypeGenCtx, t: TypeId): CDecl =
  case c.env.types[t].kind
  of tnkRecord:
    let kind =
      if false #[tfUnion in t.flags]#: cdnkUnion
      else:                  cdnkStruct

    result.add kind, 0#, cast[uint32](t.flags)
    result.add cdnkEmpty

    # base type
    if (let base = c.env.types.baseType(t); base != NoneType):
      result.addField(c.cache, c.requestType(base), BaseName)
      inc result[0].a

    var i = c.env.types[t].record.toIndex.RecordNodeIndex
    let f = c.env.types[t].a.int
    let count = genRecordNode(c, result, i, f)
    result[0].a += count.uint32

  of tnkArray:
    # --> struct { T data[len] }
    # Arrays are wrapped in a struct which allows for them to appear as return-types and in assignments
    result.add cdnkStruct, 1.uint32
    result.add cdnkEmpty

    result.add cdnkField
    # not a weak-dep, since the complete type is required
    result.add cdnkType, c.requestType(c.env.types.elemType(t)).uint32
    result.add cdnkBracket
    result.add cdnkIdent, c.cache.getOrIncl(ArrayInnerName).uint32
    result.addIntLit c.env.types.length(t).uint64

  of tnkProc:
      result.add cdnkFuncPtr, c.env.types.numParams(t).uint32
      result.addWeakType(c, c.env.types.getReturnType(t))

      for it in c.env.types.params(t):
          # function pointer declarations don't require complete types
          result.addWeakType(c, it)

  of tnkRef, tnkPtr, tnkVar, tnkLent:
    result.add cdnkPtr
    # we only need a weak-dep for the pointer's element type
    result.addWeakType(c, c.env.types.elemType(t))

  of tnkUncheckedArray:
    result.add cdnkBracket
    result.add cdnkType, c.requestType(c.env.types.elemType(t)).uint32
    # ``SEQ_DECL_SIZE`` is a macro defined in ``nimbase.h``
    result.add cdnkIdent, c.cache.getOrIncl("SEQ_DECL_SIZE").uint32

  of tnkCString:
    result.add cdnkPtr
    result.add cdnkIdent, c.cache.getOrIncl("char").uint32

  else:
    let kind = c.env.types[t].kind
    result.add cdnkIdent, c.cache.getOrIncl(fmt"genCType_missing_{kind}").uint32

  assert result.len > 0

func getTypeName(c: var IdentCache, env: TypeEnv, id: TypeId, decl: Declaration): CIdent =
  # TODO: not finished
  if (let a = env.getAttachmentIndex(id); a.isSome):
    let attach = env.getAttachment(a.unsafeGet)
    if attach[1]:
      c.getOrIncl(attach[0].s)
    else:
      c.getOrIncl(fmt"{mangle(attach[0].s)}_{id.uint32}")
  else:
    # some types require a definition and thus need a name
    case env.kind(id)
    of tnkProc:
      c.getOrIncl(fmt"proc_{id.uint32}")
    of tnkRecord:
      # a record type without a name is always a tuple
      c.getOrIncl(fmt"tuple_{id.uint32}")
    of tnkArray, tnkUncheckedArray:
      c.getOrIncl(fmt"array_{id.uint32}")
    else:
      # the other types don't need generated names
      InvalidCIdent

const AutoImported = {tnkVoid, tnkBool, tnkChar, tnkInt, tnkUInt, tnkFloat} # types that are treated as imported

func genCTypeInfo(gen: var TypeGenCtx, env: TypeEnv, id: TypeId): CTypeInfo =
  let t = env[id]
  if (let iface = env.iface(id); iface != nil and sfImportc in iface.flags):
    result = CTypeInfo(name: gen.cache.getOrIncl(mangledName(iface)))
  elif t.kind in AutoImported:
    let name =
      case t.kind
      of tnkVoid: "void"
      of tnkChar:  "NIM_CHAR"
      of tnkBool:  "NIM_BOOL"
      of tnkInt:
        {.warning: "NI is never emitted anymore, as we can't detect an `int` here".}
        fmt"NI{t.size}"
      of tnkUInt:  fmt"NU{t.size}"
      of tnkFloat: fmt"NF{t.size}"
      else: unreachable()

    result = CTypeInfo(name: gen.cache.getOrIncl(name))
  else:
    let name = getTypeName(gen.cache, env, id, Declaration())
    var decl = genCTypeDecl(gen, id)

    # set the identifier field for struct and union types:
    if decl[0].kind in {cdnkStruct, cdnkUnion}:
      decl[1] = (cdnkIdent, name.uint32, 0'u32)

    result = CTypeInfo(decl: decl, name: name)


func useFunction(c: var ModuleCtx, s: ProcId) =
  ##
  c.funcs.incl s
  #[
  if lfHeader in s.loc.flags:
    c.headers.incl getStr(s.annex.path)
  elif lfNoDecl notin s.loc.flags:
    discard c.syms.mgetOrPut(s.id, s)
  ]#

func useType(c: var ModuleCtx, t: TypeId) =
  assert t != NoneType
  c.types.incl t

#[
func useTypeWeak(c: var ModuleCtx, t: PType): CTypeId=
  c.types

func useType(c: var ModuleCtx, t: PType): CTypeId =
]#

func requestFunction(c: var GlobalGenCtx, s: SymId): int =
  ## Requests the ID of the C-function `s` maps to
  discard "now a no-op"
  #[
  assert s.kind in routineKinds
  let nextId = c.funcs.len
  result = c.funcMap.mgetOrPut(s.id, nextId)
  if result != nextId:
    assert result < nextId
    # the header's content is generated later; we just reserve the slot here
    c.funcs.setLen(c.funcs.len + 1)
  ]#

func requestTypeName(c: var GlobalGenCtx, t: PType): CIdent =
  # TODO: not finished
  if t.sym != nil:
    c.idents.getOrIncl(mangledName(t.sym))
  else:
    c.idents.getOrIncl(fmt"requestTypeName_missing_{t.kind}")

type GenCtx = object
  f: File
  tmp: int
  sym: ProcId

  names: seq[CAst] # IRIndex -> expr
  exprs: seq[bool]
  types: seq[TypeId]
  config: ConfigRef

  scope: NameScope
  localNames: seq[CIdent]

  env: #[lent]# ptr IrEnv

  gl: GlobalGenCtx # XXX: temporary
  m: ModuleCtx # XXX: temporary

func getUniqueName(scope: NameScope, cache: var IdentCache, name: string): CIdent =
  ## Returns a name that is not already in use in the given `scope`. If the
  ## given `name` is not in use, the corresponding identifier ID is returned.
  ## Otherwise, `name` appended to with a number is returned.
  ## Does **not** register the returned identifier with `scope`.

  result = cache.getOrIncl(name)

  if scope.idents.contains(result):
    # an identifier with the requested name already exists
    var buf = name
    buf.add "_"
    let origLen = buf.len

    var next = 10
    var i = 0
    while true:
      while i < next:
        buf.setLen(origLen)
        buf.addInt(i)
        result = cache.getOrIncl(buf)
        if not scope.idents.contains(result):
          return

        inc i

      # no free name was found in the previous range. Expand the search range.
      # Not resetting `i` back to 0 means that we're wasting character space,
      # but it also prevents `a_1` and `a_01` from both existing in the same
      # scope

      next *= 10

func gen(c: GenCtx, irs: IrStore3, n: IRIndex): CAst =
  c.names[n]
  #"gen_MISSING"

func mapTypeV3(t: TypeId): CTypeId

func mapTypeV2(c: var GenCtx, t: TypeId): CTypeId =
  # TODO: unfinished
  c.m.useType(t) # mark the type as used
  mapTypeV3(t)

func mapTypeV3(t: TypeId): CTypeId =
  if t != NoneType:
    # XXX: maybe just have a ``NoneType`` -> ``VoidCType`` mapping in the table instead?
    CTypeId(t)
  else:
    VoidCType

func genCProcHeader(idents: var IdentCache, env: ProcedureEnv, s: ProcId): CProcHeader =
  result.ident = idents.getOrIncl(mangledName(env, s))
  result.returnType = mapTypeV3(env.getReturnType(s))

  result.args.newSeq(env.numParams(s))
  var i = 0
  for p in env.params(s):
    result.args[i] = (mapTypeV3(p.typ),
                      idents.getOrIncl(mangledName(p.name)))
    inc i


template start(): CAstBuilder =
  var b: CAstBuilder
  b

func add(c: var CAstBuilder, kind: CAstNodeKind; a, b: uint32 = 0): var CAstBuilder =
  result = c
  c.ast.add (kind, a, b)


func add(x: var CAst, kind: CAstNodeKind; a, b: uint32 = 0) =
  x.add (kind, a, b)

func add(c: var CAstBuilder, other: CAst): var CAstBuilder =
  result = c
  c.ast.add(other)

func emitDeref(c: var CAstBuilder, idents: var IdentCache): var CAstBuilder =
  result = c
  c.ast.add cnkPrefix
  c.ast.add cnkIdent, idents.getOrIncl("*").uint32

func emitAddr(c: var CAstBuilder, idents: var IdentCache): var CAstBuilder =
  result = c
  c.ast.add cnkPrefix
  c.ast.add cnkIdent, idents.getOrIncl("&").uint32

func ident(c: var CAstBuilder, idents: var IdentCache, name: string): var CAstBuilder =
  result = c
  c.ast.add cnkIdent, idents.getOrIncl(name).uint32

func ident(c: var CAstBuilder, ident: CIdent): var CAstBuilder =
  assert ident != InvalidCIdent
  result = c
  c.ast.add cnkIdent, ident.uint32

func intLit(c: var CAstBuilder, v: BiggestUInt): var CAstBuilder =
  result = c
  c.ast.add cnkIntLit, uint32(v shr 32), uint32(v and 0xFFFFFFFF'u64)

func strLit(c: var CAstBuilder, strs: var BiTable[string], s: sink string): var CAstBuilder =
  result = c
  c.ast.add cnkStrLit, strs.getOrIncl(s).uint32

func floatLit(c: var CAstBuilder, v: BiggestFloat): var CAstBuilder =
  result = c
  let bits = cast[uint64](v)
  c.ast.add cnkFloat64Lit, uint32(bits shr 32), uint32(bits and 0xFFFFFFFF'u64)

func sub(c: var CAstBuilder): var CAstBuilder =
  result = c

func fin(c: sink CAstBuilder): CAst =
  swap(result, c.ast) # XXX: `swap` is used for refc-compatibility

func genError(c: var GenCtx, str: string): CAst =
  # XXX: cnkError always takes a string literal for now
  result.add cnkError, c.gl.strings.getOrIncl(str).uint32

func genArithm(c: var GenCtx, i: IRIndex, check: bool): CAst =
  c.genError("genArithm_missing")

func getTypeArg(irs: IrStore3, arg: IRIndex): TypeId =
  let arg = irs.at(arg)
  case arg.kind
  of ntkLit:
    irs.getLit(arg).typ
  else:
    unreachable(arg.kind)

func genBraced(elems: varargs[CAst]): CAst =
  result.add cnkBraced, elems.len.uint32
  for it in elems.items:
    result.add it

func ident(c: var GlobalGenCtx, name: string): CAst =
  result.add cnkIdent, c.idents.getOrIncl(name).uint32

func genBuiltin(c: var GenCtx, irs: IrStore3, bc: BuiltinCall, n: IRIndex): CAst =
  template arg(i: Natural): IRIndex =
    irs.args(n, i)

  case bc
  of bcUnlikely:
    start().add(cnkCall, 1).ident(c.gl.idents, "NIM_UNLIKELY").add(c.gen(irs, arg(0))).fin()
  of bcError:
    start().add(cnkCall, 1).ident(c.gl.idents, "IR_ERROR").add(cnkStrLit, c.gl.strings.getOrIncl(irs.getLit(irs.at(arg(0))).val.strVal).uint32).fin()
  else:
    genError(c, fmt"missing: {bc}")
    #unreachable(bc)


const CallMagics* = { mIsolate, mFinished, mDotDot, mEqCString,
                      mNewString, mNewStringOfCap, mExit, mParseBiggestFloat }
  ## magics for which no special handling is needed, that is, they're treated
  ## as normal procedures

type MagicKind = enum
  mkUnary
  mkBinary
  mkCall

func genMagic(c: var GenCtx, irs: IrStore3, m: TMagic, n: IRIndex): CAst =
  template arg(i: Natural): IRIndex =
    irs.args(n, i)

  let (kind, sym) =
    case m
    of mNot: (mkUnary, "!")
    of mXor: (mkBinary, "!=")
    of mEqRef, mEqCh, mEqI, mEqB, mEqEnum, mEqF64, mEqProc: (mkBinary, "==")
    of mBitandI: (mkBinary, "&")
    of mBitorI: (mkBinary, "|")
    of mBitxorI: (mkBinary, "^")
    of mBitnotI: (mkUnary, "~")
    of mShlI: (mkBinary, "<<")
    of mShrI, mAshrI: (mkBinary, ">>")
    of mAddU, mAddI, mSucc, mAddF64: (mkBinary, "+")
    of mSubU, mSubI, mPred, mSubF64: (mkBinary, "-")
    of mUnaryMinusI, mUnaryMinusI64, mUnaryMinusF64: (mkUnary, "-")
    of mUnaryPlusI, mUnaryPlusF64: (mkUnary, "+")
    of mLtI, mLtF64, mLtU, mLtB, mLtCh, mLtEnum, mLtPtr: (mkBinary, "<")
    of mLeI, mLeF64, mLeU, mLeB, mLeCh, mLeEnum, mLePtr: (mkBinary, "<=")
    of mMulI, mMulU, mMulF64: (mkBinary, "*")
    of mDivI, mDivU, mDivF64: (mkBinary, "/")
    of mModI, mModU: (mkBinary, "%")
    of mNewString, mNewStringOfCap, mExit, mParseBiggestFloat: (mkCall, "")
    of mSizeOf: (mkCall, "sizeof")
    of mAlignOf: (mkCall, "NIM_ALIGNOF")
    of mMinI, mMaxI:
      # --> (a op b) ? a : b
      let
        a = gen(c, irs, arg(0))
        b = gen(c, irs, arg(1))
        op = if m == mMinI: "<=" else: ">="
      return start().add(cnkTernary).add(cnkInfix).add(a).ident(c.gl.idents, op).add(b).add(a).add(b).fin()
    of mAbsI:
      # -->
      #   (a > 0) ? a : -a
      let a = gen(c, irs, arg(0))
      return start().add(cnkTernary).add(cnkInfix).add(a).ident(c.gl.idents, ">").intLit(0).add(a).add(cnkPrefix).ident(c.gl.idents, "-").add(a).fin()
    of mIsNil:
      # a pointer value is implicitly convertible to a bool, so we use ``!x``
      # to test for nil
      (mkUnary, "!")
    of mChr:
      return start().add(cnkCast).ident(c.gl.idents, "NIM_CHAR").add(gen(c, irs, arg(0))).fin()
    of mOrd:
      # no-op
      return c.gen(irs, arg(0))
    else:
      return genError(c, fmt"missing magic: {m}")

  case kind
  of mkUnary:
    # TODO: assert arg count == 1
    result = start().add(cnkPrefix).ident(c.gl.idents, sym).add(gen(c, irs, arg(0))).fin()
  of mkBinary:
    result = start().add(cnkInfix).add(gen(c, irs, arg(0))).ident(c.gl.idents, sym).add(gen(c, irs, arg(1))).fin()
  of mkCall:
    var builder = start().add(cnkCall, irs.at(n).argCount.uint32).ident(c.gl.idents, sym)
    for arg in irs.args(n):
      discard builder.add(gen(c, irs, arg))

    result = builder.fin()

func genLit(dest: var CAstBuilder, c: var GenCtx, val: PNode): var CAstBuilder =
  case val.kind
  of nkIntLit:
    result = dest.intLit(val.intVal.BiggestUInt)
  else:
    result = dest.add genError(c, fmt"missing: {val.kind}")

func genBracedInit(dest: var CAstBuilder, c: var GenCtx, n: PNode): var CAstBuilder {.discardable.} =
  result = dest
  case n.kind
  of nkBracket, nkTupleConstr:
    if n.kind == nkBracket:
      # arrays are wrapped in a struct so a surrounding initializer is needed
      discard dest.add(cnkBraced, 1)

    discard dest.add(cnkBraced, n.len.uint32)
    for it in n:
      genBracedInit(dest, c, it)

  of nkLiterals:
    discard genLit(dest, c, n)

  else:
    discard dest.add genError(c, fmt"genBracedInit: {n.kind}")


func genLit(c: var GenCtx, literal: Literal): CAst =
  let lit = literal.val
  if lit == nil:
    # `nil` as the value is used for type literals
    return start().add(cnkType, mapTypeV2(c, literal.typ).uint32).fin()

  case lit.kind
  of nkIntLit..nkInt64Lit:
    if lit.intVal < 0:
      # compute the two's-complement, yielding the absolute value. This works
      # even if ``lit.intVal == low(BiggestInt)``.
      # XXX: Nim doesn't guarantee that a signed integer is stored in
      #      two's-complement encoding
      let abs = not(cast[BiggestUInt](lit.intVal)) + 1
      start().add(cnkPrefix).ident(c.gl.idents, "-").intLit(abs).fin()
    else:
      start().intLit(lit.intVal.BiggestUInt).fin()
  of nkUIntLit..nkUInt64Lit:
    start().intLit(cast[BiggestUInt](lit.intVal)).fin()
  of nkCharLit:
    assert lit.intVal in 0..255
    start().add(cnkCharLit, lit.intVal.uint32).fin()
  of nkFloatLit, nkFloat64Lit:
    start().floatLit(lit.floatVal).fin()
  of nkFloat32Lit:
    start().add(cnkFloat32Lit, cast[uint32](lit.floatVal.float32)).fin()
  # TODO: what about ``nkFloat128Lit``? The other code-generators seem to be ignoring them/raising an internal error
  of nkStrLit..nkTripleStrLit:
    # XXX: some passes insert string literals without type information. It's supported for now
    assert literal.typ == NoneType or c.env.types[literal.typ].kind == tnkCString
    # treat as cstring
    start().strLit(c.gl.strings, lit.strVal).fin()
  of nkNilLit:
    start().ident(c.gl.idents, "NIM_NIL").fin()
  of nkBracket, nkTupleConstr:
    start().genBracedInit(c, lit).fin()
  else:
    genError(c, fmt"missing lit: {lit.kind}")

func accessSuper(ast: var CAstBuilder, depth: int, start: CAst, supName: CIdent): var CAstBuilder =
  result = ast

  for _ in 0..<depth:
    discard ast.add(cnkDotExpr)

  discard ast.add(start)

  for _ in 0..<depth:
    discard ast.ident(supName)

template testNode(cond: bool, i: IRIndex) =
  if not cond:
    raise (ref PassError)(msg: fmt"{astToStr(cond)} failed", n: i)

func genSection(result: var CAst, c: var GenCtx, irs: IrStore3, merge: JoinPoint, numStmts: var int, pos: var IRIndex)

proc genCode(c: var GenCtx, irs: IrStore3): CAst =

  # reset the re-usable state
  c.scope.idents.clear()
  c.localNames.setLen(0)

  # reserve the names used by the parameters
  for p in c.gl.funcs[toIndex(c.sym)].args.items:
    c.scope.idents.incl p.name

  var numStmts = 0
  result.add cnkStmtList

  # generate the local definition
  var tmp = 0
  for typ, sym in irs.locals:
    var ident: CIdent

    if sym != NoneDecl:
      let decl = c.env.syms[sym]

      ident =
        if decl.forceName:
          c.gl.idents.getOrIncl(decl.name.s)
        else:
          getUniqueName(c.scope, c.gl.idents, mangledName(decl))

    else:
      # can not clash with other user-defined identifier (except when the
      # corresponding symbols use ``.extern`` or ``.exportc``) because of the
      # leading '_'
      ident = c.gl.idents.getOrIncl(fmt"_tmp{tmp}")
      inc tmp

    c.scope.idents.incl(ident)
    # TODO: use ``setLen`` + []
    c.localNames.add(ident)

    result.add cnkDef
    result.add cnkType, mapTypeV2(c, typ).uint32
    result.add cnkIdent, ident.uint32

    inc numStmts

  c.exprs = calcStmt(irs)
  c.names.newSeq(irs.len)

  var pos = 0
  genSection(result, c, irs, JoinPoint(0), numStmts, pos)

  # exit
  if c.env.types[c.env.procs.getReturnType(c.sym)].kind == tnkVoid:
    result.add cnkReturn
  else:
    # by convention, if a procedure has a return value, the first local is the
    # 'result' variable
    result.add cnkReturn, 1
    result.add cnkIdent, c.localNames[0].uint32

  inc numStmts
  result[0].a = numStmts.uint32


func genStmtList(result: var CAst, c: var GenCtx, irs: IrStore3, merge: JoinPoint, pos: var IRIndex) =
  let start = result.len
  var numStmts = 0

  result.add cnkStmtList
  genSection(result, c, irs, merge, numStmts, pos)

  # set the number of elements in the ``cnkStmtList``:
  result[start].a = numStmts.uint32


func genSection(result: var CAst, c: var GenCtx, irs: IrStore3, merge: JoinPoint, numStmts: var int, pos: var IRIndex) =
  # TODO: `numStmts` should be the return value, but right now it can't, since
  #       `result` is already in use
  template names: untyped = c.names
  template types: untyped = c.types
  template exprs: untyped = c.exprs

  let L = irs.len
  while pos < L:
    let
      n = irs.at(pos)
      i = pos

    inc pos

    case n.kind
    of ntkSym:
      let sId = irs.sym(n)
      let sym = c.env.syms[sId]
      # TODO: refactor
      case sym.kind
      of skVar, skLet, skForVar:
        if sfGlobal in sym.flags:
          c.m.syms.incl sId
        #discard mapTypeV3(c.gl, sym.typ) # XXX: temporary
      of skConst:
        c.m.syms.incl sId
      else:
        discard

      if sym.typ != NoneType:
        useType(c.m, sym.typ)

      names[i] = start().ident(c.gl.symIdents[toIndex(sId)]).fin()

    of ntkParam:
      let name = c.gl.funcs[toIndex(c.sym)].args[n.paramIndex].name
      names[i] = start().ident(name).fin()

    of ntkProc:
      let prc = c.env.procs[n.procId]
      useFunction(c.m, n.procId)

      names[i] = start().ident(c.gl.funcs[toIndex(n.procId)].ident).fin()
    of ntkLocal:
      names[i] = start().ident(c.localNames[irs.getLocalIdx(i)]).fin()

    of ntkCall:
      case n.callKind
      of ckBuiltin:
        let name = genBuiltin(c, irs, n.builtin, i)
        names[i] = name
      of ckMagic:
        names[i] = genMagic(c, irs, n.magic, i)
      of ckNormal:
        let callee = irs.at(n.callee)
        block:
          var res = start().add(cnkCall, n.argCount.uint32).add(names[n.callee])
          for it in irs.args(i):
            discard res.add names[it]
          names[i] = res.fin()

      # TODO: we're missing a proper way to check whether a call is a statement
      if not exprs[i]:#irs.isStmt(n):
        result.add names[i]
        inc numStmts
    of ntkAddr:
      names[i] = start().emitAddr(c.gl.idents).add(names[n.addrLoc]).fin()
    of ntkDeref:
      names[i] = start().emitDeref(c.gl.idents).add(names[n.addrLoc]).fin()
    of ntkAsgn:
      testNode names[n.srcLoc].len > 0, i
      result.add start().add(cnkInfix).add(names[n.wrLoc]).ident(c.gl.idents, "=").add(names[n.srcLoc]).fin()
      inc numStmts
    of ntkPathObj:
      let
        typId = types[n.srcLoc]
        typ = c.env.types[typId]
        (fieldId, steps) = c.env.types.findField(typId, n.fieldIdx)
        field = c.env.types.field(fieldId.toIndex)
      let src = names[n.srcLoc]
      var ast = start()

      # `steps` is the relative depth in the type's hierarchy at which the
      # field is located. E.g. `steps = 0` means it's in `typ`, `steps = 1`
      # means it's in the base-type, etc.
      for i in 0..steps:
        discard ast.add(cnkDotExpr)

      discard ast.add(src)

      for i in 0..<steps:
        discard ast.ident(c.gl.idents, BaseName)

      # accessing a record means that we need a complete type. While the type
      # we're marking as used here isn't necessarily the type that holds the
      # field we're accessing (i.e. inheritance is used), the base types are
      # automatically also pulled in
      c.m.useType(typId)

      discard ast.ident(c.gl.fieldIdents[toIndex(fieldId)])

      names[i] = ast.fin()

    of ntkPathArr:
      case c.env.types[c.types[n.srcLoc]].kind
      of tnkArray:
        names[i] = start().add(cnkBracket).add(cnkDotExpr).add(names[n.srcLoc]).ident(c.gl.idents, ArrayInnerName).add(names[n.arrIdx]).fin()
      else:
        names[i] = start().add(cnkBracket).add(names[n.srcLoc]).add(names[n.arrIdx]).fin()

    of ntkConv, ntkCast:
      if n.kind == ntkConv and c.env.types[n.typ].kind == tnkRecord:
        # XXX: handling object conversion this late is very meh, but I
        #      currently don't see any other simple solution
        let depth = c.env.types.inheritanceDiff(n.typ, c.types[n.srcLoc]).unsafeGet.abs
        assert depth > 0

        names[i] = start().accessSuper(depth, names[n.srcLoc], c.gl.idents.getOrIncl(BaseName)).fin()
      else:
        # both a conversion and a cast map to the same syntax here. Int-to-float
        # and vice-versa casts are already transformed into either a ``memcpy`` or
        # union at the IR level
        names[i] = start().add(cnkCast).add(cnkType, mapTypeV2(c, n.typ).uint32).add(names[n.srcLoc]).fin()

    of ntkLit:
      names[i] = genLit(c, irs.getLit(n))
    of ntkUse:
      names[i] = names[n.srcLoc]
    of ntkBranch:
      result.add cnkIf
      result.add names[n.cond]
      result.add cnkStmtList, 1
      result.add cnkGoto, c.gl.idents.getOrIncl(fmt"label{n.target}").uint32

      # XXX: doesn't work due to how a `x or y` is represented in the IR
      #[
      genStmtList(result, c, irs, n.target, pos)

      # skip the join coming after the branch's section
      testNode irs.at(pos).kind == ntkJoin, pos
      inc pos
      ]#

      inc numStmts
    of ntkJoin:
      # always create a label, even for loops
      # XXX: loops should use 'while' loops in the generated code instead
      result.add cnkLabel, c.gl.idents.getOrIncl(fmt"label{n.joinPoint}").uint32
      inc numStmts
    of ntkGoto:
      result.add cnkGoto, c.gl.idents.getOrIncl(fmt"label{n.target}").uint32
      inc numStmts

      #[
      # a goto always marks the end of a section
      break
      ]#

    else:
      names[i] = genError(c, fmt"missing impl: {n.kind}")
      if not exprs[i]:
        result.add names[i]
        inc numStmts

proc emitCDecl(f: File, c: GlobalGenCtx, decl: CDecl)

proc emitType(f: File, c: GlobalGenCtx, t: CTypeId) =
  let info {.cursor.} = c.ctypes[t.int]
  if info.name != InvalidCIdent:
    f.write c.idents[info.name]
  else:
    # the declaration is emitted directly if a type has no name
    emitCDecl(f, c, info.decl)

proc emitCAst(f: File, c: GlobalGenCtx, ast: CAst, pos: var int)

proc emitAndEscapeIf(f: File, c: GlobalGenCtx, ast: CAst, pos: var int, notSet: set[CAstNodeKind]) =
  if ast[pos].kind in notSet:
    emitCAst(f, c, ast, pos)
  else:
    f.write "("
    emitCAst(f, c, ast, pos)
    f.write ")"

proc writeChars[I: static int](f: File, arr: array[I, char]) {.inline.} =
  discard f.writeBuffer(addr(arr), I)

proc emitCAst(f: File, c: GlobalGenCtx, ast: CAst, pos: var int) =
  if pos >= ast.len:
    for it in ast:
      echo it

  let n = ast[pos]
  inc pos

  case n.kind
  of cnkError:
    f.write "GEN_ERROR(\""
    f.write c.strings[n.a.LitId]
    f.write "\")"
  of cnkStmtList:
    for _ in 0..<n.a:
      emitCAst(f, c, ast, pos)
      f.writeLine ";"

  of cnkDef:
    emitCAst(f, c, ast, pos) # type
    f.write " "
    emitCAst(f, c, ast, pos) # ident
    if n.a != 0'u32:
      f.write " = "
      emitCAst(f, c, ast, pos) # initializer

  of cnkIdent:
    f.write c.idents[n.a.LitId]

  of cnkInfix:
    emitAndEscapeIf(f, c, ast, pos, {cnkIdent, cnkIntLit, cnkStrLit}) # lhs
    f.write " "
    emitCAst(f, c, ast, pos) # infix
    f.write " "
    emitAndEscapeIf(f, c, ast, pos, {cnkIdent, cnkIntLit, cnkStrLit}) # rhs

  of cnkPrefix:
    emitCAst(f, c, ast, pos)
    emitAndEscapeIf(f, c, ast, pos, {cnkIdent})

  of cnkBracket:
    emitAndEscapeIf(f, c, ast, pos, {cnkIdent, cnkDotExpr})
    f.write "["
    emitCAst(f, c, ast, pos)
    f.write "]"

  of cnkCall:
    emitAndEscapeIf(f, c, ast, pos, {cnkIdent}) # callee
    f.write "("
    for i in 0..<n.a:
      if i > 0:
        f.write ", "

      emitCAst(f, c, ast, pos)

    f.write ")"

  of cnkIf:
    f.write "if ("
    emitCAst(f, c, ast, pos) # condition
    f.writeLine ") {"
    emitCAst(f, c, ast, pos) # stmt list
    f.write "}"

  of cnkWhile:
    f.write "while ("
    emitCAst(f, c, ast, pos) # condition
    f.write ") {"
    emitCAst(f, c, ast, pos) # stmt list
    f.write "}"

  of cnkReturn:
    f.write "return "
    if n.a == 1:
      emitCAst(f, c, ast, pos)

  of cnkLabel:
    f.write c.idents[n.a.LitId]
    f.writeLine ":"
  of cnkGoto:
    f.write "goto "
    f.write c.idents[n.a.LitId]
  of cnkDotExpr:
    emitAndEscapeIf(f, c, ast, pos, {cnkIdent, cnkCall, cnkDotExpr})
    f.write "."
    emitCAst(f, c, ast, pos)
  of cnkCharLit:
    var arr = ['\'', '\\', 'x', '0', '0', '\'']
    formatHexChar(arr, 3, n.a.uint8)

    f.writeChars arr
  of cnkStrLit:
    f.write '"'
    let str = c.strings[n.a.LitId]
    # XXX: escape the string prior to adding to ``c.strings``?
    for ch in str.items:
      if ch in '\x20'..'\x7F':
        f.write ch
      else:
        var arr = ['\\', 'x', '0', '0']
        formatHexChar(arr, 2, ord(ch).uint8)

        f.writeChars arr

    f.write '"'

  of cnkIntLit:
    f.write (n.a.uint64 shl 32) or n.b.uint64

  of cnkFloat32Lit:
    f.write toStrMaxPrecision(cast[float32](n.a))
  of cnkFloat64Lit:
    f.write toStrMaxPrecision(cast[float64]((n.a.uint64 shl 32) or n.b.uint64))

  of cnkType:
    emitType(f, c, n.a.CTypeId)

  of cnkCast:
    f.write "("
    emitCAst(f, c, ast, pos)
    f.write ") "
    emitAndEscapeIf(f, c, ast, pos, {cnkIdent})

  of cnkBraced:
    f.write "{"
    for i in 0..<n.a:
      if i > 0:
        f.write ", "
      emitCAst(f, c, ast, pos)
    f.write "}"

  of cnkTernary:
    f.write "("
    emitCAst(f, c, ast, pos) # condition
    f.write ") ? "
    emitAndEscapeIf(f, c, ast, pos, {cnkIdent}) # a
    f.write " : "
    emitAndEscapeIf(f, c, ast, pos, {cnkIdent}) # b

  else:
    f.write "EMIT_ERROR(\"missing " & $n.kind & "\")"

proc emitCAst(f: File, c: GlobalGenCtx, ast: CAst) =
  var pos = 0
  while pos < ast.len:
    emitCAst(f, c, ast, pos)


proc emitCDecl(f: File, c: GlobalGenCtx, decl: CDecl, pos: var int)

proc emitFuncDecl(f: File, c: GlobalGenCtx, decl: CDecl, ident: CIdent, L: int, pos: var int) =
  emitCDecl(f, c, decl, pos) # return type
  if ident != InvalidCIdent:
    f.write "(*"
    f.write c.idents[ident]
    f.write ")("
  else:
    f.write "(*)("
  for i in 0..<L:
    if i > 0:
      f.write ", "

    emitCDecl(f, c, decl, pos)

  f.write ")"

proc emitCDecl(f: File, c: GlobalGenCtx, decl: CDecl, pos: var int) =
  if pos >= decl.len:
    for it in decl:
      echo it
  let n = decl[pos]
  inc pos

  case n.kind
  of cdnkStruct, cdnkUnion:
    f.write:
      if n.kind == cdnkStruct: "struct "
      else: "union "

    emitCDecl(f, c, decl, pos)
    f.writeLine "{"
    for _ in 0..<n.a:
      emitCDecl(f, c, decl, pos)
      f.writeLine ";"

    f.write "}"

  of cdnkField:
    emitCDecl(f, c, decl, pos) # type
    f.write " "
    emitCDecl(f, c, decl, pos) # name

  of cdnkType:
    let info {.cursor.} = c.ctypes[n.a.uint32]
    if info.name == InvalidCIdent:
      emitCDecl(f, c, info.decl)
    else:
      f.write c.idents[info.name]

  of cdnkWeakType:
    let info {.cursor.} = c.ctypes[n.a.uint32]
    assert info.name != InvalidCIdent

    f.write c.idents[info.name]

  of cdnkPtr:
    emitCDecl(f, c, decl, pos)
    f.write "*"

  of cdnkIdent:
    f.write c.idents[n.a.CIdent]

  of cdnkIntLit:
    let val = n.a.uint64 or (n.b.uint64 shl 32)
    f.write $val

  of cdnkBracket:
    emitCDecl(f, c, decl, pos)
    f.write "["
    emitCDecl(f, c, decl, pos)
    f.write "]"

  of cdnkFuncPtr:
    emitFuncDecl(f, c, decl, InvalidCIdent, n.a.int, pos)

  of cdnkEmpty:
    discard "nothing"


proc emitCDecl(f: File, c: GlobalGenCtx, decl: CDecl) =
  var pos = 0
  emitCDecl(f, c, decl, pos)

proc emitCType(f: File, c: GlobalGenCtx, info: CTypeInfo, isFwd: bool) =
  var pos = 0

  assert info.decl.len > 0, c.idents[info.name]

  let kind = info.decl[0].kind
  case kind
  of cdnkStruct, cdnkUnion:
    if isFwd:
      # --> ``typdef struct X X;``
      # forward-declare the record type and make the identifier available in
      # the ordinary namespace
      f.write "typedef "
      f.write:
        case kind
        of cdnkStruct: "struct"
        of cdnkUnion:  "union"
        else: unreachable()

      f.write fmt" {c.idents[info.name]}"
      f.write fmt" {c.idents[info.name]}"
      pos = info.decl.len # mark the body as processed
    else:
      # definition requested
      emitCDecl(f, c, info.decl, pos)

  of cdnkBracket:
    f.write "typedef "
    pos = 1
    emitCDecl(f, c, info.decl, pos)
    f.write " "
    f.write c.idents[info.name]
    f.write "["
    emitCDecl(f, c, info.decl, pos) # the array size
    f.write "]"
  of cdnkFuncPtr:
    f.write "typedef "
    pos = 1
    emitFuncDecl(f, c, info.decl, info.name, info.decl[0].a.int, pos)
  else:
    f.write "typedef "
    emitCDecl(f, c, info.decl, pos)
    f.write " "
    f.write c.idents[info.name]

  f.writeLine ";"

  assert pos == info.decl.len

proc writeProcHeader(f: File, c: GlobalGenCtx, h: CProcHeader, decl: DeclarationV2, withParamNames: bool): bool =
  if decl.omit:
    return false

  emitType(f, c, h.returnType)
  f.write(" ")
  f.write(c.idents[h.ident])
  f.write("(")
  for i, it in h.args.pairs:
    if i > 0:
      f.write ", "

    emitType(f, c, it.typ)

    if withParamNames:
      f.write " "
      f.write c.idents[it.name]

  f.write ")"
  result = true

func initGlobalContext*(c: var GlobalGenCtx, env: IrEnv) =
  ## Initializes the ``GlobalGenCtx`` to use for all following ``emitModuleToFile`` calls. Creates the ``CTypeInfo`` for each IR type.

  # TODO: use ``setLen`` + []
  for id in env.syms.items:
    c.symIdents.add c.idents.getOrIncl(mangledName(env.syms[id].decl, id.uint32))

  block:
    # setup the field -> identifier map:
    c.fieldIdents.newSeq(env.types.totalFields)

    # we setup the identifiers for named fields here instead of in
    # ``genRecordNode`` (as is done for anonymous fields). Why? Because
    # no ``CDecl`` is generated for ``.nodecl`` object types, but their fields
    # still need identifiers.
    # XXX: with this approach, nodecl named tuple types will cause a compiler
    #      crash since their fields are treated as anonymous
    for i, f in env.types.allFields:
      # anonymous fields (used by anonymous tuples) use a position-based
      # naming scheme, but since we don't know about field positions here, we
      # defer identifier creation for those to ``genRecordNode``
      if f.sym != NoneDecl:
        c.fieldIdents[i] = c.idents.getOrIncl(mangledName(env.syms[f.sym]))

  var gen = TypeGenCtx(weakTypes: {tnkRecord}, env: unsafeAddr env)
  swap(gen.cache, c.idents)
  swap(gen.fieldIdents, c.fieldIdents)

  # XXX: a leftover from the CTypeId -> TypeId transition. Needs to be removed
  c.ctypes.add(CTypeInfo(name: gen.cache.getOrIncl("void"))) # the `VoidCType`

  # TODO: use ``setLen`` + []
  for id in types(env.types):
    c.ctypes.add genCTypeInfo(gen, env.types, id)

  # TODO: rewrite the type translation logic here; it's wasteful
  for id, target in env.types.proxies:
    # don't redirect types that have an interface (i.e. are imported)
    if env.types.iface(id) == nil:
      # replace with an alias
      c.ctypes[id.int].decl = @[(cdnkType, target.uint32, 0'u32)]

  swap(gen.cache, c.idents)
  swap(gen.fieldIdents, c.fieldIdents)

  # create the procedure headers
  # TODO: use ``setLen`` + []
  for id in env.procs.items:
    c.funcs.add genCProcHeader(c.idents, env.procs, id)


proc emitModuleToFile*(conf: ConfigRef, filename: AbsoluteFile, ctx: var GlobalGenCtx, env: IrEnv,
                       impls: openArray[IrStore3], m: ModuleData) =
  let f = open(filename.string, fmWrite)
  defer: f.close()

  echo "Here: ", filename.string

  var
    mCtx: ModuleCtx
    asts: seq[CAst]

  mCtx.headers.incl("\"nimbase.h\"")

  for sym in m.procs.items:
    let irs = impls[sym.toIndex]
    useFunction(mCtx, sym)

    if sfImportc in env.procs[sym].flags:
      asts.add(default(CAst))
      continue

    echo "genFor: ", env.procs[sym].decl.name.s #, " at ", conf.toFileLineCol(sym.info)
    var c = GenCtx(f: f, config: conf, sym: sym, env: unsafeAddr env)
    # doing a separate pass for the type computation instead of doing it in
    # `genCode` is probably a bit less efficient, but it's also simpler;
    # requires less code duplication; and is also good for modularity
    c.types = computeTypes(irs, env)

    swap(c.gl, ctx)
    swap(c.m, mCtx)
    asts.add genCode(c, irs)
    swap(c.m, mCtx)
    swap(c.gl, ctx)

  # XXX: this might lead to an ordering problem, since we're not registering
  #      the types on the first occurence
  # mark the types used in routine signatures as used
  for id in mCtx.funcs.items:
    mCtx.useType(env.procs.getReturnType(id))

    for it in env.procs.params(id):
      mCtx.useType(it.typ)

  for id in m.syms.items:
    mCtx.useType(env.syms[id].typ)

  # mark the type of used globals and constants as used and collect C-header
  # dependencies
  for id in mCtx.syms.items:
    # TODO: is it necessary to mark the type as used if the sym is
    #       ``.nodecl``?
    # XXX: the ``useType`` here is redundant - already happened when the
    #      symbol was added to the set
    mCtx.useType(env.syms[id].typ)
    if (let iface = env.syms.iface(id); iface != nil):
      if lfHeader in iface.loc.flags:
        mCtx.headers.incl getStr(iface.annex.path)

  # collect all types that we need to be defined in this translation unit (.c file)

  type TypeDef = tuple[fwd: bool, id: CTypeId]

  func collectFwd(list: var seq[TypeDef], types: seq[CTypeInfo], id: CTypeId, marker, markerFwd: var PackedSet[CTypeId]) =
    if id notin marker and not markerFwd.containsOrIncl(id):
      # not defined nor forward declared
      assert types[id.int].name != InvalidCIdent
      list.add (true, id)

  func collectOrdered(list: var seq[TypeDef], types: seq[CTypeInfo],
                      id: CTypeId, marker, markerFwd: var PackedSet[CTypeId]) =
    let info {.cursor.} = types[id.int]
    if marker.containsOrIncl(id):
      # nothing to do
      return

    if info.decl.len > 0 and info.decl[0].kind in {cdnkStruct, cdnkUnion}:
      # a forward ``typedef`` is used for all structs and unions for two reasons:
      # * the typedef introduces the identifier in the ordinary name-space
      # * it's a simple solution to the cyclical object type problem
      list.add (true, id)
      markerFwd.incl id

    # scan the type's body for dependencies and add them first
    for n in info.decl.items:
      case n.kind
      of cdnkType:
        # requires a definition
        collectOrdered(list, types, n.a.CTypeId, marker, markerFwd)
      of cdnkWeakType:
        # only requires a forward declaration
        collectFwd(list, types, n.a.CTypeId, marker, markerFwd)
      else:
        discard

    # XXX: the used headers could also be collected here, but that would grow
    # the required state even more

    if info.name != InvalidCIdent:
      # only collect types that have an identifier. The others don't need a
      # typedef (they're inlined directly) and also don't/can't have header
      # dependency information attached
      list.add (false, id)

  var typedefs: seq[TypeDef]
  var marker, markerFwd: PackedSet[CTypeId]
  for it in mCtx.types.items:
    collectOrdered(typedefs, ctx.ctypes, it, marker, markerFwd)

  marker.reset() # no longer needed

  # collect the header dependencies from the used types
  # XXX: to be more efficient, writing out the header includes for the types
  #      could be combined with emitting the type definitions

  for _, id in typedefs.items:
    let iface = env.types.iface(id)
    if iface != nil and lfHeader in iface.loc.flags:
      mCtx.headers.incl getStr(iface.annex.path)

  # collect the header dependencies of used functions
  for id in mCtx.funcs.items:
    let header = env.procs[id].iface.header
    if header.len != 0:
      mCtx.headers.incl header

  # ----- start of the emit logic -----

  f.writeLine "#define NIM_INTBITS 64" # TODO: don't hardcode

  # headers
  for h in mCtx.headers.items:
    f.writeLine fmt"#include {h}"

  # type section

  for fwd, id in typedefs.items:
    let info = ctx.ctypes[id.int]
    # imported types don't have a body
    if info.decl.len > 0:
      emitCType(f, ctx, ctx.ctypes[id.int], isFwd=fwd)

  # generate all procedure forward declarations
  for id in mCtx.funcs.items:
    #echo "decl: ", sym.name.s, " at ", conf.toFileLineCol(sym.info)
    if writeProcHeader(f, ctx, ctx.funcs[id.toIndex], env.procs[id].decl, false):
      f.writeLine ";"

  # globals of the current module
  for id in m.syms.items:
    let sym = env.syms[id]
    if sym.decl.omit:
      continue

    case sym.kind
    of skLet, skVar, skForVar:
      emitType(f, ctx, sym.typ)
      f.write " "
      f.write ctx.idents[ctx.symIdents[toIndex(id)]]
      f.writeLine ";"
    else:
      unreachable(sym.kind)

  # referenced globals and constants
  for id in mCtx.syms.items:
    let sym = env.syms[id]
    let ident = ctx.symIdents[toIndex(id)]

    if sym.decl.omit:
      continue

    case sym.kind
    of skLet, skVar, skForVar:
      # XXX: the `mCtx.syms` set might also include globals that are *defined*
      #      as part of this module - in which case the declaration here is
      #      redundant
      f.write "extern "
      emitType(f, ctx, sym.typ)
      f.write " "
      f.write ctx.idents[ident]
      f.writeLine ";"
    of skConst:
      f.write "static "
      emitType(f, ctx, sym.typ)
      f.write " "
      f.write ctx.idents[ident]
      f.writeLine ";"
    else:
      unreachable(sym.kind)

  var i = 0
  for it in asts.items:
    if it.len == 0:
      inc i
      continue

    let
      id = m.procs[i]
      prc = env.procs[id]

    if writeProcHeader(f, ctx, ctx.funcs[id.toIndex], prc.decl, true):
      # only emit the body if the procedure is not omitted (i.e. declared
      # as `.noDecl`)
      # XXX: forbid using the .noDecl pragma on a procedure with body?
      f.writeLine "{"
      emitCAst(f, ctx, it)
      f.writeLine "}"

    inc i