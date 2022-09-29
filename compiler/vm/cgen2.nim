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

    # XXX: instead of using ``PackedSet``s here, ``TBitSet`` might be the
    #      better choice:
    #      - all operations are faster on them
    #      - it's simpler data-structure in general
    #      - it's trivial to iterate them in the order of ID values
    #      - better memory locality (it's just a seq)
    #      - except for resizing, no operations require allocations
    #
    #      The only downside is that they on probably require more memory on
    #      average.
    #      The ordered iteration property could unlock alot of simplifications.
    #      For example, if it'd be enforced that for types and constants,
    #      items with a lower ID never depend on types with a higher ID, the
    #      quite complex logic for emitting them in the correct order can be
    #      turned into a simple for-loop

    types: PackedSet[TypeId] # all used type for the module

    syms: PackedSet[SymId] ## all used symbols that need to be declared in the C code. # TODO: should be a SymSet
    funcs: PackedSet[ProcId] ## all used functions that need to be declared in the C code

    # TODO: header paths can be very regular. Maybe a CritBitTree[void} would make sense here?
    # TODO: the header includes are currently emitted in an arbitrary order, is that okay? (check the old cgen)
    headers: HashSet[string] ## all headers the module depends on

  # TODO: rename
  FuncId = distinct uint32

  COperator = enum
    ## Currently only meant to name operators without having to
    ## use the string representations directly.
    # XXX: these could be sorted by precedence
    copAdd = "+", copSub = "-", copMul = "*", copDiv="/", copMod="%",
    copNot = "!"
    copBitnot = "~", copBitand="&", copBitor="|", copBitxor="^"
    copOr = "||"
    copShl="<<", copShr=">>"
    copEq="==", copNEq="!=", copLt="<", copLe="<=", copGt=">", copGe=">=" # comparison
    copAsgn="="

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

    cnkOpToken

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
    # XXX: the only remaining use for `strings` is error messages
    strings: BiTable[string]

    rttiV1: Table[TypeKey, CIdent] # run-time-type-information requested by the IR being processed.
    rttiV2: Table[TypeKey, CIdent]

    funcMap: Table[int, int] ## symbol-id -> index into `procs` # TODO: a table is maybe the wrong data structure here.
    funcs: seq[CProcHeader]

    symIdents: seq[CIdent] # maps each symbol *index* to an identifier
    fieldIdents: seq[CIdent] # maps each field *index* to an identifier

    constInit: Table[SymId, CAst] ## the initializer for each constant
    constDeps: Table[SymId, seq[SymId]] ## the dependencies on other constants
                                        ## for each constant

    ctypes: seq[CTypeInfo] #

  NameScope = object
    ## Stores all identifiers defined in a C scope. Used for resolving name conflicts
    # XXX: use a different name?
    idents: PackedSet[CIdent]

  CAstBuilder = object
    ast: CAst

  ExprInfo = object
    # TODO: manually implement the bitfield. C doesn't make guarantees
    #       regarding the layout and ``bitsize`` also doesn't work on the
    #       VM target
    # TODO: only 4 bits are used - making use of a ``PackedSeq`` would reduce
    #       the amount of storage required  
    rc {.bitsize: 2.}: uint ## 0 = no referenced
                            ## 1 = referenced only once
                            ## 2 = referenced more than once
    disjoint {.bitsize: 1.}: bool ## whether the expression is used in a
      ## partition different from the one it's located in
    hasSideEffect {.bitsize: 1.}: bool ## whether the expression has
                                       ## side-effects

const VoidCType = CTypeId(0)
const StringCType = CTypeId(1)

const InvalidCIdent = CIdent(0) # warning: this depends on a implementation detail of `BiTable`

# ``char`` is an unsigned 8-bit value in NimSkull, so we just use ``NU8``
# XXX: if this causes issues with ``cstring`` (which now is an
#      ``unsigned char*``), ``unsigned char`` could be used
#      instead
const CharType = "NU8"

func enumToStrTbl[E: enum](_: typedesc[E]): array[E, string] =
  for e in low(E)..high(E):
    result[e] = $e

const COpToStr = enumToStrTbl(COperator)
  ## Maps a ``COperator`` to it's string representation. More efficient than
  ## ``$COperator``

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

func formatOctChar(dst: var openArray[char], pos: int, x: uint8) =
  const Chars = ['0', '1', '2', '3', '4', '5', '6', '7']
  dst[pos + 0] = Chars[x shr 6]
  dst[pos + 1] = Chars[(x shr 3) and 0x07]
  dst[pos + 2] = Chars[x and 0x07]

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

func genRecordNode(c: var TypeGenCtx, decl: var CDecl, iter: var RecordIter): int =
  let n = next(c.env.types, iter)

  case n.kind
  of rnkList:
    for _ in 0..<n.len:
      result += genRecordNode(c, decl, iter)

  of rnkFields:
    for i in n.slice.items:
      let
        fId = iter.field(i)
        field = c.env.types[fId]
        typ = c.requestType(field.typ)

      var ident: CIdent
      if field.sym == NoneDecl:
        # note: ignoring the records field offset means that unnamed
        #       fields in ``object`` types using inheritance won't work
        ident = c.cache.getOrIncl(fmt"Field{i}")
        c.fieldIdents[fId.toIndex] = ident
      else:
        ident = c.fieldIdents[fId.toIndex] # identifier was already created

      decl.addField(typ, ident)

    result = n.slice.len

  of rnkCase:
    # TODO: properly name the generated fields, unions, and structs
    #[let discrField = c.env.types.field(f)
    decl.addField(c.cache, c.requestType(discrField.typ), c.env.syms[discrField.sym].decl.name)
    ]#

    discard genRecordNode(c, decl, iter) # discriminator; needs to come before the union

    decl.add cdnkUnion, n.len - 1
    decl.add cdnkEmpty
    for _ in 1..<n.len:
      discard genRecordNode(c, decl, iter)

    result = 2 # discriminator field + union

  of rnkBranch:
    let start = decl.len
    decl.add cdnkStruct
    decl.add cdnkEmpty

    assert n.len == 1
    decl[start].a = genRecordNode(c, decl, iter).uint32

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
      if tfUnion in c.env.types.flags(t): cdnkUnion
      else:                               cdnkStruct

    result.add kind, 0#, cast[uint32](t.flags)
    result.add cdnkEmpty

    # base type
    if (let base = c.env.types.baseType(t); base != NoneType):
      result.addField(c.cache, c.requestType(base), BaseName)
      inc result[0].a

    var iter = initRecordIter(c.env.types, t)
    let count = genRecordNode(c, result, iter)
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
    result.add cdnkIdent, c.cache.getOrIncl("unsigned char").uint32

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
    result = CTypeInfo(name: gen.cache.getOrIncl($iface.loc.r))
  elif t.kind in AutoImported:
    let name =
      case t.kind
      of tnkVoid: "void"
      of tnkChar:  CharType
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
  exprs: seq[ExprInfo]
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
    let ident =
      if p.name != nil: idents.getOrIncl(mangledName(p.name))
      else:             idents.getOrIncl("_param" & $i)

    result.args[i] = (mapTypeV3(p.typ),
                      ident)
    inc i


template start(): CAstBuilder =
  var b: CAstBuilder
  b

template buildAst(code: untyped): CAst =
  var builder {.inject.}: CAstBuilder
  code
  builder.fin()

template void(c: CAstBuilder) =
  ## Convenience routine meant for discarding the result of a builder call
  ## chain
  discard c

func add(c: var CAstBuilder, kind: CAstNodeKind; a, b: uint32 = 0): var CAstBuilder =
  result = c
  c.ast.add (kind, a, b)


func add(x: var CAst, kind: CAstNodeKind; a, b: uint32 = 0) =
  x.add (kind, a, b)

func add(c: var CAstBuilder, other: CAst): var CAstBuilder =
  result = c
  c.ast.add(other)

func emitDeref(c: var CAstBuilder): var CAstBuilder =
  result = c
  c.ast.add cnkPrefix
  c.ast.add cnkOpToken, copMul.ord.uint32

func emitAddr(c: var CAstBuilder): var CAstBuilder =
  result = c
  c.ast.add cnkPrefix
  c.ast.add cnkOpToken, copBitand.ord.uint32

func ident(c: var CAstBuilder, idents: var IdentCache, name: string): var CAstBuilder =
  result = c
  c.ast.add cnkIdent, idents.getOrIncl(name).uint32

func ident(c: var CAstBuilder, ident: CIdent): var CAstBuilder =
  assert ident != InvalidCIdent
  result = c
  c.ast.add cnkIdent, ident.uint32

func op(c: var CAstBuilder, op: COperator): var CAstBuilder =
  result = c
  c.ast.add cnkOpToken, op.ord.uint32

func intLit(c: var CAstBuilder, v: BiggestUInt): var CAstBuilder =
  result = c
  c.ast.add cnkIntLit, uint32(v shr 32), uint32(v and 0xFFFFFFFF'u64)

func strLit(c: var CAstBuilder, id: LiteralId): var CAstBuilder =
  assert id.kind == lkString
  result = c
  c.ast.add cnkStrLit, id.uint32

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
    start().add(cnkCall, 1).ident(c.gl.idents, "IR_ERROR").strLit(irs.getLit(irs.at(arg(0))).val).fin()
  else:
    genError(c, fmt"missing: {bc}")
    #unreachable(bc)

# TODO: include ``mParseBiggestFloat`` back in the set once the issue
#       described in ``cpasses`` is fixed
const CallMagics* = { mIsolate, mFinished, mDotDot, mEqCString,
                      mNewString, mNewStringOfCap, mExit #[, mParseBiggestFloat]# }
  ## magics for which no special handling is needed, that is, they're treated
  ## as normal procedures

type MagicKind = enum
  mkUnary
  mkBinary
  mkCall

func genSimpleMagic(c: var CAstBuilder, ctx: GenCtx, irs: IrStore3, m: TMagic, n: IRIndex): bool =
  let (kind, op) =
    case m
    of mNot: (mkUnary, copNot)
    of mXor: (mkBinary, copNEq)
    of mEqRef, mEqCh, mEqI, mEqB, mEqEnum, mEqF64, mEqProc: (mkBinary, copEq)
    of mBitandI: (mkBinary, copBitand)
    of mBitorI: (mkBinary, copBitor)
    of mBitxorI: (mkBinary, copBitxor)
    of mBitnotI: (mkUnary, copBitnot)
    of mShlI: (mkBinary, copShl)
    of mShrI, mAshrI: (mkBinary, copShr)
    of mAddU, mAddI, mSucc, mAddF64: (mkBinary, copAdd)
    of mSubU, mSubI, mPred, mSubF64: (mkBinary, copSub)
    of mUnaryMinusI, mUnaryMinusI64, mUnaryMinusF64: (mkUnary, copSub)
    of mUnaryPlusI, mUnaryPlusF64: (mkUnary, copAdd)
    of mLtI, mLtF64, mLtU, mLtB, mLtCh, mLtEnum, mLtPtr: (mkBinary, copLt)
    of mLeI, mLeF64, mLeU, mLeB, mLeCh, mLeEnum, mLePtr: (mkBinary, copLe)
    of mMulI, mMulU, mMulF64: (mkBinary, copMul)
    of mDivI, mDivU, mDivF64: (mkBinary, copDiv)
    of mModI, mModU: (mkBinary, copMod)
    of mOr: (mkBinary, copOr)
    of mIsNil:
      # a pointer value is implicitly convertible to a bool, so we use ``!x``
      # to test for nil
      (mkUnary, copNot)
    else:
      # not a simple operator
      return false

  template arg(i: Natural): IRIndex =
    irs.args(n, i)

  case kind
  of mkUnary:
    # TODO: assert arg count == 1
    c.add(cnkPrefix).op(op).add(gen(ctx, irs, arg(0))).void()
  of mkBinary:
    c.add(cnkInfix).add(gen(ctx, irs, arg(0))).op(op).add(gen(ctx, irs, arg(1))).void()
  else:
    unreachable(kind)

  result = true

func genMagic(c: var GenCtx, irs: IrStore3, m: TMagic, n: IRIndex): CAst =
  template arg(i: Natural): IRIndex =
    irs.args(n, i)

  block:
    # see if the magic directly maps to a C operator
    var ast = start()
    if genSimpleMagic(ast, c, irs, m, n):
      return ast.fin()

  # it doesn't
  let (kind, sym) =
    case m
    of mSizeOf: (mkCall, "sizeof")
    of mAlignOf: (mkCall, "NIM_ALIGNOF")
    of mOffsetOf:
      # --> offsetof(typename, fieldname)
      let
        a = gen(c, irs, arg(0))
        typ = irs.getLit(irs.at(arg(0))).typ
        # TODO: the field position argument needs to a ``ntkImm``
        b = c.env.data.getInt(irs.getLit(irs.at(arg(1))).val)
      return start().add(cnkCall, 2).ident(c.gl.idents, "offsetof").add(a).ident(c.gl.fieldIdents[c.env.types.nthField(typ, b.int).toIndex]).fin()
    of mMinI, mMaxI:
      # --> (a op b) ? a : b
      let
        a = gen(c, irs, arg(0))
        b = gen(c, irs, arg(1))
        op = if m == mMinI: copLe else: copGe
      return start().add(cnkTernary).add(cnkInfix).add(a).op(op).add(b).add(a).add(b).fin()
    of mAbsI:
      # -->
      #   (a > 0) ? a : -a
      let a = gen(c, irs, arg(0))
      return start().add(cnkTernary).add(cnkInfix).add(a).op(copGt).intLit(0).add(a).add(cnkPrefix).op(copSub).add(a).fin()
    of mChr:
      return start().add(cnkCast).ident(c.gl.idents, CharType).add(gen(c, irs, arg(0))).fin()
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

func genLit(ast: var CAstBuilder, c: var GenCtx, lit: LiteralId) =
  ## Generates the AST for the given untyped literal `lit`

  # without type information, the only available piece of information we have
  # is the shape of the literal
  case lit.kind
  of lkNumber:
    # assume that it's a signed integer
    let intVal = c.env.data.getInt(lit)
    if intVal >= 0:
      ast.intLit(intVal.BiggestUInt).void()
    else:
      let abs = not(cast[BiggestUInt](intVal)) + 1
      ast.add(cnkPrefix).op(copSub).intLit(abs).void()
  of lkString:
    # treat as cstring
    ast.strLit(lit).void()
  of lkPacked, lkComplex:
    # XXX: those can reach here, which is a symptom of proper garbage
    #      collection still missing for the procedure body IR. For
    #      example, the of-branch matching makes use of complex literals (a
    #      slice-list), but it's used as a leaf node, so it stays around as
    #      garbage.
    #      For now, we simply ignore them here
    # TODO: use ``unreachable`` again
    #unreachable(lit.kind)
    discard

func genLit(ast: var CAstBuilder, c: var GenCtx, lit: LiteralId, typ: TypeId) =
  case c.env.types.kind(typ)
  of tnkInt:
    let intVal = c.env.data.getInt(lit)
    if intVal < 0:
      # compute the two's-complement, yielding the absolute value. This works
      # even if ``intVal == low(BiggestInt)``.
      # XXX: Nim doesn't guarantee that a signed integer is stored in
      #      two's-complement encoding
      let abs = not(cast[BiggestUInt](intVal)) + 1
      ast.add(cnkPrefix).op(copSub).intLit(abs).void()
    else:
      ast.intLit(intVal.BiggestUInt).void()
  of tnkUInt, tnkBool:
    # a bool is also stored and emitted as a uint
    ast.intLit(c.env.data.getUInt(lit)).void()
  of tnkChar:
    ast.add(cnkCharLit, c.env.data.getUInt(lit).uint32).void()
  of tnkFloat:
    let floatVal = c.env.data.getFloat(lit)
    case c.env.types[typ].size
    of 32:
      ast.add(cnkFloat32Lit, cast[uint32](floatVal.float32)).void()
    of 64, 128:
      ast.floatLit(floatVal).void()
    else:
      unreachable()
  of tnkCString:
    case lit.kind
    of lkNumber:
      # must be a nil-literal
      assert c.env.data.getUInt(lit) == 0
      ast.ident(c.gl.idents, "NIM_NIL").void()
    of lkString:
      ast.strLit(lit).void()
    else:
      unreachable(lit.kind)

  of tnkPtr, tnkRef:
    let intVal = c.env.data.getUInt(lit)
    # XXX: not strictly necessary - only done for improved readability
    if intVal == 0:
      ast.ident(c.gl.idents, "NIM_NIL").void()
    else:
      # TODO: make sure this works across compilers - a cast might be needed
      ast.intLit(intVal).void()

  of tnkProc:
    # only 'nil' procedural literals use a ``LiteralId``
    # XXX: maybe that's not a good idea and an ``ntkProc`` with ``NoneProc``
    #      should be used instead?
    assert c.env.data.getUInt(lit) == 0
    ast.ident(c.gl.idents, "NIM_NIL").void()
  else:
    # TODO: use ``unreachable`` again
    #unreachable(lit.kind)
    ast.add(genError(c, fmt"missing lit: {c.env.types.kind(typ)}")).void()

func genLit(c: var GenCtx, literal: Literal): CAst =
  let lit = literal.val
  if lit == NoneLit:
    # `nil` as the value is used for type literals
    start().add(cnkType, mapTypeV2(c, literal.typ).uint32).fin()
  elif literal.typ != NoneType:
    # a typed literal
    # a cast to the given type is used in order to communicate the literal's
    # type to C
    buildAst: builder.add(cnkCast).add(cnkType, mapTypeV2(c, literal.typ).uint32).genLit(c, lit, literal.typ)
  else:
    # an untyped literal
    # XXX: ideally, these shouldn't exist
    buildAst: genLit(builder, c, lit)

func startArrayInitializer(ast: var CAstBuilder, len: uint) =
  # arrays are wrapped into a struct, so double braces have to be used
  ast.add(cnkBraced, 1).void() # <- for the wrapper struct
  # XXX: only array literals with a len <= 2^32 are supported...
  ast.add(cnkBraced, len.uint32).void()


func genPackedArray(ast: var CAstBuilder, c: var GenCtx, data: LiteralData, lit: LiteralId) =
  startArrayInitializer(ast, data.packedLen(lit).uint)
  for u in data.uints(lit):
    discard ast.intLit(u)

func genDefaultVal(ast: var CAstBuilder, c: var GenCtx, id: TypeId) =
  ## Generates the default value for the type with the given `id`
  case c.env.types.kind(id)
  of tnkBool, tnkChar, tnkInt, tnkUInt:
    ast.intLit(0).void()
  of tnkPtr, tnkRef:
    ast.ident(c.gl.idents, "NIM_NIL").void()

  of tnkArray:
    let L = c.env.types.length(id)
    startArrayInitializer(ast, L)

    if L == 0:
      # empty array; early out
      return

    # the default value is the same across all elements, so we only create it
    # once and then re-use it
    let tmp = buildAst: builder.genDefaultVal(c, c.env.types.base(id))
    for _ in 0..<L:
      discard ast.add(tmp)

  of tnkRecord:
    let
      t = c.env.types[id]
      numFields = c.env.types.numFields(id)
      # TODO: a ``RecordIter`` is often used to only query field related
      #       information. Maybe we also need a ``FieldIter``?
      iter = initRecordIter(c.env.types, id)

    ast.add(cnkBraced, uint32(numFields + ord(t.base != NoneType))).void()
    if t.base != NoneType:
      # the base type is also stored as a field
      ast.genDefaultVal(c, t.base)

    # TODO: variant objects need to be supported too!

    # fill in the default value for each field
    for i in 0..<numFields:
      ast.genDefaultVal(c, c.env.types[iter.field(i)].typ)

  else:
    unreachable(c.env.types.kind(id))

type
  ArrayIter = object
    iter: DataIter
    pos: Option[int]
    i: int
    len: int

type DataRecordIter = object
  ## Encapsulates data relevant for iterating over the entries in a
  ## ``conRecord`` structure in a convenient way
  iter: #[var]# DataIter
  pos: Option[int] ## the 'position' part of the currently pointed to pair
  i: int   ## the index of the current pair
  len: int ## the number of pairs

func next(x: var DataRecordIter, d: LiteralData) =
  if x.i < x.len:
    x.iter.next() # move to the next 'position' part
    x.pos = some d.getRecPos(x.iter)
    inc x.i
  else:
    x.pos = none int

func enter(x: var DataRecordIter, data: LiteralData): var DataIter =
  result = x.iter

func fork(x: var DataIter, data: LiteralData): DataRecordIter =
  result.len = x.len.int
  result.iter = x # XXX: swap instead?
  result.i = 0
  result.next(data)

func join(x: var DataIter, y: sink DataRecordIter) =
  # XXX: if it'd be possible to have `x.iter` be a view, ``DataRecordIter``
  #      would be smaller and `join` would be unnecessary
  x = y.iter


func next(x: var ArrayIter, data: LiteralData): var DataIter =
  if x.i < x.len:
    #x.iter.next() # move to next item
    #debugEcho "post: ", x.iter.get(data)
    x.pos = some x.i

    inc x.i
  else:
    x.pos = none(int)

  result = x.iter

func forkA(x: var DataIter, data: LiteralData): ArrayIter =
  result.len = x.len.int
  result.iter = x # XXX: swap instead?

func join(x: var DataIter, y: sink ArrayIter) =
  # XXX: if it'd be possible to have `x.iter` be a view, ``DataRecordIter``
  #      would be smaller and `join` would be unnecessary
  x = y.iter

func genBracedObjConstr(ast: var CAstBuilder, c: var GenCtx, id: TypeId,
                        data: LiteralData, iter: var DataRecordIter)
func genBracedObjConstrPos(ast: var CAstBuilder, c: var GenCtx, id: TypeId,
                           data: LiteralData, iter: var ArrayIter)

# TODO: rename to ``genBracedInitializer``
func genConstInitializer(ast: var CAstBuilder, c: var GenCtx,
                         data: LiteralData, iter: var DataIter, id: TypeId
                        ): var CAstBuilder =
  # XXX: the fact that a mutable context (`c`) is required is a bit meh
  result = ast

  let n = iter.next(data)
  # first check if the initializer is a reference to another constant
  case n.kind
  of conConst:
    # a reference to a constant. The C standard doesn't mandate that
    # non-address lvalue expressions are *constant expressions* in the context
    # of static initializers, and not all C compilers treat them as such, so
    # they're embedded directly
    let sId = data.sym(iter).SymId
    assert c.gl.constInit[sId].len > 0, "initializer missing"

    return ast.add(c.gl.constInit[sId])
  of conConstAddr:
    # similar to ``conConst`` above, with the difference that we want the address
    let sId = data.sym(iter).SymId
    return ast.emitAddr().ident(c.gl.symIdents[sId.toIndex])
  else:
    # no special handling
    discard

  case c.env.types.kind(id)
  of tnkChar, tnkBool, tnkInt, tnkUInt, tnkFloat, tnkCString, tnkPtr, tnkRef:
    ast.genLit(c, data.getLit(iter), id)
  of tnkProc:
    ast.ident(c.gl.funcs[data.getExt(iter).ProcId.toIndex].ident).void()
  of tnkArray:
    let elemType = c.env.types.base(id)
    var sub = iter.enter(data)
    case sub.kind
    of conLit:
      # support string literals as the initializer for char arrays
      let val = data.getLit(iter)
      case val.kind
      of lkString:
        assert c.env.types.kind(elemType) == tnkChar, $c.env.types.kind(elemType)
        ast.strLit(val).void()
      of lkPacked:
        assert data.packedLen(val).uint == c.env.types.length(id)
        genPackedArray(ast, c, data, val)
      else:
        unreachable(val.kind)

    of conArray:
      # --> {..., ..., ...}
      startArrayInitializer(ast, sub.len.uint)
      for _ in 0..<sub.len:
        discard genConstInitializer(ast, c, data, sub, elemType)

    else:
      unreachable(n.kind)

    iter.close(sub)
  of tnkRecord:
    var sub = iter.enter(data)
    case sub.kind
    of conRecord:
      var obj = fork(sub, data)
      genBracedObjConstr(ast, c, id, data, obj)
      join(sub, obj)
    of conArray:
      var arr = forkA(sub, data)
      genBracedObjConstrPos(ast, c, id, data, arr)
      join(sub, arr)
    else:
      unreachable(n.kind)

    iter.close(sub)
  of tnkString, tnkSeq:
    # strings and seq are required to be lowered earlier
    unreachable("untransformed seq literal")
  else:
    unreachable(c.env.types.kind(id))



func genBracedObjConstr(ast: var CAstBuilder, c: var GenCtx, id: TypeId, data: LiteralData, iter: var DataRecordIter) =
  ## Returns the number of sub-nodes processed in `data` so far
  ## Consider the following object hierarchy:
  ##
  ## .. code-block::nim
  ##
  ##   # each field is annotated with it's position
  ##   type A = object of RootObj
  ##     x: int # 0
  ##     y: int # 1
  ##
  ##   type B = object of A
  ##     z: int # 2
  ##     q: int # 3
  ##
  ## Since in the layout used for the C target, the base record is stored in a
  ## member field itself, so the following braced initializer is required:
  ##
  ## .. code-block::c
  ##
  ##   { /* B */ { /* A */ { /* RootObj */ }, x, y }, z, q }
  ##
  ## The data to initialize the fields with is stored in `data` as a
  ## ``conRecord`` structure, which is an ordered collection of
  ## (position, data).
  ## If a field doesn't have corresponding pair entry in `data` the default
  ## value for the field's type is used.
  let
    base = c.env.types.base(id)
    numFields = c.env.types.numFields(id)

  # in the layout used for the C target, the base type is a member field
  # itself
  ast.add(cnkBraced, uint32(numFields + ord(base != NoneType))).void()

  if base != NoneType:
    # process and consume the entries for the base record first
    genBracedObjConstr(ast, c, id, data, iter)

  let
    # we use a ``RecordIter`` to query information about the fields
    rIter = c.env.types.initRecordIter(id)
    start = rIter.fieldPos(0)
    last = start + numFields - 1

  # TODO: support for variant objects is missing

  # walk each field in the record type
  for i in start..last:
    let pos = iter.pos.get(last + 1)

    if i == pos:
      genConstInitializer(ast, c, data, iter.enter(data), c.env.types[c.env.types.nthField(id, i.int)].typ).void()
      iter.next(data)
    else:
      assert pos > i
      # each field must have an initializer expression when using a braced
      # initializer - use a default value if none is specified by `data`
      genDefaultVal(ast, c, id)

func genBracedObjConstrPos(ast: var CAstBuilder, c: var GenCtx, id: TypeId, data: LiteralData, iter: var ArrayIter) =
  ## Generates the braced initializer for an object based using positional
  ## construction (``nkTupleConstr``)
  let
    base = c.env.types.base(id)
    len = c.env.types.numFields(id)

  #assert c.env.types.numFields(id) + start == len
  ast.add(cnkBraced, uint32(len + ord(base != NoneType))).void()

  # first consume the base type's fields
  if base != NoneType:
    genBracedObjConstrPos(ast, c, base, data, iter)

  let rIter = c.env.types.initRecordIter(id)
  for i in 0..<len:
    # note: `iter.next` modifies `iter.i`, so it's important to fetch the type first
    let typ = c.env.types[c.env.types.nthField(id, iter.i)].typ
    discard genConstInitializer(ast, c, data, iter.next(data), typ)

func genInitializerExpr(ast: var CAstBuilder, c: var GenCtx, data: LiteralData,
                        lit: LiteralId, typ: TypeId): var CAstBuilder =
  ## Generates an expression from the literal to use as an initializer
  case lit.kind
  of lkNumber, lkString:
    ast.genLit(c, lit, typ)
  of lkPacked:
    ast.genPackedArray(c, data, lit)
  of lkComplex:
    var iter = initDataIter(data, lit)
    ast.genConstInitializer(c, data, iter, typ).void()

  result = ast

func accessSuper(ast: var CAstBuilder, depth: int, start: CAst, supName: CIdent): var CAstBuilder =
  result = ast

  for _ in 0..<depth:
    discard ast.add(cnkDotExpr)

  discard ast.add(start)

  for _ in 0..<depth:
    discard ast.ident(supName)

func scanDeps(data: LiteralData, id: LiteralId, m: var ModuleCtx) =
  if id.kind == lkComplex:
    var iter = initDataIter(data, id)
    iter.moveInto(data)
    let L =
      case iter.kind
      of conRecord: iter.len * 2
      of conArray: iter.len
      else: 0
    for _ in 0..<L:
      let n = iter.next(data)
      case n.kind
      of conConst, conConstAddr:
        m.syms.incl data.sym(iter).SymId
      of conExt:
        useFunction(m, data.getExt(iter).ProcId)
      of conLit:
        let lit = data.getLit(iter)
        scanDeps(data, lit, m)
      of conArray, conRecord:
        assert false
      else:
        discard

template testNode(cond: bool, i: IRIndex) =
  if not cond:
    raise (ref PassError)(msg: fmt"{astToStr(cond)} failed", n: i)

func genSection(result: var CAst, c: var GenCtx, irs: IrStore3, merge: JoinPoint, numStmts: var int, pos: var IRIndex)


func needsTemp(x: ExprInfo): bool {.inline.} =
  ## If the value of an expression is referenced multiple times or if
  ## the expression is used in a partition different from the one it's located
  ## in, a temporary is introduced. For expressions with side-effects, this is
  ## required in order to adhere to the semantics of the code IR - for
  ## expressions without side-effects, it's not necessary
  x.rc == 2 or x.disjoint

# XXX: as can be seen by all the todo/xxx comments, there's still a lot to do
#      in this area. For a prototype, it's acceptable however.
func computeExpressions(code: IrStore3): seq[ExprInfo] =
  ## Computes the information about the expressions in `code` that are necessary
  ## for deciding what expressions have to go through temporaries in order for
  ## the generated code to match the code IR's evaluation order semantics
  # TODO: rename
  # TODO: add to the documentation concrete examples of what this procedure does
  # TODO: the places where temporaries are inserted is still not correct
  #       (especially in the area of function argument evaluation). Nim
  #       guarantees left-to-right evaluation of both side-effects and values for
  #       procedure arguments while C does **not**.
  # TODO: the documentation needs to be reworked here in general
  result.newSeq(code.len)
  var usesite: seq[uint32]
  usesite.newSeq(code.len)

  const Atoms = {ntkSym, ntkLit, ntkImm, ntkLocal, ntkParam, ntkProc}

  # XXX: ``ntkLocal`` and ``ntkSym`` (with the symbol of a global) don't
  #      mean "load local/global" - they just name them. The question now is:
  #      at which abstract machine instruction are the corresponding locations
  #      (or locations derived from them) loaded (i.e. their values observed)?
  #      The two possible solutions are:
  #      - directly at the instruction they're referenced from (this would
  #        include ``nktPathObj|ntkPathArr``)
  #      - at a ``ntkUse|ntkConsume|ntkModify`` or, if it's used as the callee
  #        of a procedure, right before the arguments are evaluated
  #
  #      A previous iteration of the code IR had the ``ntkLoad`` instruction
  #      to make this explicit, but it was phased out (maybe reinstate it?). 

  # since we're iterating backwards, we start with the highest possible
  # partition value. The value itself doesn't matter - the important thing is
  # that ``partition(x) < partition(y)`` if there exists one or more
  # - control-flow statement
  # - call with side-effects
  # - assignment
  # between ``x`` and ``y`` (where ``y`` comes after ``x`` in the code).
  var part = uint32(code.len-1)

  for i in countdown(code.len-1, 0):
    template use(other: IRIndex) =
      let idx = other
      usesite[idx] = max(usesite[idx], part)
      result[idx].rc += ord(result[idx].rc < 2).uint

    let n = code[i]
    result[i].disjoint = part != usesite[i]

    case n.kind
    of Atoms:
      discard "atoms"
    of ntkAsgn:
      use(n.srcLoc)
      dec part # TODO: is this really correct?
      use(n.wrLoc)
    of ntkAddr, ntkDeref:
      use(n.addrLoc)
    of ntkUse, ntkConsume:
      use(n.srcLoc)
    of ntkCall:
      # everything that has side-effects starts a new partition. Magics and
      # built-ins are treated as side-effect free
      if n.callKind == ckNormal:
        dec part
        use(n.callee)

      for it in code.rawArgs(i):
        use(it)

    of ntkPathObj:
      use(n.srcLoc)

    of ntkPathArr:
      use(n.srcLoc)
      use(n.arrIdx)

    of ntkConv, ntkCast:
      use(n.srcLoc)

    of ntkBranch:
      dec part
      use(n.cond)
    of ntkGoto, ntkGotoLink, ntkContinue, ntkJoin:
      # control-flow starts a new partition
      dec part
    of ntkLocEnd:
      discard "not relevant"

  # TODO: the documentation needs a bit more tweaking here. The wording needs
  #       to be very concise and precise, and the used terminology needs to be
  #       correct

  # propagate the side-effectness. If an expression with side-effects is
  # materialized (it's value is written to a temporary location), usage of
  # it is not considered to have side-effects, because with the
  # materialization, the side-effects of the expression were observed
  for i in 0..<code.len:
    template se(i: IRIndex): bool =
      bool(ord(result[i].hasSideEffect) and ord(not result[i].needsTemp))

    let n = code[i]

    result[i].hasSideEffect =
      case code[i].kind
      of Atoms: false
      of ntkAddr, ntkDeref:  se(n.addrLoc)
      of ntkPathArr:         se(n.srcLoc) or se(n.arrIdx)
      of ntkUse, ntkConsume, ntkPathObj, ntkConv, ntkCast:
        se(n.srcLoc)
      of ntkCall:
        case n.callKind
        of ckNormal: true # treated as always having side-effects
        of ckMagic, ckBuiltin:
          # calls to magics and built-ins have side-effects if one of their
          # operands have them
          var r = false
          for it in code.rawArgs(i):
            if se(it):
              r = true
              break

          r

      of ntkGoto, ntkGotoLink, ntkContinue, ntkBranch, ntkJoin, ntkAsgn, ntkLocEnd: false

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

  # TODO: re-use the ``exprs`` and ``names`` seqs
  c.exprs = computeExpressions(irs)
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


func isEmptyType(env: TypeEnv, id: TypeId): bool =
  # TODO: properly set the type for all built-in calls and remove the test
  #       for ``NoneType``
  id == NoneType or env.kind(id) == tnkVoid

func genArgs(ast: var CAstBuilder, c: GenCtx, code: IrStore3, call: IRIndex) =
  ## Generate the arguments for a non-imported function
  for it in code.args(call):
    discard ast.add(c.names[it])

func safeKind(env: TypeEnv, id: TypeId): TypeNodeKind {.inline.} =
  if id != NoneType: env.kind(id)
  else:              tnkVoid

func genArgsImported(ast: var CAstBuilder, c: var GenCtx, code: IrStore3,
                     call: IRIndex) =
  ## Generate the arguments for an imported function
  for it in code.args(call):
    case c.env.types.safeKind(c.types[it])
    of tnkArray:
      # the function expects a pure C-array
      ast.add(cnkDotExpr).add(c.names[it]).ident(c.gl.idents, ArrayInnerName).void()
    else:
      discard ast.add(c.names[it])


func genSection(result: var CAst, c: var GenCtx, irs: IrStore3, merge: JoinPoint, numStmts: var int, pos: var IRIndex) =
  # TODO: `numStmts` should be the return value, but right now it can't, since
  #       `result` is already in use
  # TODO: refactor this procedure
  template names: untyped = c.names
  template types: untyped = c.types
  template exprs: untyped = c.exprs

  let L = irs.len
  while pos < L:
    let
      n = irs.at(pos)
      i = pos

    inc pos

    const Stmts = {ntkAsgn, ntkBranch, ntkGoto, ntkGotoLink, ntkJoin, ntkContinue, ntkLocEnd}

    # XXX: skipping unreferenced expressions probably hides some bugs...
    if n.kind notin Stmts and c.exprs[i].rc == 0 and not c.exprs[i].hasSideEffect:
      # an expression that is not used and has no side-effects -> skip it
      continue

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
        scanDeps(c.env.data, c.env.syms.data(sId), c.m)

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
          let isImported = callee.kind == ntkProc and
                           sfImportc in c.env.procs[callee.procId].flags and
                           c.env.procs[callee.procId].decl.omit

          # XXX: this ``isImported`` hack makes sure that calls to
          #      ``posix.pipe`` compile for now
          if not isImported:
            genArgs(res, c, irs, i)
          else:
            genArgsImported(res, c, irs, i)

          names[i] = res.fin()

      if c.exprs[i].rc == 0:
        assert c.exprs[i].hasSideEffect
        result.add names[i]
        names[i].reset()
        inc numStmts

        # XXX: for instructions with ``rc == 0``, ``disjoint`` is always true
        #      (due to how it's computed), which would cause the logic below
        #      to try and emit a temporary. We short-circuit the logic for
        #      now, but a cleaner solution is needed. 
        continue

    of ntkAddr:
      names[i] = start().emitAddr().add(names[n.addrLoc]).fin()
    of ntkDeref:
      names[i] = start().emitDeref().add(names[n.addrLoc]).fin()
    of ntkAsgn:
      testNode names[n.srcLoc].len > 0, i
      result.add start().add(cnkInfix).add(names[n.wrLoc]).op(copAsgn).add(names[n.srcLoc]).fin()
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
    of ntkLocEnd:
      # do nothing; we can't communicate to the C compiler that the
      # referenced local isn't used anymore in the currrent control-flow path
      discard

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

    of ntkContinue, ntkGotoLink:
      # both are required to be lowered earlier
      unreachable(n.kind)

    else:
      names[i] = genError(c, fmt"missing impl: {n.kind}")
      if c.exprs[i].rc == 0:
        # make sure the error is present in the generated code by emitting it
        # as a statement
        result.add names[i]
        inc numStmts

    if n.kind notin Stmts and needsTemp(c.exprs[i]) and
       c.exprs[i].hasSideEffect:
      testNode(not isEmptyType(c.env.types, c.types[i]), i)

      let ident = c.gl.idents.getOrIncl(fmt"_cr{i}")
      result.add cnkDef, 1
      result.add cnkType, c.types[i].uint32
      result.add cnkIdent, ident.uint32
      result.add names[i]

      names[i] = start().ident(ident).fin()
      inc numStmts

proc emitCDecl(f: File, c: GlobalGenCtx, decl: CDecl)

proc emitType(f: File, c: GlobalGenCtx, t: CTypeId) =
  let info {.cursor.} = c.ctypes[t.int]
  if info.name != InvalidCIdent:
    f.write c.idents[info.name]
  else:
    # the declaration is emitted directly if a type has no name
    emitCDecl(f, c, info.decl)

# XXX: since strings are now stored as part of `LiteralData`, we need access
#      to it during emitting. To shorten the signatures a bit it might make
#      sense to store the `LiteralData` object as part of `GlobalGenCtx`.
#      That would more or less require splitting `LiteralData` out of
#      ``IrEnv``, but that probably a good idea anyway.
proc emitCAst(f: File, c: GlobalGenCtx, data: LiteralData, ast: CAst, pos: var int)

proc emitAndEscapeIf(f: File, c: GlobalGenCtx, data: LiteralData, ast: CAst, pos: var int, notSet: set[CAstNodeKind]) =
  if ast[pos].kind in notSet:
    emitCAst(f, c, data, ast, pos)
  else:
    f.write "("
    emitCAst(f, c, data, ast, pos)
    f.write ")"

proc writeChars[I: static int](f: File, arr: array[I, char]) {.inline.} =
  discard f.writeBuffer(unsafeAddr(arr), I)

func formatCChar(a: var array[4, char], ch: char): range[1..4] {.inline.} =
  ## Escapes the character with value `ch`, if necessary, and writes the
  ## result to `a`. Returns the length of the resulting string
  case ch
  of '\x00'..'\x1F', '\x7F'..'\xFF':
    a[0] = '\\'
    # clang doesn't accept strings like "\x000" and complains with the
    # message "hex escape sequence out of range". So instead, we use octal
    # escape sequences
    formatOctChar(a, 1, ord(ch).uint8)
    4
  of '\\', '\'', '\"':
    a[0] = '\\'
    a[1] = ch
    2
  else:
    a[0] = ch
    1

proc emitCAst(f: File, c: GlobalGenCtx, data: LiteralData, ast: CAst, pos: var int) =
  if pos >= ast.len:
    for it in ast:
      echo it

  template emitSub() =
    emitCAst(f, c, data, ast, pos)

  template emitSub(s: set[CAstNodeKind]) =
    emitAndEscapeIf(f, c, data, ast, pos, s)

  let n = ast[pos]
  inc pos

  case n.kind
  of cnkError:
    f.write "GEN_ERROR(\""
    f.write c.strings[n.a.LitId]
    f.write "\")"
  of cnkStmtList:
    for _ in 0..<n.a:
      emitSub()
      f.writeLine ";"

  of cnkDef:
    emitSub() # type
    f.write " "
    emitSub() # ident
    if n.a != 0'u32:
      f.write " = "
      emitSub() # initializer

  of cnkIdent:
    f.write c.idents[n.a.LitId]

  of cnkInfix:
    emitSub({cnkIdent, cnkIntLit, cnkStrLit}) # lhs
    f.write " "
    emitSub() # infix
    f.write " "
    emitSub({cnkIdent, cnkIntLit, cnkStrLit}) # rhs

  of cnkPrefix:
    emitSub()
    emitSub({cnkIdent})

  of cnkBracket:
    emitSub({cnkIdent, cnkDotExpr})
    f.write "["
    emitSub()
    f.write "]"

  of cnkCall:
    emitSub({cnkIdent}) # callee
    f.write "("
    for i in 0..<n.a:
      if i > 0:
        f.write ", "

      emitSub()

    f.write ")"

  of cnkIf:
    f.write "if ("
    emitSub() # condition
    f.writeLine ") {"
    emitSub() # stmt list
    f.write "}"

  of cnkWhile:
    f.write "while ("
    emitSub() # condition
    f.write ") {"
    emitSub() # stmt list
    f.write "}"

  of cnkReturn:
    f.write "return "
    if n.a == 1:
      emitSub()

  of cnkLabel:
    f.write c.idents[n.a.LitId]
    f.writeLine ":"
  of cnkGoto:
    f.write "goto "
    f.write c.idents[n.a.LitId]
  of cnkDotExpr:
    emitSub({cnkIdent, cnkCall, cnkDotExpr})
    f.write "."
    emitSub()
  of cnkCharLit:
    var arr = ['\'', '\\', 'x', '0', '0', '\'']
    formatHexChar(arr, 3, n.a.uint8)

    f.writeChars arr
  of cnkStrLit:
    f.write '"'
    let str = data.getStr(n.a.LiteralId)
    # XXX: escape the string prior to adding it to ``c.strings``?
    for ch in str.items:
      var arr: array[4, char]
      let len = formatCChar(arr, ch)
      discard f.writeBuffer(unsafeAddr arr, len)

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
    emitSub()
    f.write ") "
    emitSub({cnkIdent})

  of cnkOpToken:
    f.write COpToStr[COperator(n.a)]

  of cnkBraced:
    f.write "{"
    for i in 0..<n.a:
      if i > 0:
        f.write ", "
      emitSub()
    f.write "}"

  of cnkTernary:
    f.write "("
    emitSub() # condition
    f.write ") ? "
    emitSub({cnkIdent}) # a
    f.write " : "
    emitSub({cnkIdent}) # b

  else:
    f.write "EMIT_ERROR(\"missing " & $n.kind & "\")"

proc emitCAst(f: File, c: GlobalGenCtx, data: LiteralData, ast: CAst) =
  var pos = 0
  while pos < ast.len:
    emitCAst(f, c, data, ast, pos)


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

proc emitConst(f: File, ctx: GlobalGenCtx, data: LiteralData, syms: SymbolEnv, id: SymId, marker: var PackedSet[SymId]) =
  ## Emits the definition for the constant named by `id` and remember it in
  ## `marker`. If the constant is already present in `marker`, nothing is
  ## emitted.
  ## If the constant's intializer references other constants, those are
  ## emitted first
  if marker.containsOrIncl(id):
    return

  # recursively emit the constant's dependencies first
  for dep in ctx.constDeps[id].items:
    emitConst(f, ctx, data, syms, dep, marker)

  f.write "static const "
  emitType(f, ctx, syms[id].typ)
  f.write " "
  f.write ctx.idents[ctx.symIdents[id.toIndex]]
  f.write " = "
  emitCAst(f, ctx, data, ctx.constInit[id])
  f.writeLine ";"

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

iterator pairs[T](x: PackedSet[T]): (int, T) =
  var i = 0
  for it in x.items:
    yield (i, it)
    inc i

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

  block:
    # a constant needs to emitted in each module that references it. Instead
    # of re-generating the ``CAst`` for each module the constant is used in,
    # we create the initializer expression for all constants only once at the
    # start.
    # In this case, constant refers to *complex* constants (arrays, records,
    # etc.) not simple ones (ints, floats, etc.). The latter were already
    # inlined in ``transf``
    # XXX: it's very bad that we need a full ``GenCtx`` here. Some
    #      split-ups/reorderings are needed
    var ctx = GenCtx(env: unsafeAddr env)
    swap(ctx.gl, c)

    # iterate the items in reverse so that the initializer AST for all
    # dependencies is generated before their dependees. Constants can
    # only reference other constants with a higher ID
    for id in env.syms.ritems:
      let sym = env.syms[id]
      if sym.kind == skConst:
        let lit = env.syms.data(id)
        # note: `c` is swapped with `ctx.gl`
        ctx.gl.constInit[id] = start().genInitializerExpr(ctx, env.data, lit, sym.typ).fin()

        if lit.kind == lkComplex:
          # collect the dependencies on other constants for the constant and
          # store the result in a lookup-table for later
          # XXX: the pre-calculated dependency list is only used for emitting
          #      constant definitions in the right order, but there's a better
          #      solution for the ordering problem.
          #      Due to how constants are generated and later transformed, the
          #      following is true:
          #        for constants 'a' and 'b', if `a.id < b.id`, then 'b'
          #        *cannot* depend on 'a'
          #
          #      Emitting the used constants for a module in descending ID
          #      order would thus be enough to ensure correct definition order.
          #      ``PackedSet`` doesn't support iterating over it's items in
          #      that order however - ``TBitSet`` probably needs to be used
          #      instead
          scanDeps(env.data, lit, ctx.m)

          var deps = newSeq[SymId](ctx.m.syms.len)
          for i, it in ctx.m.syms.pairs:
            deps[i] = it

          ctx.gl.constDeps[id] = move deps

          # prepare for the next constant:
          ctx.m.syms.clear()
          ctx.m.funcs.clear()
        else:
          # non-complex constants don't have dependencies
          ctx.gl.constDeps[id] = @[]

    swap(ctx.gl, c)

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
    # the provided header string is allowed to not have the extra include
    # syntax:
    if h[0] in {'<', '"'}:
      f.write "#include "
      f.writeLine h
    else:
      # no '"' or '<' is present -> wrap the string in a '"' pair
      f.write "#include \""
      f.write h
      f.writeLine "\""

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

  # referenced globals
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
      discard "processed later"
    else:
      unreachable(sym.kind)

  block:
    # emit all used constants
    var symMarker: PackedSet[SymId]
    for id in mCtx.syms.items:
      let s = env.syms[id]

      if not s.decl.omit and s.kind == skConst:
        emitConst(f, ctx, env.data, env.syms, id, symMarker)

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
      emitCAst(f, ctx, env.data, it)
      f.writeLine "}"

    inc i