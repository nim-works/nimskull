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
    types,
    trees
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/ic/[
    bitabs
  ],
  compiler/sem/[
    sighashes
  ],
  compiler/utils/[
    int128,
    pathutils
  ],
  compiler/vm/[
    vmir,
    irdbg
  ]

from compiler/modules/modulegraphs import `$`
from compiler/vm/vmdef import unreachable
from compiler/vm/vmaux import getEnvParam

from compiler/vm/irpasses import computeTypes, PassError

type
  TypeKey = distinct PType

  TypeSet = Table[int, PType] # TODO: only store unique types?
  SymSet = Table[int, PSym] # TODO: use a `HashSet`

  LocalGenCtx = object
    ## Mutable environmental state that only applies to the current item
    ## (routine) that is being code-gen'ed

  ModuleCtx = object
    ## Environmental state that is local to the module the IR being processed
    ## lies in.

    # TODO: use a data structure more efficient than `TIdTable`. Maybe
    #       translating to unique type, similar to how types in the VM
    #       are handled, make sense here
    types: OrderedTable[int, PType] # all used type for the module

    syms: Table[int, PSym] ## symbol-id -> symbol. All used symbols that need to be declared in the C code. # TODO: should be a SymSet

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

    cnkStrLit # string literal
    cnkIntLit

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

    cdnkIntLit # a unsigned integer literal (`a` encodes the high and `b` the low bits)

    cdnkFuncPtr # function-ptr type decl
    cdnkPtr # XXX: strictly speaking, the `*` is part of the declarator and not of the specifier
    cdnkBracket
    cdnkIdent

  CDecl = seq[tuple[kind: CDeclAstNodeKind, a, b: uint32]]

  CTypeDesc = distinct CDecl


  CTypeId = distinct uint32

  CIdent = LitId ## An identifier in the generated code

  CTypeInfo = object
    decl: CDecl
    name: CIdent #

  ProcHeader = object
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

  GlobalGenCtx = object
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
    funcs: seq[ProcHeader]

    ctypeMap: Table[TypeKey, CTypeId] #
    ctypes: seq[CTypeInfo] #

    defered: seq[(PType, CTypeId)]

  CAstBuilder = object
    ast: CAst

const VoidCType = CTypeId(0)
const StringCType = CTypeId(1)

const InvalidCIdent = CIdent(0) # warning: this depends on a implementation detail of `BiTable`

func hash(a: TypeKey): Hash =
  hash(PType(a).itemId)

func `==`(a, b: TypeKey): bool =
  a.PType.itemId == b.PType.itemId

func `==`(a, b: CTypeId): bool {.borrow.}

func mangledName(sym: PSym): string =
  # TODO: cache the mangled names (and don't use TLoc for it!)
  # TODO: implement
  sym.name.s

const BaseName = "Sub" ## the name of the field for the base type

func add(decl: var CDecl, k: CDeclAstNodeKind; a, b: uint32 = 0) =
  decl.add((k, a, 0'u32))

func addField(decl: var CDecl, typ: CTypeId, name: CIdent) =
  decl.add cdnkField
  decl.add cdnkType, typ.uint32
  decl.add cdnkIdent, name.uint32

func addField(decl: var CDecl, cache: var IdentCache, typ: CTypeId, name: sink string) {.inline.} =
  decl.addField(typ, cache.getOrIncl(name))

func addIntLit(decl: var CDecl, i: uint64) {.inline.} =
  decl.add cdnkIntLit, uint32(i shl 32), uint32(i and 0xFFFFFFFF'u64)

type CTypeMap = Table[TypeKey, CTypeId]

type TypeGenCtx = object
  # inherited state
  tm: CTypeMap # mutated
  ctypes: seq[CTypeInfo] # mutated
  cache: IdentCache # mutated

  # non-inherited state
  weakTypes: set[TTypeKind] # the set of types that can be turned into forward declarations when declared as a pointer

  forwardBegin: int
  forwarded: seq[PType] ## types who's creation was defered. THe first entry
                        ## has an ID of `forwardBegin`, the second
                        ## `forwardBegin + 1`, etc.

func requestType(c: var TypeGenCtx, t: PType): CTypeId =
  ## Requests the type-id for `t`. If the c-type for `t` doesn't exist yet, a
  ## slot for it is reserved and it's added to the `c.forwared` list
  let next = c.ctypes.len.CTypeId
  result = c.tm.mgetOrPut(t.TypeKey, next)
  if result == next:
    # type wasn't generated yet
    assert c.forwardBegin + c.forwarded.len == next.int
    c.ctypes.setLen(c.ctypes.len + 1)
    c.forwarded.add(t)

func requestFuncType(c: var TypeGenCtx, t: PType): CTypeId =
  # XXX: this is going to be tricky
  discard

func genRecordNode(c: var TypeGenCtx, decl: var CDecl, n: PNode): int =
  case n.kind
  of nkSym:
    let s = n.sym
    decl.addField(c.cache, c.requestType(s.typ), s.name.s)
    result = 1
  of nkRecList:
    for it in n.sons:
      discard genRecordNode(c, decl, it)

    result = n.len

  of nkRecCase:
    # TODO: properly name the generated fields, unions, and structs
    decl.addField(c.cache, c.requestType(n[0].sym.typ), n[0].sym.name.s)
    decl.add cdnkUnion, uint32(n.len-1)
    decl.add cdnkEmpty
    for i in 1..<n.len:
      let start = decl.len
      decl.add cdnkStruct
      decl.add cdnkEmpty

      let count = genRecordNode(c, decl, n[i][^1])
      decl[start].a = count.uint32

    result = 2 # discriminator field + union
  else:
    unreachable(n.kind)


func genCType(dest: var CDecl, cache: var IdentCache, t: PType)

func addWeakType(dest: var CDecl, c: var TypeGenCtx, t: PType) =
  let orig = t
  let t = t.skipTypes(abstractInst)

  # don't use a weak-dependency for ptr-like types
  let kind =
    if t.kind in c.weakTypes: cdnkWeakType
    else: cdnkType

  dest.add kind, c.requestType(orig).uint32

func genCTypeDecl(c: var TypeGenCtx, t: PType): CDecl =
  case t.kind
  of tyObject:
    let kind =
      if tfUnion in t.flags: cdnkUnion
      else:                  cdnkStruct

    result.add cdnkStruct, 0, cast[uint32](t.flags)
    # TODO: typ.sym may be nil
    if t.sym == nil:
      result.add cdnkEmpty
    else:
      result.add cdnkIdent, c.cache.getOrIncl(mangledName(t.sym)).uint32
    # base type
    if t[0] != nil:
      let base = t[0].skipTypes(skipPtrs)
      result.addField(c.cache, c.requestType(base), BaseName)
      inc result[0].a

    let count = genRecordNode(c, result, t.n)
    result[0].a += count.uint32

  of tyTuple:
    result.add cdnkStruct, t.len.uint32
    # TODO: typ.sym may be nil. Edit: It is, but why again?
    if t.sym == nil:
      result.add cdnkEmpty
    else:
      result.add cdnkIdent, c.cache.getOrIncl(mangledName(t.sym)).uint32
    if t.n != nil:
      # a named tuple
      discard genRecordNode(c, result, t.n)
    else:
      # an anonymous tuple
      for i in 0..<t.len:
        result.addField(c.cache, c.requestType(t[i]), fmt"Field{i}")

  of tyArray:
    result.add cdnkBracket
    # not a weak-dep, since the a complete type is required
    result.add cdnkType, c.requestType(t.elemType).uint32
    # TODO: pass a valid ConfigRef
    {.cast(noSideEffect).}:
      result.addIntLit lengthOrd(nil, t).toUInt64()

  of tyProc:
    case t.callConv
    of ccClosure:
      result.add cdnkStruct, 2
      # TODO: typ.sym may be nil
      if t.sym != nil:
        result.add cdnkIdent, c.cache.getOrIncl(mangledName(t.sym)).uint32
      else:
        result.add cdnkEmpty

      result.addField(c.cache, c.requestFuncType(t), "ClP_0")
      result.addField(c.cache, c.requestType(t.lastSon), "ClE_0")

    else:
      result.add cdnkFuncPtr, uint32(t.len - 1) # -1 for the always present return type
      for it in t.sons:
        if it == nil:
          result.add cdnkType, VoidCType.uint32
        else:
          # function pointer declarations don't require complete types
          result.addWeakType(c, it)

  of tyRef, tyPtr, tyVar, tyLent:
    result.add cdnkPtr
    # we only need a weak-dep for the pointer's element type
    result.addWeakType(c, t.elemType)

  of tyUncheckedArray:
    result.add cdnkBracket
    result.addWeakType(c, t.elemType)
    # ``SEQ_DECL_SIZE`` is a macro defined in ``nimbase.h``
    result.add cdnkIdent, c.cache.getOrIncl("SEQ_DECL_SIZE").uint32

  of tyGenericInst, tyOwned:
    result = genCTypeDecl(c, t.lastSon.skipTypes(abstractInst))
  of tyDistinct, tyRange, tyOrdinal:
    result = genCTypeDecl(c, t[0].skipTypes(abstractInst))

  else:
    # XXX: using `genCType` doesn't feel right
    genCType(result, c.cache, t)

  assert result.len > 0

func getTypeName(c: var IdentCache, typ: PType): CIdent =
  # TODO: not finished
  if typ.sym != nil:
    c.getOrIncl(mangledName(typ.sym))
  else:
    let h = hashType(typ)
    c.getOrIncl(fmt"{typ.kind}_{h}")


func genForwarded(c: var TypeGenCtx) =
  ## Generates the `CTypeInfo` for all forwarded types (and also for their
  ## dependencies)
  var i = 0
  # note: ``genCTypeDecl`` may add to ``forwarded``
  while i < c.forwarded.len:
    let fwd = c.forwarded[i]
    # XXX: forwarded could be cleared when ``i == forwarded.high`` in
    #      order to cut down on allocations
    let decl = genCTypeDecl(c, c.forwarded[i].skipTypes(abstractInst))
    c.ctypes[c.forwardBegin + i] = CTypeInfo(decl: decl, name: getTypeName(c.cache, fwd))
    inc i

  c.forwarded.setLen(0)
  c.forwardBegin = c.ctypes.len # prepare for following ``requestType`` calls

func genCType(dest: var CDecl, cache: var IdentCache, t: PType) =
  template addIdentNode(n: string) =
    dest.add cdnkIdent, cache.getOrIncl(n).uint32

  const
    NumericalTypeToStr: array[tyInt..tyUInt64, string] = [
      "NI", "NI8", "NI16", "NI32", "NI64",
      "NF", "NF32", "NF64", "NF128",
      "NU", "NU8", "NU16", "NU32", "NU64"]

  case t.kind
  of tyVoid: addIdentNode("void")
  of tyPointer, tyNil:
    dest.add cdnkPtr
    addIdentNode("void")
  of tyInt..tyUInt64:
    addIdentNode(NumericalTypeToStr[t.kind])
  of tyCstring:
    addIdentNode("NIM_CHAR")
  of tyBool:
    addIdentNode("NIM_BOOL")
  else:
    addIdentNode(fmt"genCType_missing_{t.kind}")

func genCType(cache: var IdentCache, t: PType): CTypeInfo =
  # TODO: name handling is unfinished
  genCType(result.decl, cache, t)
  result.name = getTypeName(cache, t)


func useFunction(c: var ModuleCtx, s: PSym) =
  ##
  if lfHeader in s.loc.flags:
    c.headers.incl getStr(s.annex.path)
  elif lfNoDecl notin s.loc.flags:
    discard c.syms.mgetOrPut(s.id, s)

func useType(c: var ModuleCtx, t: PType) =
  c.types[t.id] = t

#[
func useTypeWeak(c: var ModuleCtx, t: PType): CTypeId=
  c.types

func useType(c: var ModuleCtx, t: PType): CTypeId =
]#

func requestFunction(c: var GlobalGenCtx, s: PSym): int =
  ## Requests the ID of the C-function `s` maps to
  assert s.kind in routineKinds
  let nextId = c.funcs.len
  result = c.funcMap.mgetOrPut(s.id, nextId)
  if result != nextId:
    assert result < nextId
    # the header's content is generated later; we just reserve the slot here
    c.funcs.setLen(c.funcs.len + 1)


func requestTypeName(c: var GlobalGenCtx, t: PType): CIdent =
  # TODO: not finished
  if t.sym != nil:
    c.idents.getOrIncl(mangledName(t.sym))
  else:
    c.idents.getOrIncl(fmt"requestTypeName_missing_{t.kind}")

type GenCtx = object
  f: File
  tmp: int
  sym: PSym

  names: seq[CAst] # IRIndex -> expr
  types: seq[PType]
  config: ConfigRef

  gl: GlobalGenCtx # XXX: temporary
  m: ModuleCtx # XXX: temporary

func gen(c: GenCtx, irs: IrStore3, n: IRIndex): CAst =
  c.names[n]
  #"gen_MISSING"

func mapTypeV3(c: var GlobalGenCtx, t: PType): CTypeId

func mapTypeV2(c: var GenCtx, t: PType): CTypeId =
  # TODO: unfinished
  c.m.useType(t) # mark the type as used

func mapTypeV3(c: var GlobalGenCtx, t: PType): CTypeId =
  let k = t.TypeKey
  result = c.ctypeMap[k]

func genProcHeader(c: var GlobalGenCtx, t: PType): ProcHeader =
  assert t.kind == tyProc

  result.returnType =
    if t[0].isEmptyType(): VoidCType
    else:                  mapTypeV3(c, t[0])

  result.args.newSeq(t.len - 1)
  for i in 1..<t.len:
    result.args[i - 1] = (mapTypeV3(c, t[i]),
                          c.idents.getOrIncl(t.n[i].sym.name.s))


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

func intLit(c: var CAstBuilder, v: BiggestInt): var CAstBuilder =
  result = c
  # TODO: int literals need some more development
  c.ast.add cnkIntLit, uint32(cast[uint64](v) shr 32), uint32(cast[uint64](v) and 0xFFFFFFFF'u64)

func strLit(c: var CAstBuilder, strs: var BiTable[string], s: sink string): var CAstBuilder =
  result = c
  c.ast.add cnkStrLit, strs.getOrIncl(s).uint32

func sub(c: var CAstBuilder): var CAstBuilder =
  result = c

func fin(c: sink CAstBuilder): CAst =
  swap(result, c.ast) # XXX: `swap` is used for refc-compatibility

func genError(c: var GenCtx, str: string): CAst =
  # XXX: cnkError always takes a string literal for now
  result.add cnkError, c.gl.strings.getOrIncl(str).uint32

func genArithm(c: var GenCtx, i: IRIndex, check: bool): CAst =
  c.genError("genArithm_missing")

func getTypeArg(irs: IrStore3, arg: IRIndex): PType =
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

func genBuiltin(c: var GenCtx, irs: IrStore3, bc: BuiltinCall, n: IrNode3): (CAst, PType) =
  case bc
  of bcNewClosure:
    (genBraced(c.gl.ident("NIM_NIL"), c.gl.ident("NIM_NIL")), nil) # XXX: hmm, closure type is known during irgen time...
  of bcOverflowCheck:
    (genArithm(c, n.args(0), true), nil) # TODO: use the type of ``n.args(0)``
  of bcTestError:
    var ast = start()
    (ast.add(cnkCall, 1).sub().ident(c.gl.idents, "NIM_UNLIKELY").emitDeref(c.gl.idents).sub().ident(c.gl.idents, "err").fin(), nil) # TODO: use tyBool
  of bcCast:
    let dstTyp = n.typ
    var ast = start()
    discard ast.add(cnkCast).add(cnkType, mapTypeV2(c, dstTyp).uint32).add(gen(c, irs, n.args(0)))
    (ast.fin(), dstTyp)
  of bcRaise:
    var ast = start()
    if argCount(n) == 0:
      # re-raise
      discard ast.add(cnkCall, 0).ident(c.gl.idents, "reraiseException")
    else:
      # TODO: empty arguments
      discard ast.add(cnkCall, 5).ident(c.gl.idents, "raiseExceptionEx")
      discard ast.add(c.gen(irs, n.args(0))).add(c.gen(irs, n.args(1)))
      discard ast.ident(c.gl.idents, "NIM_NIL").ident(c.gl.idents, "NIM_NIL").intLit(0)

    (ast.fin(), nil)

  else:
    (genError(c, fmt"missing: {bc}"), nil)
    #unreachable(bc)

type MagicKind = enum
  mkUnary
  mkBinary
  mkCall

func genMagic(c: var GenCtx, irs: IrStore3, m: TMagic, n: IrNode3): CAst =
  let (kind, sym) =
    case m
    of mNot: (mkUnary, "!")
    of mEqRef: (mkBinary, "==")
    else:
      return genError(c, fmt"missing magic: {m}")

  case kind
  of mkUnary:
    # TODO: assert arg count == 1
    result = start().add(cnkPrefix).ident(c.gl.idents, sym).add(gen(c, irs, n.args(0))).fin()
  of mkBinary:
    result = start().add(cnkInfix).add(gen(c, irs, n.args(0))).ident(c.gl.idents, sym).add(gen(c, irs, n.args(1))).fin()
  of mkCall:
    result = genError(c, fmt"missing magic call: {sym}")


func nthField(n: PNode, pos: int): PSym =
  case n.kind
  of nkSym:
    if n.sym.position == pos:
      result = n.sym
  of nkRecList:
    for it in n.sons:
      result = nthField(it, pos)
      if result != nil:
        return
  of nkRecCase:
    if n[0].sym.position == pos:
      return n[0].sym

    for i in 1..<n.len:
      result = nthField(n[i].lastSon, pos)
      if result != nil:
        return
  else:
    unreachable(n.kind)

# XXX: I'm very sure there exists a proc that does the same in the compiler
#      code already
func nthField(t: PType, pos: int): PSym =
  # TODO: also traverse base types
  assert t.kind == tyObject

  if t.n != nil:
    result = nthField(t.n, pos)

  if result == nil and t.len > 0 and t[0] != nil:
    result = nthField(t[0].skipTypes(skipPtrs), pos)

func safeKind(t: PType): TTypeKind {.inline.} =
  if t == nil: tyVoid
  else:        t.kind

func genLit(c: var GenCtx, lit: PNode): CAst =
  case lit.kind
  of nkIntLit:
    start().intLit(lit.intVal).fin()
  of nkStrLit:
    if lit.typ == nil:
      # XXX: some passes insert string literals without type information. It's supported for now
      # treat as cstring
      start().strLit(c.gl.strings, lit.strVal).fin()
    else:
      case lit.typ.kind
      of tyString, tyDistinct:
        # XXX: the string lit handling is probably too late here and should be
        #      done as part of the `seq` lowering passes instead
        # XXX: this currently only takes the old GC-based strings into account
        if lit.strVal.len == 0:
          # XXX: yeah, this is bad. The lowering needs to happen at the IR level
          start().add(cnkCast).ident(c.gl.idents, "NimStringDesc*").ident(c.gl.idents, "NIM_NIL").fin()
        else:
          genError(c, fmt"missing lit: non-empty tyString")
      of tyCstring:
        start().strLit(c.gl.strings, lit.strVal).fin()
      else:
        unreachable(lit.typ.kind)
  of nkNilLit:
    start().ident(c.gl.idents, "NIM_NIL").fin()
  else:
    genError(c, fmt"missing lit: {lit.kind}")

template testNode(cond: bool, i: IRIndex) =
  if not cond:
    debugEcho astToStr(cond), " failed"
    debugEcho "node: ", i
    printIr(irs, exprs)
    for e in irs.traceFor(i).items:
      debugEcho e
    if irs.at(i).kind == ntkLocal:
      debugEcho "trace for local:"
      for e in irs.traceForLocal(irs.getLocalIdx(i)).items:
        debugEcho e
    doAssert false

proc genCode(c: var GenCtx, irs: IrStore3): CAst =
  var i = 0
  template names: untyped = c.names
  template types: untyped = c.types
  template f: untyped = c.f

  var numStmts = 0
  result.add cnkStmtList

  var tmp = 0
  for typ, sym in irs.locals:
    if sym != nil:
      if lfHeader in sym.loc.flags:
        let str = getStr(sym.annex.path)
        continue
      elif lfNoDecl in sym.loc.flags:
        continue

    result.add cnkDef
    result.add cnkType, mapTypeV2(c, typ).uint32
    if sym != nil: # TODO: don't test for temps like this
      result.add c.gl.ident mangledName(sym)

    else:
      result.add c.gl.ident(fmt"_tmp{tmp}")
      inc tmp

    inc numStmts

  let exprs = calcStmt(irs)
  names.newSeq(irs.len)

  for n in irs.nodes:
    case n.kind
    of ntkSym:
      let sym = irs.sym(n)
      # TODO: refactor
      if sym.kind in routineKinds and sym.magic == mNone:
        useFunction(c.m, sym)
      elif sym.kind in {skVar, skLet} and sfGlobal in sym.flags:
        c.m.syms[sym.id] = sym
        #discard mapTypeV3(c.gl, sym.typ) # XXX: temporary

      if sym.kind notin routineKinds and sym.typ != nil:
        useType(c.m, sym.typ)

      names[i] = start().ident(c.gl.idents, mangledName(sym)).fin()
    of ntkLocal:
      let (kind, typ, sym) = irs.getLocal(i)
      if sym == nil:
        names[i] = start().ident(c.gl.idents, "_tmp" & $c.tmp).fin()
        inc c.tmp
      else:
        names[i] = start().ident(c.gl.idents, mangledName(sym)).fin()

    of ntkCall:
      if n.isBuiltIn:
        let (name, typ) = genBuiltin(c, irs, n.builtin, n)
        names[i] = name
      else:
        let callee = irs.at(n.callee)
        if callee.kind == ntkSym and irs.sym(callee).magic != mNone:
          names[i] = genMagic(c, irs, irs.sym(callee).magic, n)
        else:
          var res = start().add(cnkCall, n.argCount.uint32).add(names[n.callee])
          for it in n.args:
            discard res.add names[it]
          names[i] = res.fin()

      # TODO: we're missing a proper way to check whether a call is a statement
      if not exprs[i]:#irs.isStmt(n):
        result.add names[i]
        inc numStmts
    of ntkAddr:
      names[i] = start().emitAddr(c.gl.idents).add(names[n.addrLoc]).fin()
    of ntkDeref:
      let t = types[n.addrLoc].skipTypes(abstractInst)
      testNode t.kind in {tyPtr, tyRef, tyVar, tyLent, tySink}, n.addrLoc
      names[i] = start().emitDeref(c.gl.idents).add(names[n.addrLoc]).fin()
    of ntkAsgn:
      result.add start().add(cnkInfix).add(names[n.wrLoc]).ident(c.gl.idents, "=").add(names[n.srcLoc]).fin()
      inc numStmts
    of ntkPathObj:
      let typ = types[n.srcLoc].skipTypes(abstractInst)
      let src = names[n.srcLoc]
      let idx = n.fieldIdx
      var ast = start().add(cnkDotExpr).add(src)
      case typ.kind
      of tyObject:
        let f = typ.nthField(n.fieldIdx)
        discard ast.ident(c.gl.idents, mangledName(f))
      of tyTuple:
        if typ.n != nil:
          discard ast.ident(c.gl.idents, typ.n[idx].sym.mangledName())
        else:
          # annonymous tuple
          discard ast.ident(c.gl.idents, fmt"Field{idx}")

      else:
        testNode false, n.srcLoc

      names[i] = ast.fin()

    of ntkPathArr:
      names[i] = start().add(cnkBracket).add(names[n.srcLoc]).add(names[n.arrIdx]).fin()
    of ntkLit:
      names[i] = genLit(c, irs.getLit(n))
    of ntkUse:
      names[i] = names[n.srcLoc]
    of ntkBranch:
      result.add cnkIf
      result.add names[n.cond]
      result.add cnkStmtList, 1
      result.add cnkGoto, c.gl.idents.getOrIncl(fmt"label{n.target}").uint32
      inc numStmts
    of ntkJoin:
      if irs.isLoop(n.joinPoint):
        result.add genError(c, "loop impl missing")
        discard#f.writeLine "while (true) {"
      else:
        result.add cnkLabel, c.gl.idents.getOrIncl(fmt"label{n.joinPoint}").uint32
      inc numStmts
    of ntkGoto:
      if irs.isLoop(n.target):
        # there exists only one `goto loop` and it's at the end of the loop
        # XXX: very brittle
        result.add genError(c, "loop impl missing")
      else:
        result.add cnkGoto, c.gl.idents.getOrIncl(fmt"label{n.target}").uint32

      inc numStmts
    else:
      names[i] = genError(c, fmt"missing impl: {n.kind}")
      if not exprs[i]:
        result.add names[i]
        inc numStmts

    inc i

  # exit
  if c.sym.typ.n[0].typ.isEmptyType():
    result.add cnkReturn
  else:
    result.add cnkReturn, 1
    result.add cnkIdent, c.gl.idents.getOrIncl("result").uint32
  inc numStmts

  echo numStmts
  result[0].a = numStmts.uint32

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
    else:
      f.write ";"

  of cnkIdent:
    f.write c.idents[n.a.LitId]

  of cnkInfix:
    emitCAst(f, c, ast, pos) # lhs
    f.write " "
    emitCAst(f, c, ast, pos) # infix
    f.write " "
    emitCAst(f, c, ast, pos) # rhs

  of cnkPrefix:
    emitCAst(f, c, ast, pos)
    f.write "("
    emitCAst(f, c, ast, pos)
    f.write ")"

  of cnkBracket:
    emitAndEscapeIf(f, c, ast, pos, {cnkIdent})
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

  of cnkReturn:
    f.write "return"
    if n.a == 1:
      emitCAst(f, c, ast, pos)

  of cnkLabel:
    f.write c.idents[n.a.LitId]
    f.writeLine ":"
  of cnkGoto:
    f.write "goto "
    f.write c.idents[n.a.LitId]
  of cnkDotExpr:
    f.write "("
    emitCAst(f, c, ast, pos)
    f.write ")."
    emitCAst(f, c, ast, pos)

  of cnkStrLit:
    f.write '"'
    f.write c.strings[n.a.LitId]
    f.write '"'

  of cnkIntLit:
    f.write (n.a.uint64 shl 64) or n.b.uint64

  of cnkType:
    emitType(f, c, n.a.CTypeId)

  of cnkCast:
    f.write "("
    emitCAst(f, c, ast, pos)
    f.write ") ("
    emitCAst(f, c, ast, pos)
    f.write ")"

  of cnkBraced:
    f.write "{"
    for i in 0..<n.a:
      if i > 0:
        f.write ", "
      emitCAst(f, c, ast, pos)
    f.write "}"

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

    f.write:
      case info.decl[0].kind
      of cdnkStruct: "struct "
      of cdnkUnion: "union "
      else: unreachable()

    f.write c.idents[info.name]

  of cdnkPtr:
    emitCDecl(f, c, decl, pos)
    f.write "*"

  of cdnkIdent:
    f.write c.idents[n.a.CIdent]

  of cdnkIntLit:
    let val = (n.a.uint64 shl 32) or n.b.uint64
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

proc emitCType(f: File, c: GlobalGenCtx, info: CTypeInfo) =
  var pos = 0

  assert info.decl.len > 0, c.idents[info.name]

  let kind = info.decl[0].kind
  case kind
  of cdnkStruct, cdnkUnion:
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

proc writeDecl(f: File, c: GlobalGenCtx, h: ProcHeader, name: PSym) =
  emitType(f, c, h.returnType)
  f.write(" ")
  f.write(mangledName(name))
  f.write("(")
  for i, it in h.args.pairs:
    if i > 0:
      f.write ", "

    emitType(f, c, it.typ)

  f.writeLine(");")

proc writeDef(f: File, c: GlobalGenCtx, h: ProcHeader, name: PSym) =
  emitType(f, c, h.returnType)
  f.write(" ")
  f.write(mangledName(name))
  f.write("(")
  for i, it in h.args.pairs:
    if i > 0:
      f.write ", "

    emitType(f, c, it.typ)
    f.write " "
    f.write c.idents[it.name]

  f.writeLine(") {")

proc emitModuleToFile*(conf: ConfigRef, filename: AbsoluteFile, procs: openArray[(PSym, IrStore3)]) =
  let f = open(filename.string, fmWrite)
  defer: f.close()

  echo "Here: ", filename.string

  var
    ctx: GlobalGenCtx
    mCtx: ModuleCtx
    asts: seq[CAst]

    tgc = TypeGenCtx(weakTypes: {tyObject, tyTuple})

  template swapTypeCtx() =
    swap(tgc.tm, ctx.ctypeMap)
    swap(tgc.ctypes, ctx.ctypes)
    swap(tgc.cache, ctx.idents)

  ctx.ctypes.add(CTypeInfo(name: ctx.idents.getOrIncl("void"))) # the `VoidCType`
  # XXX: we need the `NimStringDesc` PType here
  #ctx.ctypes.add(CTypeInfo(name: ctx.idents.getOrIncl("NimString"))) # XXX: wrong, see above
  mCtx.headers.incl("\"nimbase.h\"")

  tgc.forwardBegin = ctx.ctypes.len

  for sym, irs in procs.items:
    useFunction(mCtx, sym)

    if sfImportc in sym.flags:
      asts.add(default(CAst))
      continue

    echo "genFor: ", sym.name.s, " at ", conf.toFileLineCol(sym.info)
    var c = GenCtx(f: f, config: conf, sym: sym)
    # doing a separate pass for the type computation instead of doing it in
    # `genCode` is probably a bit less efficient, but it's also simpler;
    # requires less code duplication; and is also good for modularity
    c.types = computeTypes(irs)

    swapTypeCtx()

    # request all types used inside the IR to be setup. Note that this only
    # means that the C-type equivalents are created, not that the declarations
    # are also emitted in the output file
    for t in c.types.items:
      if t != nil:
        discard tgc.requestType(t)

    swapTypeCtx()

    swap(c.gl, ctx)
    swap(c.m, mCtx)
    asts.add genCode(c, irs)
    swap(c.m, mCtx)
    swap(c.gl, ctx)

  swapTypeCtx()
  # XXX: this might lead to an ordering problem, since we're not registering
  #      the types on the first occurence
  # mark the types used in routine signatures as used
  for sym in mCtx.syms.values:
    case sym.kind
    of routineKinds:
      for it in sym.typ.sons:
        if it != nil:
          discard tgc.requestType(it)
          mCtx.useType(it)
    else:
      discard tgc.requestType(sym.typ)
      mCtx.useType(sym.typ)

  tgc.genForwarded()

  swapTypeCtx()

  var used: seq[CTypeId]

  block:
    for typ in mCtx.types.values:
      used.add ctx.ctypeMap[typ.TypeKey]

  for i, t in ctx.ctypes.pairs:
    assert t.name != InvalidCIdent, $i

  f.writeLine "#define NIM_INTBITS 64" # TODO: don't hardcode

  # headers
  for h in mCtx.headers.items:
    f.writeLine fmt"#include {h}"

  # type section

  proc emitWithDeps(f: File, c: GlobalGenCtx, t: CTypeId,
                    marker: var PackedSet[CTypeId]) =
    let info {.cursor.} = c.ctypes[t.int]
    if info.name == InvalidCIdent or marker.containsOrIncl(t):
      # nothing to do
      return

    # scan the type's body for non-weak dependencies and emit them first
    for n in info.decl.items:
      if n.kind == cdnkType:
        emitWithDeps(f, c, n.a.CTypeId, marker)

    if info.decl.len > 0:
      # only emit types that have a declaration
      emitCType(f, c, info)

  var marker: PackedSet[CTypeId]
  for it in used.items:
    emitWithDeps(f, ctx, it, marker)


  # generate all procedure forward declarations
  for sym in mCtx.syms.values:
    case sym.kind
    of routineKinds:
      #echo "decl: ", sym.name.s, " at ", conf.toFileLineCol(sym.info)
      let hdr = genProcHeader(ctx, sym.typ)

      writeDecl(f, ctx, hdr, sym)
    of skLet, skVar:
      emitType(f, ctx, ctx.ctypeMap[sym.typ.TypeKey])
      f.write " "
      f.write mangledName(sym)
      f.writeLine ";"
    of skConst:
      f.writeLine "EMIT_ERROR(\"missing logic: const\")"
    else:
      unreachable(sym.kind)

  var i = 0
  for it in asts.items:
    if it.len == 0:
      inc i
      continue

    let (sym, _) = procs[i]
    let hdr = genProcHeader(ctx, sym.typ)
    writeDef(f, ctx, hdr, sym)
    try:
      emitCAst(f, ctx, it)
    except:
      echo "emit: ", sym.name.s, " at ", conf.toFileLineCol(sym.info)
      raise
    f.writeLine "}"
    inc i