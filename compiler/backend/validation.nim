## Implements the grammer and type checker for the CGIR's in-memory
## representation.

import
  std/[
    macros,
    math,
    options,
    packedsets,
    strformat,
    tables
  ],
  std/private/[
    containers
  ],
  compiler/backend/[
    cgir2,
    pretty
  ]

type
  TypeKind = enum
    tkError
    tkNil
    tkSimple
    tkComplex
    tkNominal

  Type = object
    ## Type representation used by the type checker.
    case kind: TypeKind
    of tkComplex:
      n: NodeIndex
    of tkNominal:
      name: StringId
    of tkSimple:
      nt: CgNodeKind
    of tkError, tkNil:
      discard

  Qualifier {.pure.} = enum Mut, Lval
  QualType = tuple
    ## Qualified type.
    typ: Type
    qual: set[Qualifier]

  ProcContext = object
    ## Type checker context object.
    blocks: seq[tuple[isTry: bool, label: int]]
    ret: Type
    scopes: seq[Table[StringId, Type]]

  MsgContext* = object
    ## Contextual information about an error.
    top*: Option[NodeIndex]
      ## the index of the top-level tree node the error appears in
    where*: NodeIndex
      ## the node where the

  ErrorHandler = proc(m: CgModule, ast: Ast, ctx: MsgContext, msg: sink string)
  LocalHandler = proc(m: CgModule, ctx: MsgContext, msg: sink string)

  ErrorContext = object
    handler: LocalHandler
    top: NodeIndex
    steps: seq[NodeIndex]
      ## stack of tree roots leading to the currently processed node

const
  ErrorType = Type(kind: tkError)
  BoolType  = Type(kind: tkSimple, nt: cnkBoolTy)
  ValueType = {cnkBoolTy, cnkCharTy, cnkIntTy, cnkUIntTy, cnkFloatTy,
               cnkPtrTy, cnkStructTy, cnkUnionTy, cnkArrayTy, cnkOpaqueTy}

using
  c: var ProcContext
  m: CgModule
  ast: Ast
  tast: Ast
  pos: var NodeIndex
  err: var ErrorContext
  handler: ErrorHandler

template `+`(typ: Type, qual: set[Qualifier]): QualType = (typ, qual)
template `+`(typ: Type, qual: Qualifier): QualType      = typ + {qual}

func `==`(a, b: NodeIndex): bool {.borrow.}

proc emit(err; m; str: sink string) =
  ## Emits an error with message `str`.
  err.handler(m, MsgContext(top: some(err.top), where: err.steps[^1]), str)

proc emit(err; m; pos: NodeIndex, str: sink string) =
  ## Emits an error with message `str`, for the node at `pos`.
  err.handler(m, MsgContext(top: some(err.top), where: pos), str)

template step(err; n: NodeIndex) =
  ## Adds `n` as a context node for the remainder of the invoked-in scope.
  err.steps.add(n)
  defer: err.steps.shrink(err.steps.len - 1)

proc advance(ast; pos): CgNode =
  result = ast[pos]
  inc pos

proc skip(ast; pos) =
  pos = ast.next(pos)

proc len(n: CgNode): int =
  n.val.int

proc checkPos(ast; pos: NodeIndex): bool =
  ## Returns whether all nodes nominally part of the AST fragment at `pos` are
  ## really part of the buffer.
  if ord(pos) >= 0 and ord(pos) < ast.len:
    # compute the number of nodes the tree spans in a safe manner
    var i = pos.uint32
    var fin = i + 1
    while i < fin and i < uint32(ast.len):
      if not isLeaf(ast[i]):
        fin += ast[i].val
      inc i

    result = i == fin

proc checkSet[T](m; ast; pos; handler): bool =
  ## Checks whether the node at the current cursor represents an embedded set
  ## value with element type `E`.
  if isInt(m, ast[pos]): # ignore illformed int nodes
    let v = m.unpackUInt(advance(ast, pos).val)
    # make sure the bit pattern is well formed and is a valid set value
    let all = cast[uint64]({low(T) .. high(T)})
    if (v and not(all)) != 0:
      # emit an error, but still report a match
      handler(m, ast, MsgContext(where: pos), "set bits are invalid")
    true
  else:
    false

proc checkEnum[E: enum](m; ast; pos; handler): bool =
  ## Checks whether the node at the current cursor represents an embedded enum
  ## value of type `E`.
  if isInt(m, ast[pos]): # ignore illformed int nodes
    const candidates = block:
      var s: seq[uint64]
      for it in E.items:
        s.add uint64(it)
      s

    result =
      case m.unpackUInt(ast[pos].val)
      of candidates: true
      else:          false
    if result:
      inc pos

proc check(m; cond: bool, ast: Ast, name: string, at: var NodeIndex,
           context: NodeIndex; handler) =
  ## Helper procedure for `genGrammar`.
  if not cond:
    handler(m, ast, MsgContext(where: at)):
      fmt"expected production of '{name}'"
    at = ast.next(at)

macro genGrammar(name, entry, ast, body: untyped) =
  ## Generates a procedure with name `name` for verifying that some AST
  ## fragment is valid according to the grammar provided by `body`.
  ## `entry` is the entry non-terminal and `ast` is the name of the field
  ## storing the AST in the ``CgModule`` provided at run-time.
  let ast = ast # XXX: csources compiler bug workaround
  body.expectKind nnkStmtList
  var forward = newStmtList()
  var defs = newStmtList()

  forward.add quote do:
    template check(code: bool, name: untyped) =
      check(m, code, m.`ast`, astToStr(name), pos, context, handler)
    template checkSet(m, pos, handler, name): bool {.used.} =
      checkSet[name](m, m.`ast`, pos, handler)
    template checkEnum(m, pos, handler, name): bool {.used.} =
      checkEnum[name](m, m.`ast`, pos, handler)

  proc processElements(n: NimNode, to: NimNode) {.nimcall.} =
    var isDynamic = false
    # gather the minimum length
    for i in 1..<n.len:
      let it = n[i]
      case it.kind
      of nnkIdent, nnkCurly:
        discard
      of nnkPrefix:
        if isDynamic:
          error("production can only contain one repetition pattern", n)
        isDynamic = true
      else:
        error("unexpected syntax", n)

    var stmts = newStmtList()
    # gather the minimum length
    for i in 1..<n.len:
      let it = n[i]
      stmts.add:
        case it.kind
        of nnkIdent:
          quote do:
            when compiles(`it`(m, pos, handler)):
              check `it`(m, pos, handler), `it`
            else:
              check checkEnum(m, pos, handler, `it`), `it`
        of nnkCurly:
          it.expectLen 1
          let name = it[0]
          quote do:
            check checkSet(m, pos, handler, `name`), `it`
        of nnkPrefix:
          let bias = n.len - 2
          let name = it[1]
          quote do:
            for _ in `bias`..<len(n):
              check `name`(m, pos, handler), `name`
        else:
          unreachable()

    let kind = ident("cnk" & n[0].strVal)
    copyLineInfo(kind, n[0])
    if isDynamic:
      let count = n.len - 2
      to.add quote do:
        if n.kind == `kind` and len(n) >= `count`:
          inc pos
          `stmts`
          return true
    else:
      let count = n.len - 1
      to.add quote do:
        if n.kind == `kind` and len(n) == `count`:
          inc pos
          `stmts`
          return true

  proc processProdcutions(n: NimNode, to: NimNode) =
    case n.kind
    of nnkInfix:
      processProdcutions(n[1], to)
      processProdcutions(n[2], to)
    of nnkCall:
      processElements(n, to)
    of nnkIdent:
      to.add quote do:
        if `n`(m, pos, handler):
          return true
    else:
      error("unexpected syntax", n)

  for it in body.items:
    case it.kind
    of nnkCommand:
      it.expectLen 3
      let name = it[1]
      let p = it[2]
      forward.add quote do:
        template `name`(m; pos; handler): bool =
          `p`(m, m.`ast`, pos, handler)
    of nnkAsgn:
      it[0].expectKind nnkIdent
      let name = it[0]
      let decl = quote do:
        proc `name`(m; pos; handler): bool {.nimcall.}

      forward.add decl
      let def = copyNimTree(decl)
      def.body = newStmtList()
      def.body.add quote do:
        # fetch the current node, but don't advance the cursor yet
        let n {.used.} = m.`ast`[pos]
        let context {.used.} = pos
      processProdcutions(it[1], def.body)
      def.body.add
      discard defs.add(def)
    else:
      unreachable()

  result = quote do:
    proc `name`(m; pos; handler): bool {.nimcall.} =
      discard
  result.body.add forward
  result.body.add defs
  result.body.add quote do:
    `entry`(m, pos, handler)

template genCheckTerminal(n: untyped) {.dirty.} =
  proc `check n`(m; ast; pos; handler): bool =
    if ast[pos].kind == `cnk n`:
      if not `is n`(m, ast[pos]):
        handler(m, ast, MsgContext(where: pos), astToStr(n) & " node is ill-formed")
      inc pos
      return true

genCheckTerminal(Bool)
genCheckTerminal(Int)
genCheckTerminal(Float)
genCheckTerminal(String)
genCheckTerminal(Local)
genCheckTerminal(Datum)
genCheckTerminal(Global)
genCheckTerminal(Proc)
genCheckTerminal(Type)

proc checkLabel(m; ast; pos; handler): bool =
  if ast[pos].kind == cnkLabel:
    inc pos
    return true

genGrammar(checkProcDecl, def, ast):
  extern prc, checkProc
  extern global, checkGlobal
  extern local, checkLocal
  extern typ, checkType
  extern datum, checkDatum
  extern label, checkLabel
  extern string, checkString
  extern float, checkFloat
  extern int, checkInt
  extern bool, checkBool

  sym = prc or global or local or datum or Unknown(string, string)
  val = bool or int or float or string
  val_expr = Value(typ, val)
  addr_op = sym or expr
  path_elem = int or ExtField(typ, string) or expr
  expr = Use(typ, sym) or
         Call(expr, ...expr) or
         BitAnd(typ, expr, expr) or
         BitOr(typ, expr, expr) or
         BitXor(typ, expr, expr) or
         BitNot(typ, expr) or
         Shl(typ, expr, expr) or
         Shr(typ, expr, expr) or
         Add(typ, expr, expr) or
         Sub(typ, expr, expr) or
         Mul(typ, expr, expr) or
         Div(typ, expr, expr) or
         Mod(typ, expr, expr) or
         CheckedAdd(typ, typ, expr, expr, expr) or
         CheckedSub(typ, typ, expr, expr, expr) or
         CheckedMul(typ, typ, expr, expr, expr) or
         Not(typ, expr) or
         Neg(typ, expr) or
         Le(typ, typ, expr, expr) or
         Lt(typ, typ, expr, expr) or
         Eq(typ, typ, expr, expr) or
         Zext(typ, expr) or
         Sext(typ, expr) or
         FToU(typ, expr) or
         FToI(typ, expr) or
         IToF(typ, expr) or
         UToF(typ, expr) or
         Demote(typ, expr) or
         Promote(typ, expr) or
         Trunc(typ, expr) or
         Conv(typ, expr) or
         Bitcast(typ, expr) or
         PtrCast(typ, expr) or
         NilLit() or
         Value(typ, val) or
         Sizeof(typ, typ) or
         Alignof(typ, typ) or
         Offsetof(typ, typ, path_elem, ...path_elem) or
         Load(typ, expr) or
         Addr(typ, addr_op) or
         Path(typ, expr, path_elem, ...path_elem)

  dst = sym or expr
  pred = Unlikely(expr) or expr
  exit = label or Unwind()

  emit_arg = string or typ or sym or expr
  disp_target = Target(...val_expr, stmt)

  stmt = If(pred, stmt) or
         If(pred, stmt, stmt) or
         Dispatch(expr, ...disp_target, disp_target) or
         StmtList(...stmt) or
         Scope(...stmt) or
         Block(label, stmt) or
         Try(label, stmt) or
         Def(int, {CgLocAttrib}, typ, local) or
         Asgn(dst, expr) or
         Store(expr, expr) or
         Call(expr, ...expr) or
         CheckedCall(expr, ...expr, exit) or
         CheckedCallAsgn(expr, expr, ...expr, exit) or
         TailCall(expr, ...expr) or
         While(pred, stmt) or
         Return() or
         Return(expr) or
         Unreachable() or
         Break(label) or
         Raise(exit) or
         Drop(expr) or
         Emit(emit_arg, ...emit_arg) or
         Asm(AsmMode, emit_arg, ...emit_arg)

  param  = Param({CgParamAttrib}, local)
  params = Params(...param)
  def    = ProcDef({CgProcAttrib}, typ, prc, params, stmt) or
           ProcExp({CgProcAttrib}, typ, prc, params, stmt) or
           ProcImp({CgProcAttrib}, typ, prc)

genGrammar(checkConst, cnst, ast):
  extern typ, checkType
  extern dref, checkDatum
  extern global, checkGlobal
  extern prc, checkProc
  extern string, checkString
  extern float, checkFloat
  extern int, checkInt
  extern bool, checkBool

  sym = prc or global or dref or Unknown(string, string)
  use = Use(typ, sym)
  addr_op = sym or Path(typ, use, int, ...int)
  val = bool or int or string or float
  cnst = NilLit() or
         Addr(typ, addr_op) or
         PtrCast(typ, cnst) or
         Value(typ, val) or
         Sizeof(typ, typ) or
         Alignof(typ, typ) or
         Offsetof(typ, typ, int, ...int)

proc checkConst(m; ast; pos; handler): bool =
  ## Adapter to make the procedure available to `genGrammar`.
  checkConst(m, pos, handler)

genGrammar(checkConstr, constr, ast):
  extern typ, checkType
  extern cnst, checkConst
  extern int, checkInt

  expr = cnst or constr
  init = FieldInit(int, ...int, expr)
  constr = Constr(typ, ...expr) or
           RecConstr(typ, ...init)

proc checkConstr(m; ast; pos; handler): bool =
  ## Adapter to make the procedure available to `genGrammar`.
  checkConstr(m, pos, handler)

genGrammar(checkGlobalDecl, def, ast):
  extern constr, checkConstr
  extern cnst, checkConst
  extern int, checkInt
  extern typ, checkType
  extern global, checkGlobal

  init = cnst or constr
  def = GlobalDef(CgStorage, int, {CgLocAttrib}, typ, global) or
        GlobalDef(CgStorage, int, {CgLocAttrib}, typ, global, init) or
        GlobalImp(CgStorage, int, {CgLocAttrib}, typ, global) or
        GlobalExp(CgStorage, int, {CgLocAttrib}, typ, global)

genGrammar(checkTypeBody, body, tast):
  extern tref, checkType
  extern string, checkString
  extern int, checkInt
  extern bool, checkBool

  field = Field(typ, int, {CgLocAttrib}, int, string)
  typ = tref or body
  param = tref or body or Varargs()
  body = VoidTy() or IntTy(int) or FloatTy(int) or UIntTy(int) or
         CharTy() or BoolTy() or PtrTy(typ) or
         StructTy(bool, ...field, field) or
         UnionTy(bool, ...field, field) or
         ArrayTy(int, typ) or
         OpaqueTy(string, string) or
         ProcTy(CgCallConv, typ, ...param)

# ------------------- type queries --------------------------------------------

proc unqual(q: sink QualType): Type = q.typ

proc readType(m; typ: StringId): Type =
  let n = m.types[typ]
  case m.tast[n].kind
  of cnkStructTy, cnkUnionTy:
    Type(kind: tkNominal, name: typ)
  of cnkVoidTy, cnkBoolTy, cnkCharTy:
    Type(kind: tkSimple, nt: m.tast[n].kind)
  else:
    Type(kind: tkComplex, n: n)

proc readType(m; pos: NodeIndex): Type =
  case m.tast[pos].kind
  of cnkType:
    readType(m, m.tast[pos].val.StringId)
  of cnkVoidTy, cnkBoolTy, cnkCharTy:
    Type(kind: tkSimple, nt: m.tast[pos].kind)
  else:
    Type(kind: tkComplex, n: pos)

proc typeKind(m; typ: Type): CgNodeKind =
  case typ.kind
  of tkSimple:  typ.nt
  of tkNil:     cnkPtrTy
  of tkComplex: m.tast[typ.n].kind
  of tkNominal: m.tast[m.types[typ.name]].kind
  of tkError:   cnkVoidTy

proc resolved(m; typ: Type): Type =
  case typ.kind
  of tkNominal: readType(m, m.types[typ.name])
  else:         typ

proc width(typ: Type, m): int =
  if typ.kind == tkError:
    0
  else:
    int m.unpackInt(m.tast[resolved(m, typ).n, 0].val)

proc pointeeType(m; typ: Type): Type =
  case typeKind(m, typ)
  of cnkPtrTy:
    m.readType(m.tast.child(typ.n, 0))
  else:
    ErrorType

proc retType(typ: Type, m): Type =
  case typeKind(m, typ)
  of cnkProcTy:
    readType(m, m.tast.child(typ.n, 1))
  else:
    ErrorType

proc param(typ: Type, i: Natural, m): Type =
  ## Retrieves the `i`-th parameter type.
  case typeKind(m, typ)
  of cnkProcTy:
    if i + 2 < len(m.tast[typ.n]):
      readType(m, m.tast.child(typ.n, i + 2))
    elif m.tast[m.tast.last(typ.n)].kind == cnkVarargs:
      Type(kind: tkSimple, nt: cnkVarargs)
    else:
      ErrorType
  else:
    ErrorType

proc member(typ: Type, i: int64, m): Type =
  ## Retrieves the `i`-th member of struct/union type `typ`.
  case typ.kind
  of tkError, tkSimple, tkNil:
    ErrorType
  of tkNominal:
    member(Type(kind: tkComplex, n: m.types[typ.name]), i, m)
  of tkComplex:
    let idx = i + 1
    if m.tast[typ.n].kind in {cnkStructTy, cnkUnionTy} and
       idx in 1..<len(m.tast[typ.n]):
      let pos = m.tast.child(typ.n, idx)
      readType(m, m.tast.child(pos, 0))
    else:
      ErrorType

proc arrayElem(t: Type, m): Type =
  m.readType(m.tast.child(resolved(m, t).n, 1))

proc arrayLen(t: Type, m): int64 =
  m.unpackInt(m.tast[resolved(m, t).n, 0].val)

proc isDynArray(m; typ: Type): bool =
  typeKind(m, typ) == cnkArrayTy and arrayLen(typ, m) == 0

proc isSized(m; typ: Type): bool =
  ## Computes whether `typ` has a statically known size.
  case typeKind(m, typ)
  of cnkArrayTy:
    arrayLen(typ, m) > 0
  of cnkStructTy:
    let n = if typ.kind == tkNominal: m.types[typ.name] else: typ.n
    isSized(m, m.readType(m.tast.child(m.tast.last(n), 0)))
  of cnkVoidTy, cnkProcTy:
    false
  else:
    true ## all other types have a known static size

proc equal(m; a, b: Type): bool =
  ## Compares two types for equality.
  if a.kind != b.kind:
    return false

  proc equal(m; a, b: NodeIndex): bool =
    ## Compares two structural types for equality.
    if m.tast[a].kind != m.tast[b].kind:
      return false

    template equal(a, b: NodeIndex): bool =
      equal(m, readType(m, a), readType(m, b))

    case CgTypeKind(m.tast[a].kind)
    of cnkVoidTy, cnkBoolTy, cnkCharTy:
      true
    of cnkIntTy, cnkUIntTy, cnkFloatTy:
      m.tast[a, 0] == m.tast[b, 0] # width must be the same
    of cnkPtrTy:
      equal(m.tast.child(a, 0), m.tast.child(b, 0))
    of cnkArrayTy:
      m.tast[a, 0] == m.tast[b, 0] and
        equal(m.tast.child(a, 1), m.tast.child(b, 1))
    of cnkOpaqueTy:
      # header and name must be equal
      m.tast[a, 0] == m.tast[b, 0] and m.tast[a, 1] == m.tast[b, 1]
    of cnkProcTy:
      if m.tast[a, 0] == m.tast[b, 0] and m.tast[a].len == m.tast[b].len:
        var (p1, p2) = (m.tast.child(a, 1), m.tast.child(b, 1))
        for i in 1..<m.tast[a].len:
          if not equal(p1, p2):
            return false
          (p1, p2) = (m.tast.next(p1), m.tast.next(p2))
        true
      else:
        false
    of cnkStructTy, cnkUnionTy:
      unreachable()

  case a.kind
  of tkNil:     true
  of tkSimple:  a.nt == b.nt
  of tkComplex: a.n == b.n or equal(m, a.n, b.n)
  of tkNominal: a.name == b.name
  of tkError:   unreachable()

proc match(m; a, b: Type): bool =
  ## Computes whether a value of type `b` is also a value of type `a`.
  if a.kind == tkError or b.kind == tkError:
    true # the error type acts as both a top and bottom type
  elif typeKind(m, a) == cnkPtrTy and b.kind == tkNil:
    true # nil is part of every pointer type
  elif typeKind(m, a) == cnkVarargs:
    true # varargs fit everything
  else:
    equal(m, a, b) # use normal type equality

# ------------------- type checking -------------------------------------------

proc render(m; typ: Type): string =
  case typ.kind
  of tkComplex: render(m, m.tast, typ.n)
  of tkNominal: m.get(typ.name)
  of tkNil:     "<nil>"
  of tkSimple:  fmt"({typ.nt})"
  of tkError:   unreachable()

template formatValue(res: var string, t: Type, format: string) =
  res.add render(m, t)

proc formatValue(res: var string, t: CgNodeKind, formal: string) =
  let str = $t
  res.add substr(str, 3, str.high) # cut off the prefix

proc formatValue(res: var string, s: set[CgTypeKind], format: string) =
  let len = card(s)
  var i = 0
  for it in s.items:
    if i > 0:
      if i == len - 1: res.add ", or "
      else:            res.add ", "

    res.add "'"
    formatValue(res, it, format)
    res.add "'"
    inc i

proc typeMismatchMsg(m; expected, got: Type): string =
  fmt"expected expression of type '{expected}', but got '{got}'"

proc lookup(c; name: StringId): Option[Type] =
  for it in c.scopes.items:
    if name in it:
      return some(it[name])
  none(Type)

proc expectNode(m; ast; pos; kind: CgNodeKind, err) =
  let n = advance(ast, pos)
  if n.kind != kind:
    err.emit m, fmt"expected node with kind '{kind}', but got '{n.kind}'"

proc typeExpr(c; m; pos; err): QualType

proc require(c; m; pos; typ: Type, err) =
  let at = pos
  let got = unqual typeExpr(c, m, pos, err)
  if not match(m, typ, got):
    err.emit(m, at, typeMismatchMsg(m, typ, got))

proc require(c; m; pos; kind: set[CgTypeKind], err): Type =
  let at = pos
  result = unqual typeExpr(c, m, pos, err)
  if result.kind != tkError and typeKind(m, result) notin kind:
    err.emit m, at,
      fmt"expected one of {kind}, but got '{typeKind(m, result)}' type"

proc expect(typ: Type, kind: CgTypeKind, m; err) =
  if typ.kind != tkError and typeKind(m, typ) != kind:
    err.emit m, fmt"expected '{kind}', but got '{typeKind(m, typ)}' type"

proc expect(typ: var Type, kind: set[CgTypeKind], m, err) =
  if typ.kind != tkError and typeKind(m, typ) notin kind:
    err.emit m, fmt"expected one of {kind}, but got '{typeKind(m, typ)}' type"
    typ = ErrorType

proc expectStatic(typ: var Type, m, err) =
  if typ.kind != tkError and not isSized(m, typ):
    err.emit m, fmt"expected statically-sized type, but got '{typ}'"
    typ = ErrorType

proc requireTypeKind(m; typ: Type, kinds: set[CgTypeKind], err) =
  if typ.kind != tkError and typeKind(m, typ) notin kinds:
    err.emit m,
      fmt"expected type of kind '{kinds}', but got '{typeKind(m, typ)}'"

proc requireStaticType(m; typ: Type, err) =
  if typ.kind != tkError and not isSized(m, typ):
    err.emit m, fmt"expected statically-sized type, but got '{typ}'"

proc expectType(m; pos): Type =
  let n = advance(m.ast, pos)
  readType(m, n.val.StringId)

proc expectType(m; pos; kinds: set[CgTypeKind], err): Type =
  err.step(pos)
  result = expectType(m, pos)
  result.expect(kinds, m, err)

proc readSet[T](m; ast; pos): set[T] =
  cast[set[T]](m.unpackUInt(advance(ast, pos).val))

template context(at, body: untyped): untyped =
  if true:
    let at = pos
    body
  else:
    unreachable()

template context(err: var ErrorContext, pos: NodeIndex, name: untyped) =
  let name = pos
  err.step(pos)

proc validateTypeBody(m; pos; err)

proc checkAlign(m; n: CgNode, err) =
  let a = m.unpackInt(n.val)
  if a < 0:
    err.emit m, "invalid alignment"
  elif a > 0 and not isPowerOfTwo(int a):
    err.emit m, "alignment value must be a power of two"

proc checkTypeUse(m; pos; err): Type =
  case m.tast[pos].kind
  of cnkType:
    result = m.readType(advance(m.tast, pos).val.StringId)
  of cnkUnionTy, cnkStructTy:
    err.emit m, pos, "inline struct/union is not allowed in this context"
    skip(m.tast, pos)
    result = ErrorType
  else:
    result = m.readType(pos)
    validateTypeBody(m, pos, err)

proc checkInlineType(m; pos; err): Type =
  case m.tast[pos].kind
  of cnkType:
    result = m.readType(advance(m.tast, pos).val.StringId)
  else:
    result = m.readType(pos)
    validateTypeBody(m, pos, err)

proc checkField(m; pos; names: var PackedSet[StringId], isLast: bool, err) =
  context err, pos, at
  let n = m.tast.advance(pos)
  case n.kind
  of cnkField:
    let tpos = pos
    let typ = checkInlineType(m, pos, err)
    requireTypeKind(m, typ, ValueType, err)
    if not isLast or not isDynArray(m, typ):
      requireStaticType(m, typ, err)
    checkAlign(m, advance(m.tast, pos), err)
    skip(m.tast, pos)
    skip(m.tast, pos)
    let name = advance(m.tast, pos).val.StringId
    if m.get(name) == "":
      if m.tast[tpos].kind notin {cnkStructTy, cnkUnionTy}:
        err.emit m, at, "anonymous field must use inline struct/union type"
    elif containsOrIncl(names, name):
      err.emit m, at, "duplicate field name"
  else:
    unreachable()

proc validateTypeBody(m; pos; err) =
  context err, pos, at
  let n = m.tast.advance(pos)
  case n.kind
  of cnkVoidTy, cnkBoolTy, cnkCharTy:
    discard "nothing to do"
  of cnkIntTy, cnkUIntTy:
    let i = m.unpackInt(advance(m.tast, pos).val)
    if i notin [1'i64, 2'i64, 4'i64, 8'i64]:
      err.emit m, at, "the only supported widths for floats are 1, 2, 4, and 8"
  of cnkFloatTy:
    let i = m.unpackInt(advance(m.tast, pos).val)
    if i notin [4'i64, 8'i64]:
      err.emit m, at, "the only supported widths for floats are 4 and 8"
  of cnkPtrTy:
    # no restrictions on the target type
    discard checkTypeUse(m, pos, err)
  of cnkArrayTy:
    let len = m.unpackInt(advance(m.tast, pos).val)
    if len < 0:
      err.emit m, at, "array length must be >= 0"
    let got = checkTypeUse(m, pos, err)
    requireTypeKind(m, got, ValueType, err)
    requireStaticType(m, got, err)
  of cnkOpaqueTy:
    skip(m.tast, pos)
    skip(m.tast, pos)
  of cnkStructTy:
    var names: PackedSet[StringId]
    skip(m.tast, pos)
    for i in 1..<len(n):
      checkField(m, pos, names, (i == len(n) - 1), err)
  of cnkUnionTy:
    var names: PackedSet[StringId]
    skip(m.tast, pos)
    for _ in 1..<len(n):
      checkField(m, pos, names, false, err)
  of cnkProcTy:
    skip(m.tast, pos)
    let ret = checkTypeUse(m, pos, err)
    if typeKind(m, ret) != cnkVoidTy:
      requireStaticType(m, ret, err)
    for p in 2..<len(n):
      if p == len(n) - 1 and m.tast[pos].kind == cnkVarargs:
        skip(m.tast, pos)
      else:
        let got = checkTypeUse(m, pos, err)
        requireTypeKind(m, got, ValueType, err)
        requireStaticType(m, got, err)
  else:
    unreachable()

proc sym(c; m; pos; err): QualType =
  let n = advance(m.ast, pos)
  case n.kind
  of cnkUnknown:
    skip(m.ast, pos)
    skip(m.ast, pos)
    ErrorType + {Mut, Lval} # the type is not known
  of cnkLocal:
    let name = n.val.StringId
    let got = lookup(c, name)
    if got.isSome:
      got.unsafeGet + {Mut, Lval}
    else:
      err.emit(m, "undeclared local: " & m.get(name))
      ErrorType + {Mut, Lval}
  of cnkGlobal:
    let name = n.val.StringId
    if name in m.globals:
      readType(m, m.ast[m.globals[name], 3].val.StringId) + {Mut, Lval}
    else:
      err.emit m, pos, "undeclared global: " & m.get(name)
      ErrorType + {Mut, Lval}
  of cnkProc:
    let name = n.val.StringId
    if name in m.procs:
      readType(m, m.ast[m.procs[name], 1].val.StringId) + Lval
    else:
      err.emit m, pos, "undeclared proc: " & m.get(name)
      ErrorType + Lval
  of cnkDatum:
    # not mutable
    let id = n.val
    if id < m.data.nextId().uint32:
      readType(m, m.ast[m.data[id.Datum], 0].val.StringId) + Lval
    else:
      err.emit m, pos, "undeclared anonymous constant: " & $id
      ErrorType + Lval
  else:
    unreachable()

proc typeCall(c; m; pos; args: int, err): Type =
  ## `pos` is expected to point to the callee expression.
  var callee = unqual typeExpr(c, m, pos, err)
  if typeKind(m, callee) == cnkPtrTy:
    callee = pointeeType(m, callee)
  callee.expect(cnkProcTy, m, err)
  for i in 0..<args:
    context at:
      let formal = callee.param(i, m)
      require(c, m, pos, formal, err)
      if callee.kind != tkError and formal.kind == tkError:
        err.emit m, at, "too many arguments"

  let next = callee.param(args, m)
  if callee.kind != tkError and
     next.kind != tkError and typeKind(m, next) != cnkVarargs:
    # the parameter at the next position being valid and not varargs means
    # that there are parameter without a corrsponding argument
    err.emit m, fmt"not enough arguments"

  result = callee.retType(m)

proc typePath(c; m; pos; base: Type, len: int, err): Type =
  ## Makes sure the path operands at `pos` are well formed, returning the
  ## computed type of the path expression.
  var typ = base
  for _ in 0..<len:
    if typ.kind == tkError:
      skip(m.ast, pos)
      continue

    case m.ast[pos].kind
    of cnkInt:
      let at = pos
      let index = m.unpackInt(advance(m.ast, pos).val)
      case typeKind(m, typ)
      of cnkStructTy, cnkUnionTy:
        typ = member(typ, index, m)
        if typ.kind == tkError:
          err.emit m, at, "struct/union has no member with given index"
      of cnkArrayTy:
        if not isDynArray(m, typ) and (index < 0 or index >= arrayLen(typ, m)):
          err.emit m, at, "index outside of array bounds"
        typ = arrayElem(typ, m)
      else:
        err.emit m, at, fmt"cannot statically index into '{typ}'"
        typ = ErrorType
    of cnkExtField:
      discard advance(m.ast, pos)
      typ = expectType(m, pos)
      skip(m.ast, pos)
    of cnkExprs:
      if typeKind(m, typ) == cnkArrayTy:
        typ = arrayElem(typ, m)
      else:
        err.emit m, pos, "dynamic index is only valid for array types"

      discard require(c, m, pos, {cnkIntTy, cnkUIntTy, cnkOpaqueTy}, err)
    else:
      unreachable()

  result = typ

proc typeValue(m; pos; allowArr: bool, err): Type =
  result = expectType(m, pos)
  case typeKind(m, result)
  of cnkPtrTy:
    # special rule to allow for cstring values
    if isDynArray(m, pointeeType(m, result)) and
       typeKind(m, arrayElem(pointeeType(m, result), m)) == cnkOpaqueTy:
      expectNode(m, m.ast, pos, cnkString, err)
    else:
      err.emit m, pos, "invalid data for value"
      pos = m.ast.next(pos)
  of cnkBoolTy:
    expectNode(m, m.ast, pos, cnkBool, err)
  of cnkIntTy, cnkUIntTy, cnkCharTy:
    expectNode(m, m.ast, pos, cnkInt, err)
  of cnkFloatTy:
    expectNode(m, m.ast, pos, cnkFloat, err)
  of cnkOpaqueTy:
    let at = pos
    if advance(m.ast, pos).kind notin {cnkFloat, cnkInt}:
      err.emit m, at, "expected int or float"
  of cnkArrayTy:
    if allowArr:
      let n = advance(m.ast, pos)
      if n.kind == cnkString:
        if m.get(n.val.StringId).len > arrayLen(result, m):
          err.emit m, "character array is too large"
      else:
        err.emit m, "expected string"
    else:
      err.emit m, pos, "array value not supported in this context"
      pos = m.ast.next(pos)
  else:
    err.emit m, pos, "invalid data for value"
    pos = m.ast.next(pos)

proc typePathRoot(c; m; pos; err): QualType =
  let at = pos
  let got = typeExpr(c, m, pos, err)
  case typeKind(m, got.typ)
  of cnkPtrTy:
    const Roots = {cnkStructTy, cnkUnionTy, cnkOpaqueTy, cnkArrayTy}
    result = pointeeType(m, got.typ) + {Lval, Mut}
    if typeKind(m, result.typ) notin Roots:
      err.emit m, at,
        fmt"pointer target must be one of {Roots}, but type is '{result.typ}'"
  else:
    result = got

proc typeExpr(c; m; pos, err): QualType =
  ## Checks an expression for well-formedness, returning its type.
  proc promotion(c; m; pos; kinds: set[CgTypeKind], err): Type =
    let dst = expectType(m, pos, kinds, err)
    let src = require(c, m, pos, kinds, err)
    if width(src, m) >= width(dst, m):
      err.emit m, "destination must have larger width than source"
    result = dst

  proc demotion(c; m; pos; kinds: set[CgTypeKind], err): Type =
    let dst = expectType(m, pos, kinds, err)
    let src = require(c, m, pos, kinds, err)
    if width(src, m) <= width(dst, m):
      err.emit m, "destination must have smaller width than source"
    result = dst

  proc binaryOp(c; m; pos; kinds: set[CgTypeKind], err): Type =
    result = expectType(m, pos, kinds, err)
    require(c, m, pos, result, err)
    require(c, m, pos, result, err)

  proc floatOp(c; m; pos; a, b: CgTypeKind, err): Type =
    result = expectType(m, pos, {a}, err)
    discard require(c, m, pos, {b}, err)

  err.step(pos)
  let n = advance(m.ast, pos)
  case n.kind
  of cnkUse:
    let target = expectType(m, pos)
    let got = sym(c, m, pos, err)
    if not match(m, target, got.typ):
      err.emit m, "symbol's type doesn't match the specified one"
    target + got.qual
  of cnkValue:
    typeValue(m, pos, false, err) + {}
  of cnkNilLit:
    Type(kind: tkNil) + {}
  of cnkSizeof, cnkAlignof:
    let target = expectType(m, pos, {cnkUIntTy, cnkIntTy}, err)
    discard expectType(m, pos, ValueType, err)
    # the type being dynamically sized is fine in this context
    target + {}
  of cnkOffsetof:
    let target = expectType(m, pos, {cnkUIntTy, cnkIntTy}, err)
    let start = expectType(m, pos, {cnkStructTy, cnkUnionTy, cnkArrayTy}, err)
    discard typePath(c, m, pos, start, len(n) - 2, err)
    target + {}
  of cnkLoad:
    let target = expectType(m, pos, ValueType, err)
    requireStaticType(m, target, err)
    let got = unqual typeExpr(c, m, pos, err)
    if got.kind != tkError and not
       (typeKind(m, got) == cnkPtrTy and match(m, target, pointeeType(m, got))):
      err.emit m, fmt"expected '(Ptr {target})', but got '{got}'"
    target + {}
  of cnkAddr:
    let target = expectType(m, pos, {cnkPtrTy}, err)
    var elem = pointeeType(m, target)
    if isDynArray(m, elem):
      elem = pointeeType(m, elem)
    var got: QualType
    if m.ast[pos].kind in cnkSyms:
      got = sym(c, m, pos, err)
    else:
      got = typeExpr(c, m, pos, err)
    if Lval notin got.qual:
      err.emit m, "expected lvalue operand for Addr"
    if not match(m, elem, got.typ):
      err.emit m, typeMismatchMsg(m, elem, got.typ)
    target + {}
  of cnkPath:
    let target = expectType(m, pos)
    let base = typePathRoot(c, m, pos, err)
    let ret = typePath(c, m, pos, base.typ, len(n) - 2, err)
    if not match(m, target, ret):
      err.emit m, "path type doesn't match actual type"
    target + base.qual
  of cnkBitcast:
    const BitcastType = {cnkIntTy, cnkUIntTy, cnkFloatTy, cnkCharTy, cnkBoolTy}
    let target = expectType(m, pos, BitcastType, err)
    let dsize =
      case typeKind(m, target)
      of cnkIntTy, cnkUIntTy, cnkFloatTy: width(target, m)
      of cnkBoolTy, cnkCharTy:            1
      else:                               -1

    let src = require(c, m, pos, BitcastType, err)
    let ssize =
      case typeKind(m, src)
      of cnkIntTy, cnkUIntTy, cnkFloatTy: width(src, m)
      of cnkBoolTy, cnkCharTy:            1
      else:                               -1

    if dsize != -1 and ssize != -1 and dsize != ssize:
      err.emit m, "bitcast target and source type must have the same width"
    target + {}
  of cnkPtrCast:
    const PtrCastType = {cnkIntTy, cnkUIntTy, cnkPtrTy}
    let target = expectType(m, pos, PtrCastType, err)
    let src = require(c, m, pos, PtrCastType, err)
    if target.kind != tkError and src.kind != tkError and
       typeKind(m, target) notin PtrCastType and
       typeKind(m, src)    notin PtrCastType:
      err.emit m, "target or source type of 'PtrCast' must be pointer type"
    target + {}
  of cnkConv:
    const ConvType = {cnkIntTy, cnkUIntTy, cnkFloatTy, cnkBoolTy, cnkCharTy,
                      cnkPtrTy, cnkOpaqueTy}
    let target = expectType(m, pos, ConvType, err)
    let src = require(c, m, pos, ConvType, err)
    if target.kind != tkError and src.kind != tkError and
       typeKind(m, target) != cnkOpaqueTy and
       typeKind(m, src)    != cnkOpaqueTy:
      err.emit m, "target or source type of 'Conv' must be opaque type"
    target + {}
  of cnkBitNot:
    let target = expectType(m, pos, {cnkIntTy, cnkUIntTy}, err)
    require(c, m, pos, target, err)
    target + {}
  of cnkNeg:
    let target = expectType(m, pos, {cnkIntTy, cnkFloatTy}, err)
    require(c, m, pos, target, err)
    target + {}
  of cnkAdd, cnkSub, cnkMul, cnkDiv:
    binaryOp(c, m, pos, {cnkIntTy, cnkUIntTy, cnkFloatTy}, err) + {}
  of cnkCheckedAdd, cnkCheckedSub, cnkCheckedMul:
    let target = expectType(m, pos, {cnkBoolTy}, err)
    let argt = expectType(m, pos, {cnkIntTy, cnkUIntTy}, err)
    require(c, m, pos, argt, err)
    require(c, m, pos, argt, err)
    let p = unqual typeExpr(c, m, pos, err)
    if p.kind != tkError and typeKind(m, p) != cnkPtrTy or
       not match(m, pointeeType(m, p), argt):
      err.emit m, fmt"expected type '(PtrTy {argt})', but got '{p}'"
    target + {}
  of cnkMod, cnkBitAnd, cnkBitOr, cnkBitXor, cnkShl, cnkShr:
    binaryOp(c, m, pos, {cnkIntTy, cnkUIntTy}, err) + {}
  of cnkZext, cnkSext:
    promotion(c, m, pos, {cnkIntTy, cnkUIntTy}, err) + {}
  of cnkTrunc:
    demotion(c, m, pos, {cnkIntTy, cnkUIntTy}, err) + {}
  of cnkPromote:
    promotion(c, m, pos, {cnkFloatTy}, err) + {}
  of cnkDemote:
    demotion(c, m, pos, {cnkFloatTy}, err) + {}
  of cnkFToI:
    floatOp(c, m, pos, cnkIntTy, cnkFloatTy, err) + {}
  of cnkFToU:
    floatOp(c, m, pos, cnkUIntTy, cnkFloatTy, err) + {}
  of cnkIToF:
    floatOp(c, m, pos, cnkFloatTy, cnkIntTy, err) + {}
  of cnkUToF:
    floatOp(c, m, pos, cnkFloatTy, cnkUIntTy, err) + {}
  of cnkNot:
    let target = expectType(m, pos, {cnkBoolTy}, err)
    require(c, m, pos, target, err)
    target + {}
  of cnkLe, cnkLt:
    const Allowed = {cnkIntTy, cnkUIntTy, cnkFloatTy, cnkOpaqueTy, cnkPtrTy}
    let target = expectType(m, pos, {cnkBoolTy}, err)
    let typ = expectType(m, pos, Allowed, err)
    require(c, m, pos, typ, err)
    require(c, m, pos, typ, err)
    target + {}
  of cnkEq:
    const Allowed = {cnkBoolTy, cnkCharTy, cnkIntTy, cnkUIntTy, cnkFloatTy,
                     cnkOpaqueTy, cnkPtrTy}
    let target = expectType(m, pos, {cnkBoolTy}, err)
    let typ = expectType(m, pos, Allowed, err)
    require(c, m, pos, typ, err)
    require(c, m, pos, typ, err)
    target + {}
  of cnkCall:
    typeCall(c, m, pos, len(n) - 1, err) + {}
  of cnkUnlikely:
    require(c, m, pos, {cnkBoolTy}, err) + {}
  of AllNodes - cnkExprs - {cnkUnlikely}:
    unreachable()

proc checkJump(c; m; pos; isRaise: bool, err) =
  let at = pos
  let n = advance(m.ast, pos)
  for it in c.blocks.items:
    if it.label == n.val.int and it.isTry == isRaise:
      return # found one

  err.emit m, at, fmt"no target with label '{n.val}' exists"

proc checkExit(c; m; pos; err) =
  if m.ast[pos].kind == cnkUnwind:
    skip(m.ast, pos) # nothing to check
  else:
    checkJump(c, m, pos, isRaise=true, err)

proc checkLabelDef(c; m; pos; isTry: bool, err) =
  let at = pos
  let label = advance(m.ast, pos).val.int
  for it in c.blocks.items:
    if it.label == label:
      err.emit m, at, fmt"block/try with label '{label}' already exists"
      break

  # always add a new block, even if one with the given label exists already
  c.blocks.add (isTry, label)

proc closeBlock(c) =
  c.blocks.shrink(c.blocks.len - 1)

proc addLocal(c; m; name: StringId, typ: Type, err) =
  if lookup(c, name).isSome:
    err.emit m, "redefinition of local"
  c.scopes[^1][name] = typ

proc checkStmt*(c; m; pos; err): bool =
  ## Makes sure a statement/block is well-formed. Returns whether the
  ## statement acts as a terminator.
  template recurse(): bool =
    checkStmt(c, m, pos, err)

  template scoped(body: untyped) =
    c.scopes.add default(Table[StringId, Type])
    body
    c.scopes.shrink(c.scopes.len - 1)

  template terminator() =
    context at:
      if not recurse():
        err.emit m, at, "body must end in a terminator"

  err.step(pos)
  let n = advance(m.ast, pos)
  case n.kind
  of cnkDef:
    checkAlign(m, advance(m.ast, pos), err)
    let attribs = readSet[CgLocAttrib](m, m.ast, pos)
    var typ = expectType(m, pos, ValueType, err)
    typ.expectStatic(m, err)
    if CgLocAttrib.NoAlias in attribs and typeKind(m, typ) != cnkPtrTy:
      err.emit m, "noalias attribute is only allowed for pointer locations"
    let name = advance(m.ast, pos).val.StringId
    c.addLocal(m, name, typ, err)
    false
  of cnkAsgn:
    var dst =
      if m.ast[pos].kind in cnkSyms:
        sym(c, m, pos, err)
      else:
        typeExpr(c, m, pos, err)
    if Mut notin dst.qual:
      err.emit m, "destination must be mutable"
    require(c, m, pos, dst.typ, err)
    false
  of cnkStmtList:
    var res = false
    for i in 0..<len(n):
      if res:
        err.emit m, "terminator must not be followed by another statement"
      res = recurse()
    res
  of cnkScope:
    var returns: bool
    scoped: returns = recurse()
    returns
  of cnkRaise:
    checkExit(c, m, pos, err)
    true
  of cnkUnreachable:
    true
  of cnkStore:
    var dst = require(c, m, pos, {cnkPtrTy}, err)
    dst = pointeeType(m, dst)
    dst.expect(ValueType, m, err)
    dst.expectStatic(m, err)
    require(c, m, pos, dst, err)
    false
  of cnkWhile:
    require(c, m, pos, BoolType, err)
    scoped: discard recurse()
    false
  of cnkBlock:
    checkLabelDef(c, m, pos, isTry=false, err)
    scoped: discard recurse()
    closeBlock(c)
    false
  of cnkTry:
    checkLabelDef(c, m, pos, isTry=true, err)
    scoped: terminator()
    closeBlock(c)
    false
  of cnkBreak:
    checkJump(c, m, pos, isRaise=false, err)
    true

  of cnkReturn:
    case len(n)
    of 0: discard
    of 1: require(c, m, pos, c.ret, err)
    else: unreachable()
    true
  of cnkIf:
    require(c, m, pos, BoolType, err)
    scoped: discard recurse()
    if len(n) == 3:
      scoped: discard recurse()
    false
  of cnkDispatch:
    let typ = require(c, m, pos, {cnkIntTy, cnkUIntTy, cnkBoolTy, cnkCharTy}, err)
    for _ in 1..<len(n):
      let b = advance(m.ast, pos)
      for _ in 0..<len(b)-1:
        require(c, m, pos, typ, err)
      scoped: terminator()
    true
  of cnkDrop:
    let got = require(c, m, pos, ValueType, err)
    requireStaticType(m, got, err)
    false
  of cnkEmit, cnkAsm:
    # not checked
    dec pos
    pos = m.ast.next(pos)
    false
  of cnkCall:
    let typ = typeCall(c, m, pos, len(n) - 1, err)
    requireTypeKind(m, typ, {cnkVoidTy}, err)
    false
  of cnkTailCall:
    let ret = typeCall(c, m, pos, len(n) - 1, err)
    if not match(m, c.ret, ret):
      err.emit m, typeMismatchMsg(m, c.ret, ret)
    true
  of cnkCheckedCall:
    let typ = typeCall(c, m, pos, len(n) - 2, err)
    checkExit(c, m, pos, err)
    requireTypeKind(m, typ, {cnkVoidTy}, err)
    false
  of cnkCheckedCallAsgn:
    let res = context at:
      let got = typeExpr(c, m, pos, err)
      if Mut notin got.qual:
        err.emit m, at, "expected mutable location"
      unqual got
    context at:
      let typ = typeCall(c, m, pos, len(n) - 3, err)
      if not match(m, res, typ):
        err.emit m, at, typeMismatchMsg(m, res, typ)
    checkExit(c, m, pos, err)
    false
  of AllNodes - cnkStmts - cnkBlocks:
    unreachable(n.kind)

proc typeConst(m; pos; err): Type =
  ## Makes sure a constant expression is well-formed, returning its type.
  context err, pos, at
  let n = advance(m.ast, pos)
  case n.kind
  of cnkNilLit:
    result = Type(kind: tkNil)
  of cnkValue:
    result = typeValue(m, pos, true, err)
  of cnkAddr, cnkBitcast, cnkPtrCast, cnkSizeof, cnkAlignof, cnkOffsetof:
    # the syntax is a subset of that used by normal expressions, meaning
    # that the type checking logic for normal expression can be reused here
    dec pos
    var c = ProcContext()
    result = unqual typeExpr(c, m, pos, err)
  of cnkConstr:
    result = expectType(m, pos, {cnkStructTy, cnkArrayTy}, err)
    for i in 1..<len(n):
      context at:
        let got = typeConst(m, pos, err)
        let expect = member(result, i - 1, m)
        if not match(m, expect, got):
          err.emit m, at, typeMismatchMsg(m, expect, got)
  of cnkRecConstr:
    result = expectType(m, pos, {cnkStructTy, cnkArrayTy}, err)
    for _ in 1..<len(n):
      context at:
        let field = advance(m.ast, pos)
        var c = ProcContext()
        let expect = typePath(c, m, pos, result, len(field)-1, err)
        let got = typeConst(m, pos, err)
        if not match(m, expect, got):
          err.emit m, at, typeMismatchMsg(m, expect, got)
  else:
    unreachable(n.kind)

proc checkSyntax*(m: CgModule, handler: ErrorHandler) =
  ## Makes sure `m` is syntactically well-formed, reporting an error (via
  ## `handler`) everywhere it is not. The handler never being invoked means
  ## that the module is syntactically well-formed.
  template checkTree(ast: Ast, pos: NodeIndex, name: string, chck: untyped) =
    if checkPos(ast, pos):
      var p = pos
      if not chck(m, p, handler):
        handler(m, ast, MsgContext(top: some(pos), where: pos),
                "expected form of '" & name & "' non-terminal")
    else:
      handler(m, ast, MsgContext(where: pos), "malformed tree")

  for name, pos in m.types.pairs:
    checkTree(m.tast, pos, "type", checkTypeBody)

  for name, pos in m.data.pairs:
    checkTree(m.ast, pos, "constr", checkConstr)

  for name, pos in m.globals.pairs:
    checkTree(m.ast, pos, "global_decl", checkGlobalDecl)

  for name, pos in m.procs.pairs:
    checkTree(m.ast, pos, "proc_decl", checkProcDecl)

proc checkSemantics*(m: CgModule, handler: ErrorHandler) =
  ## Makes sure the syntactically well-formed `m` is semantically well-formed,
  ## reporting an error (via `handler`) everywhere it's not. Error diagnostics
  ## are formatted and dispatched to `handler` as they're discovered -- the
  ## handler never being invoked means that the module is semantically
  ## well-formed.
  var err = ErrorContext()
  var hasTypeError = false

  err.handler = proc(m; ctx: MsgContext, err: sink string) =
    handler(m, m.tast, ctx, err)
    hasTypeError = true

  # types are queried by the valdiation for all other entities; make sure
  # they're well-formed first
  for name, pos in m.types.pairs:
    var pos = pos
    validateTypeBody(m, pos, err)

  if hasTypeError:
    # don't continue when there are type errors
    return

  # switch to passing the non-type AST storage along to the handler
  err.handler = proc(m; ctx: MsgContext, err: sink string) =
    handler(m, m.ast, ctx, err)

  for name, pos in m.data.pairs:
    var pos = pos
    discard typeConst(m, pos, err)

  for name, pos in m.globals.pairs:
    context err, pos, at
    let L = len(m.ast[pos])
    var pos = m.ast.child(pos, 1)
    checkAlign(m, advance(m.ast, pos), err)

    let attribs = readSet[CgLocAttrib](m, m.ast, pos)
    if CgLocAttrib.Register in attribs:
      err.emit m, at, "'register' is not an allowed attribute for a global"

    let typ = expectType(m, pos, ValueType, err)
    if advance(m.ast, pos).val.StringId != name:
      err.emit m, at, "actual name doesn't match that of the table entry"
    if L == 6:
      let got = typeConst(m, pos, err)
      if not match(m, typ, got):
        err.emit m, at, typeMismatchMsg(m, typ, got)

  for name, pos in m.procs.pairs:
    context err, pos, at
    let kind = m.ast[pos].kind
    var pos = m.ast.child(pos, 0)
    let attribs = readSet[CgProcAttrib](m, m.ast, pos)
    if Inline in attribs and NoInline in attribs:
      err.emit m, at, "'inline' and 'noinline' are mutually exclusive"

    let typ = expectType(m, pos, {cnkProcTy}, err)
    if advance(m.ast, pos).val.StringId != name:
      err.emit m, at, "actual name doesn't match that of the table entry"

    case kind
    of cnkProcDef, cnkProcExp:
      var c = ProcContext()
      # register parameters and set the return type:
      c.scopes.add default(Table[StringId, Type])
      c.ret = typ.retType(m)
      let params = advance(m.ast, pos).len
      for i in 0..<params:
        err.step(pos)
        pos = m.ast.child(pos, 0)
        let attribs = readSet[CgParamAttrib](m, m.ast, pos)
        let param = param(typ, i, m)
        if CgParamAttrib.NoAlias in attribs and typeKind(m, param) != cnkPtrTy:
          err.emit m, "'noalias' attribute is only valid for pointer parameters"

        c.addLocal(m, advance(m.ast, pos).val.StringId, param, err)

      # then check the body
      if not checkStmt(c, m, pos, err):
        err.emit m, at, "procedure body doesn't end in a terminator"
    else:
      discard "nothing to do for imported procedures"
