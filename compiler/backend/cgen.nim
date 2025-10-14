## Implements the C code generator. The code generator takes CGIR modules as
## input and produces C translation units, nothing else.
##
## Only a subset of the CGIR is supported:
## * no support for exception handling
## * limited support for array types; array types always have to be named

import
  std/[
    math, # for float classification
    packedsets,
    strutils,
    tables
  ],
  std/private/[
    containers
  ],
  compiler/backend/[
    cgir2
  ],
  compiler/ic/[
    bitabs
  ]

import system/formatfloat # for float rendering

type
  Writer = object
    indent: int
    output: string
    currLine: uint16
    currFile: StringId
    # immutable input state:
    withLine: bool
      ## whether line directives are enabled
    anon: Table[Datum, uint32]
      ## datum -> zero-based name suffix

using
  m: CgModule
  ast: Ast
  pos: var NodeIndex

const
  CallingConvToStr = [
    Default: "N_NOCONV",
    Nimcall: "N_NIMCALL",  Stdcall:  "N_STDCALL",
    Cdecl:   "N_CDECL",    Safecall: "N_SAFECALL",
    Syscall: "N_SYSCALL",  Fastcall: "N_FASTCALL"
  ]
  PreferIdentified = {cnkStructTy, cnkUnionTy, cnkArrayTy, cnkProcTy}
    ## types that must not be inlined where a name exists

func `==`(a, b: Datum): bool {.borrow.}

proc advance(ast, pos): CgNode {.inline.} =
  result = ast[pos]
  inc pos

proc skip(ast, pos) {.inline.} =
  pos = ast.next(pos)

proc toSet[T](m; val: uint32, _: typedesc[T]): set[T] =
  cast[set[T]](m.unpackUInt(val))

proc len(n: CgNode): int =
  n.val.int

proc readInt(m; ast, pos): int64 =
  m.unpackInt(advance(ast, pos).val)
proc readUInt(m; ast, pos): uint64 =
  m.unpackUInt(advance(ast, pos).val)
proc readSet[T](m; ast; pos; _: typedesc[T]): set[T] =
  m.toSet(advance(ast, pos).val, T)

proc resolve(m: CgModule, n: NodeIndex): NodeIndex =
  if m.tast[n].kind == cnkType:
    m.types[m.tast[n].val.StringId]
  else:
    n

proc getType(m; pos: NodeIndex): NodeIndex =
  ## Returns the index of the type AST representing the result type of the
  ## expression at `n`.
  case m.ast[pos].kind
  of cnkExprs - {cnkCall, cnkNilLit}:
    m.types[m.ast[pos, 0].val.StringId]
  of cnkCall:
    # fetch the return type of the callee's proc type
    var callee = getType(m, m.ast.child(pos, 0))
    if m.tast[callee].kind == cnkPtrTy:
      callee = resolve(m, m.tast.child(callee, 0))
    m.tast.child(callee, 1)
  else:
    unreachable(m.ast[pos].kind)

proc add(r: var Writer, str: string) =
  r.output.add str
proc addInt(r: var Writer, i: int64) =
  r.output.addInt i
proc addInt(r: var Writer, i: uint64) =
  r.output.addInt i
proc addFloat(r: var Writer, f: float) =
  r.output.addFloat f

proc addChar(r: var Writer, c: char) =
  ## Emits character value `c`, using a C escape sequence where required.
  case c
  of '\0'..'\x1F', '\x7F'..'\xFF':
    r.output.add '\\'
    r.output.add toOctal(c)
  of '\'', '\"', '\\', '?':
    r.output.add '\\'
    r.output.add c
  else:
    r.output.add c

proc addEscaped(r: var Writer, s: string) =
  r.output.add '"'
  for c in s.items:
    r.addChar(c)
  r.output.add '"'

proc newLineRaw(r: var Writer) =
  r.output.add '\n'
  for i in 0..<r.indent:
    r.output.add "  "

proc newLine(r: var Writer, m: CgModule, src: uint32) =
  ## Emits the line break and, if enabled and applicable, a line directive.
  if r.withLine:
    if src > 0:
      let info = m.infos[src - 1]
      if info.line != r.currLine or info.file != r.currFile:
        # emit a line directive, without indenting first
        r.output.add "\n#line "
        r.output.addInt info.line
        if r.currFile != info.file:
          r.output.add ' '
          r.addEscaped(m.get(info.file))
          r.currFile = info.file
        r.currLine = info.line
        r.newLineRaw()
      # else: keep the C code on the same line
    else:
      if r.currLine != 0:
        # add a line directive that points to nowhere, so that the code won't
        # show up in a debugger
        r.output.add "\n#line 1 \"<none>\""
        r.currLine = 0
        r.currFile = StringId(0)
      r.newLineRaw()
  else:
    r.newLineRaw()

proc newLine(r: var Writer) =
  ## Emits a new line, taking into account line directives.
  if not r.withLine or r.currLine == 0:
    r.newLineRaw()

proc startBlock(r: var Writer) =
  r.output.add '{'
  inc r.indent

proc closeBlock(r: var Writer) =
  dec r.indent
  r.newLine()
  r.output.add "}"

proc typeToC(m; pos; r: var Writer)

proc genQualDecl(m; name: StringId, attribs: set[CgLocAttrib], bitsize: int,
                 r: var Writer) =
  ## Emits a qualified non-function C declaration without a type specifier.
  if Volatile in attribs:
    r.add "volatile "
  if CgLocAttrib.NoAlias in attribs:
    r.add "NIM_NOALIAS "
  r.add m.get(name)
  if bitsize > 0:
    r.add ":"
    r.addInt bitsize

proc genDecl(m; pos; name: StringId, attribs: set[CgLocAttrib], bitsize: int,
             r: var Writer) =
  ## Emits a full non-function C declaration with the type at `pos`, the
  ## declarator `name`, and with qualifiers computed from `attribs` and
  ## `bitsize`.
  case m.tast[pos].kind
  of cnkArrayTy:
    inc pos
    let len = m.readInt(m.tast, pos)
    genDecl(m, pos, name, attribs, bitsize, r)
    r.add "["
    if len > 0:
      r.addInt len
    r.add "]"
  else:
    typeToC(m, pos, r)
    r.add " "
    genQualDecl(m, name, attribs, bitsize, r)

proc memberToC(m; pos; r: var Writer) =
  ## Emits the code for a struct/union member declaration.
  discard advance(m.tast, pos)
  var tpos = pos
  skip(m.tast, pos)

  let align = readInt(m, m.tast, pos)
  let attribs = readSet(m, m.tast, pos, CgLocAttrib)
  let bitsize = readInt(m, m.tast, pos)
  let name = advance(m.tast, pos).val.StringId

  if align > 0:
    r.add "NIM_ALIGN("
    r.addInt align
    r.add ") "

  genDecl(m, tpos, name, attribs, bitsize.int, r)
  r.add ";"

proc structToC(m; pos; name: string, r: var Writer) =
  ## Translates and emits a struct/union definition.
  let n = advance(m.tast, pos)
  let packed = readInt(m, m.tast, pos) == 1
  if n.kind == cnkStructTy:
    r.add "struct "
  else:
    r.add "union "
  if packed:
    r.add "N_PACKED_START "
  if name.len > 0:
    r.add name
    r.add " {"
  else:
    r.add "{"
  inc r.indent
  for i in 1..<len(n):
    r.newLineRaw()
    memberToC(m, pos, r)
  dec r.indent
  r.newLineRaw()
  r.add "}"
  if packed:
    r.add " N_PACKED_END"

proc typeRefToC(m; typ: StringId, r: var Writer)

proc typeToC(m; pos; r: var Writer) =
  ## Emits the body for a type.
  let n = advance(m.tast, pos)
  case n.kind
  of cnkStructTy, cnkUnionTy:
    # an anonymous inline struct/union
    dec pos # go back to the header
    structToC(m, pos, "", r)
  of cnkVoidTy:
    r.add "void"
  of cnkVarargs:
    r.add "..."
  of cnkBoolTy:
    r.add "NIM_BOOL"
  of cnkCharTy:
    r.add "NIM_CHAR"
  of cnkIntTy:
    r.add "NI"
    r.addInt readInt(m, m.tast, pos) * 8
  of cnkUIntTy:
    r.add "NU"
    r.addInt readInt(m, m.tast, pos) * 8
  of cnkFloatTy:
    r.add "NF"
    r.addInt readInt(m, m.tast, pos) * 8
  of cnkType:
    typeRefToC(m, n.val.StringId, r)
  of cnkPtrTy:
    if m.tast[pos].kind == cnkArrayTy:
      pos = m.tast.child(pos, 1)
    typeToC(m, pos, r)
    r.add "*"
  of cnkOpaqueTy:
    r.add m.get(advance(m.tast, pos).val.StringId)
    inc pos # ignore the header name
  else:
    unreachable(n.kind)

proc typeRefToC(m; typ: StringId, r: var Writer) =
  ## Emits the C code for a type reference.
  case m.tast[m.types[typ]].kind
  of PreferIdentified:
    r.add m.get(typ)
  else:
    # inline the type expression
    var pos = m.types[typ]
    typeToC(m, pos, r)

proc typeRefToC(m; pos; r: var Writer) =
  let n = advance(m.ast, pos)
  assert n.kind == cnkType
  typeRefToC(m, n.val.StringId, r)

proc genDecl(m; typ, name: StringId, attribs: set[CgLocAttrib], bitsize: int,
             r: var Writer) =
  ## Convenience wrapper.
  var pos = m.types[typ]
  case m.tast[pos].kind
  of PreferIdentified:
    typeRefToC(m, typ, r)
    r.add " "
    genQualDecl(m, name, attribs, bitsize, r)
  else:
    genDecl(m, pos, name, attribs, bitsize, r)

proc exprToC(m; pos; r: var Writer)

proc binOpToC(m; pos; op: string, r: var Writer) =
  skip(m.ast, pos)
  r.add "("
  exprToC(m, pos, r)
  r.add op
  exprToC(m, pos, r)
  r.add ")"

proc indexToC(m; pos; r: var Writer) =
  ## Emits the C code for an index operand.
  if m.ast[pos].kind == cnkInt:
    r.addInt readInt(m, m.ast, pos)
  else:
    exprToC(m, pos, r)

proc pathToC(m; pos; tn: NodeIndex, count: int, r: var Writer) =
  ## Emits a C access sequence with `count` operands starting at `pos`, for
  ## the type whose description is at `tn`
  const Access = [false: ".", true: "->"]
  var tn = tn
  var deref = false
  if m.tast[tn].kind == cnkPtrTy:
    deref = true
    tn = resolve(m, m.tast.child(tn, 0))

  # the meaning of the index value depends on the corresponding type
  for _ in 0..<count:
    case m.tast[tn].kind
    of cnkStructTy, cnkUnionTy:
      tn = m.tast.child(tn, 1 + readInt(m, m.ast, pos))
      let str = m.get(m.tast[tn, 4].val.StringId)
      # don't add a dot access for anonymous fields
      if str.len != 0:
        r.add Access[deref]
        r.add str
        deref = false
      tn = m.tast.child(tn, 0)
    of cnkOpaqueTy:
      discard advance(m.ast, pos)
      tn = m.types[advance(m.ast, pos).val.StringId]
      r.add Access[deref]
      r.add m.get(advance(m.ast, pos).val.StringId)
      deref = false
    of cnkArrayTy:
      r.add "["
      indexToC(m, pos, r)
      r.add "]"
      tn = m.tast.child(tn, 1)
      deref = false
    else:
      unreachable(m.tast[tn].kind)
    tn = resolve(m, tn)

proc valueToC(m; pos; r: var Writer) =
  ## Emits the C code for a `cnkValue` tree. `pos` is expected to point to
  ## the first child node.
  let typ = advance(m.ast, pos).val.StringId
  let tn = m.types[typ]
  let v = advance(m.ast, pos)
  case m.tast[tn].kind
  of cnkBoolTy:
    if v.val == 0:
      r.add "NIM_FALSE"
    else:
      r.add "NIM_TRUE"
  of cnkCharTy:
    r.add "'"
    r.addChar(cast[char](m.unpackUInt(v.val)))
    r.add "'"
  of cnkIntTy, cnkUIntTy:
    r.add "("
    typeRefToC(m, typ, r)
    r.add ")"
    # always treat as an int. The C cast ensures the value is correct
    r.addInt m.unpackInt(v.val)
  of cnkFloatTy:
    r.add "("
    typeRefToC(m, typ, r)
    r.add ")"
    let f = m.unpackFloat(v.val)
    case classify(f)
    of fcNan:
      if signbit(f):
        r.add "-NAN"
      else:
        r.add "NAN"
    of fcZero:
      r.add "0.0"
    of fcNegZero:
      r.add "-0.0"
    of fcInf:
      r.add "INF"
    of fcNegInf:
      r.add "-INF"
    of fcNormal, fcSubnormal:
      r.output.addFloatRoundtrip(f)
  of cnkPtrTy, cnkArrayTy:
    # can only be a character string
    r.addEscaped m.get(v.val.StringId)
  of cnkOpaqueTy:
    # TODO: remove this case once foreign numeric types are gone
    r.add "("
    typeRefToC(m, typ, r)
    r.add ")"
    if v.kind == cnkInt:
      r.addInt m.unpackInt(v.val)
    else:
      r.addFloat m.unpackFloat(v.val)
  else:
    unreachable()

proc argsToC(m; pos; num: int, r: var Writer) =
  ## Emits the C code for an arugment list with `num` arguments.
  r.add "("
  for i in 0..<num:
    if i > 0:
      r.add ", "
    exprToC(m, pos, r)
  r.add ")"

proc checkedOpToC(m; pos; open: string, r: var Writer) =
  ## Emits the C code for a checked arithmetic operation.
  skip(m.ast, pos)
  skip(m.ast, pos)
  r.add open
  exprToC(m, pos, r)
  r.add ", "
  exprToC(m, pos, r)
  r.add ", "
  exprToC(m, pos, r)
  r.add ")"

proc unOpToC(m; pos; name: string, r: var Writer) =
  ## Emits the C code for an unary operation.
  r.add name
  skip(m.ast, pos)
  exprToC(m, pos, r)

proc cmpToC(m; pos; name: string, r: var Writer) =
  ## Emits the C code for a comparison.
  skip(m.ast, pos)
  binOpToC(m, pos, name, r)

proc exprToC(m; pos; r: var Writer) =
  ## Emits the C code for expressions and symbols.
  let n = advance(m.ast, pos)
  case n.kind
  of cnkGlobal, cnkProc, cnkLocal:
    r.add m.get(n.val.StringId)
  of cnkUse:
    skip(m.ast, pos)
    exprToC(m, pos, r)
  of cnkDatum:
    r.add "_const_"
    r.addInt r.anon[n.val.Datum]
  of cnkValue:
    valueToC(m, pos, r)
  of cnkUnknown:
    # just use the name verbatim
    skip(m.ast, pos)
    r.add m.get(advance(m.ast, pos).val.StringId)
  of cnkNilLit:
    r.add "NIM_NIL"
  of cnkUnlikely:
    r.add "NIM_UNLIKELY("
    exprToC(m, pos, r)
    r.add ")"
  of cnkBitNot: unOpToC(m, pos, "~", r)
  of cnkBitAnd: binOpToC(m, pos, " & ", r)
  of cnkBitOr:  binOpToC(m, pos, " | ", r)
  of cnkBitXor: binOpToC(m, pos, " ^ ", r)
  of cnkShr:    binOpToC(m, pos, " >> ", r)
  of cnkShl:    binOpToC(m, pos, " << ", r)
  of cnkEq:     cmpToC(m, pos, " == ", r)
  of cnkLe:     cmpToC(m, pos, " <= ", r)
  of cnkLt:     cmpToC(m, pos, " < ", r)
  of cnkNot:    unOpToC(m, pos, "!", r)
  of cnkNeg:    unOpToC(m, pos, "-", r)
  of cnkAdd:    binOpToC(m, pos, " + ", r)
  of cnkSub:    binOpToC(m, pos, " - ", r)
  of cnkMul:    binOpToC(m, pos, " * ", r)
  of cnkDiv:    binOpToC(m, pos, " / ", r)
  of cnkMod:    binOpToC(m, pos, " % ", r)
  of cnkCheckedAdd: checkedOpToC(m, pos, "nimAddInt(", r)
  of cnkCheckedSub: checkedOpToC(m, pos, "nimAddSub(", r)
  of cnkCheckedMul: checkedOpToC(m, pos, "nimAddMul(", r)
  of cnkZext:
    let typ = advance(m.ast, pos).val.StringId
    let styp = getType(m, pos)
    r.add "(("
    typeRefToC(m, typ, r)
    r.add ")"
    if m.tast[styp].kind == cnkIntTy:
      # convert the input to an unsigned value first, so that zero
      # extension happens
      r.add "(NU"
      r.addInt m.unpackInt(m.tast[styp, 0].val)
      r.add ")"
    exprToC(m, pos, r)
    r.add ")"
  of cnkSext:
    let typ = advance(m.ast, pos).val.StringId
    let styp = getType(m, pos)
    r.add "(("
    typeRefToC(m, typ, r)
    r.add ")"
    if m.tast[styp].kind == cnkUIntTy:
      # convert the input to a signed value first, so that sign
      # extension happens
      r.add "(NI"
      r.addInt m.unpackInt(m.tast[styp, 0].val)
      r.add ")"
    exprToC(m, pos, r)
    r.add ")"
  of cnkFToI, cnkFToU, cnkIToF, cnkUToF:
    # TODO: make behaviour defined
    r.add "(("
    typeRefToC(m, pos, r)
    r.add ")"
    exprToC(m, pos, r)
    r.add ")"
  of cnkTrunc, cnkPromote, cnkDemote, cnkConv, cnkBitcast, cnkPtrCast:
    r.add "(("
    typeRefToC(m, pos, r)
    r.add ")"
    exprToC(m, pos, r)
    r.add ")"
  of cnkLoad:
    skip(m.ast, pos)
    r.add "(*"
    exprToC(m, pos, r)
    r.add ")"
  of cnkAddr:
    let tn = m.types[advance(m.ast, pos).val.StringId]
    if m.tast[tn, 0].kind == cnkArrayTy and
       m.unpackInt(m.tast[m.tast.child(tn, 0), 0].val) > 0:
      # don't take the address of array lvalues; let them implicitly convert to
      # pointers to their first element
      exprToC(m, pos, r)
    else:
      r.add "(&"
      exprToC(m, pos, r)
      r.add ")"
  of cnkCall:
    exprToC(m, pos, r)
    argsToC(m, pos, len(n) - 1, r)
  of cnkSizeof:
    r.add "("
    typeRefToC(m, pos, r)
    r.add ")sizeof("
    typeRefToC(m, pos, r)
    r.add ")"
  of cnkAlignof:
    r.add "("
    typeRefToC(m, pos, r)
    r.add ")NIM_ALIGNOF("
    typeRefToC(m, pos, r)
    r.add ")"
  of cnkOffsetof:
    r.add "("
    typeRefToC(m, pos, r)
    r.add ")offsetof("
    let typ = m.ast[pos].val.StringId
    typeRefToC(m, pos, r)
    r.add ", "
    let start = r.output.len
    pathToC(m, pos, m.types[typ], len(n) - 2, r)
    # remove the leading dot:
    r.output.delete(start..start)
    r.add ")"
  of cnkPath:
    skip(m.ast, pos)
    let typ = getType(m, pos)
    exprToC(m, pos, r)
    pathToC(m, pos, typ, len(n) - 2, r)
  of AllNodes - cnkExprs -
     {cnkUnknown, cnkUnlikely, cnkDatum, cnkProc, cnkGlobal, cnkLocal}:
    unreachable(n.kind)

proc stmtToC(m; pos; r: var Writer) =
  ## Emits the C code for statements and blocks.
  proc emit(m; pos; len: int, r: var Writer) =
    ## Emits the asm/emit operands without additional formatting.
    for _ in 0..<len:
      case m.ast[pos].kind
      of cnkString:
        # it's a code snippet that's to be used verbatim
        r.add m.get(advance(m.ast, pos).val.StringId)
      of cnkType:
        typeRefToC(m, pos, r)
      else:
        exprToC(m, pos, r)

  let n = advance(m.ast, pos)
  case n.kind
  of cnkStmtList:
    for _ in 0..<len(n):
      stmtToC(m, pos, r)
  of cnkDef:
    r.newLine(m, n.info)
    let align = readUInt(m, m.ast, pos)
    let flags = readSet(m, m.ast, pos, CgLocAttrib)
    if align > 0:
      r.add "NIM_ALIGN("
      r.addInt align
      r.add ") "

    if Register in flags:
      r.add "register " # a specifier, not a qualfiier

    genDecl(m,
      advance(m.ast, pos).val.StringId, # type
      advance(m.ast, pos).val.StringId, # name
      flags, 0, r)
    r.add ";"
  of cnkUnreachable:
    r.newLine(m, n.info)
    r.add "NIM_UNREACHABLE();"
  of cnkDrop:
    r.newLine(m, n.info)
    r.add "(void)"
    exprToC(m, pos, r)
    r.add ";"
  of cnkEmit:
    r.newLine(m, n.info)
    emit(m, pos, len(n), r)
  of cnkAsm:
    r.newLine(m, n.info)
    case AsmMode(readUInt(m, m.ast, pos))
    of asmGnu:
      # GNU inline asm requires each instruction being its own string constant
      let start = r.output.len
      emit(m, pos, len(n) - 1, r)
      let res = r.output[start..^1]
      r.output.setLen(start)
      # split up the result
      r.add "asm("
      inc r.indent
      for line in splitLines(res):
        var i = 0
        while i < line.len and line[i] in {' ', '\t'}:
          inc i

        if i < line.len:
          r.newLineRaw()
          if line[i] in {'"', ':'}:
            # keep clobber lists and quoted lines as-is
            r.add line
          else:
            # turn the line into a C constant, with a newline at the end
            r.add "\""
            for j in i..<line.len:
              r.addChar line[j]
            r.add "\\n\""
      dec r.indent
      r.add ");"
    of asmMsvc:
      r.add "__asm "
      r.startBlock()
      emit(m, pos, len(n) - 1, r)
      r.closeBlock()
    of asmJs:
      unreachable()
  of cnkScope:
    r.newLine(m, n.info)
    r.startBlock()
    stmtToC(m, pos, r)
    r.closeBlock()
  of cnkDispatch:
    r.newLine(m, n.info)
    r.add "switch ("
    exprToC(m, pos, r)
    r.add ") "
    r.startBlock()
    for _ in 1..<len(n):
      let dest = advance(m.ast, pos)
      if len(dest) == 1:
        r.newLine(m, n.info)
        r.add "default:"
      else:
        for _ in 1..<len(dest):
          r.newLine(m, n.info)
          r.add "case "
          exprToC(m, pos, r)
          r.add ":"
      stmtToC(m, pos, r)
      # no 'break' needed, as the statement must end in a terminator
    r.closeBlock()
  of cnkAsgn:
    r.newLine(m, n.info)
    exprToC(m, pos, r)
    r.add " = "
    exprToC(m, pos, r)
    r.add ";"
  of cnkStore:
    r.newLine(m, n.info)
    r.add "*"
    exprToC(m, pos, r)
    r.add " = "
    exprToC(m, pos, r)
    r.add ";"
  of cnkBreak:
    r.newLine(m, n.info)
    r.add "goto L"
    r.addInt advance(m.ast, pos).val
    r.add "_;"
  of cnkBlock:
    let label = advance(m.ast, pos).val
    stmtToC(m, pos, r)
    r.newLine(m, n.info)
    r.add "L"
    r.addInt label
    r.add "_:;"
  of cnkIf:
    r.newLine(m, n.info)
    r.add "if ("
    exprToC(m, pos, r)
    r.add ") "
    r.startBlock()
    stmtToC(m, pos, r)
    r.closeBlock()
    if len(n) == 3:
      r.add " else "
      r.startBlock()
      stmtToC(m, pos, r)
      r.closeBlock()
  of cnkWhile:
    r.newLine(m, n.info)
    r.add "while ("
    exprToC(m, pos, r)
    r.add ")"
    r.startBlock()
    stmtToC(m, pos, r)
    r.closeBlock()
  of cnkReturn:
    r.newLine(m, n.info)
    if len(n) > 0:
      r.add "return "
      exprToC(m, pos, r)
      r.add ";"
    else:
      r.add "return;"
  of cnkCall:
    r.newLine(m, n.info)
    exprToC(m, pos, r)
    argsToC(m, pos, len(n) - 1, r)
    r.add ";"
  of cnkRaise, cnkCheckedCall, cnkCheckedCallAsgn, cnkTry, cnkTailCall:
    r.newLine(m, n.info)
    r.add "NIM_STATIC_ASSERT(0, \"unsupported statement\");"
  of AllNodes - cnkStmts - cnkBlocks:
    unreachable()

proc constrToC(m; pos; r: var Writer) =
  ## Emits the C code for a construction expression.
  let n = advance(m.ast, pos)
  case n.kind
  of cnkProc, cnkGlobal:
    r.add m.get(n.val.StringId)
  of cnkDatum:
    r.add "_const_"
    r.addInt r.anon[n.val.Datum]
  of cnkValue:
    valueToC(m, pos, r)
  of cnkSizeof, cnkAlignof, cnkOffsetof, cnkPtrCast, cnkAddr:
    # `exprToC` already implements these
    dec pos
    exprToC(m, pos, r)
  of cnkNilLit:
    r.add "NIM_NIL"
  of cnkConstr:
    skip(m.ast, pos)
    r.add "{"
    for i in 1..<len(n):
      if i > 1:
        r.add ", "
      constrToC(m, pos, r)
    r.add "}"
  of cnkRecConstr:
    let typ = m.types[advance(m.ast, pos).val.StringId]
    r.add "{"
    for i in 1..<len(n):
      if i > 1:
        r.add ", "
      let init = advance(m.ast, pos)
      pathToC(m, pos, typ, len(init)-1, r)
      r.add " = "
      constrToC(m, pos, r)
    r.add "}"
  else:
    unreachable(n.kind)

proc genProcDecl(m; typ, name: StringId; r: var Writer) =
  ## Emits the C type and function declarator for `typ` and `name`, using
  ## unnamed parameters.
  var pos = m.types[typ]
  let L = len(advance(m.tast, pos)) - 2
  r.add CallingConvToStr[CgCallConv(readUInt(m, m.tast, pos))]
  r.add "("
  typeToC(m, pos, r)
  r.add ", "
  r.add m.get(name)
  r.add ")("
  # the names of parameters don't matter; given them locally unique ones
  for i in 0..<L:
    if i > 0:
      r.add ", "
    if m.tast[pos].kind == cnkVarargs:
      r.add "..."
    else:
      typeToC(m, pos, r)
      r.add " _"
      r.addInt i

  r.add ")"

proc genProcDecl(m; typ, name: StringId, params: NodeIndex; r: var Writer) =
  ## Emits the C type and function declarator for `typ` `name`, and
  ## parameter list `params`.
  var pos = m.types[typ]
  let L = len(advance(m.tast, pos)) - 2
  r.add CallingConvToStr[CgCallConv(readUInt(m, m.tast, pos))]
  r.add "("
  typeToC(m, pos, r)
  r.add ", "
  r.add m.get(name)
  r.add ")("
  var ppos = params
  let numParams = len(advance(m.ast, ppos))
  for i in 0..<numParams:
    if i > 0:
      r.add ", "
    typeToC(m, pos, r)
    r.add " "
    inc ppos # skip the Param node
    let attribs = readSet(m, m.ast, ppos, CgParamAttrib)
    if CgParamAttrib.NoAlias in attribs:
      r.add "NIM_NOALIAS "
    r.add m.get(advance(m.ast, ppos).val.StringId)

  if numParams < L:
    r.add "..."
  r.add ")"

proc globalToC(m; pos; r: var Writer) =
  ## Emits the C type, qualifiers, specifiers, and the declarator - but not
  ## the initializer - for a global.
  let storage = cast[CgStorage](readUInt(m, m.ast, pos))
  if storage == CgStorage.Thread:
    r.add "NIM_THREADVAR "

  let align = readUInt(m, m.ast, pos)
  let flags = readSet(m, m.ast, pos, CgLocAttrib)
  if align > 0:
    r.add "NIM_ALIGN("
    r.addInt align
    r.add ") "

  typeRefToC(m, pos, r)
  r.add " "

  if storage == Const:
    r.add "NIM_CONST "
  if Volatile in flags:
    r.add "volatile "
  if CgLocAttrib.NoAlias in flags:
    r.add "NIM_NOALIAS "

  r.add m.get(advance(m.ast, pos).val.StringId)

type
  Emit* = object
    ## Top-level emit and asm statements to add to the C module.
    includes*: seq[NodeIndex]
    types*: seq[NodeIndex]
    globals*: seq[NodeIndex]
    procs*: seq[NodeIndex]

  ModuleDesc* = object
    ## Describes the shape of a C module, i.e., what entities need to be
    ## declared and defined and in what order.
    headers: seq[StringId]
    dataFwd: seq[Datum]
    data: seq[Datum]
    tdecls: seq[StringId]
    tdefs: seq[StringId]
    gdecls: seq[StringId]
    gdefs: seq[StringId]
    fdecls: seq[tuple[inlined: bool, name: StringId]]
    fdefs: seq[tuple[inlined: bool, name: StringId]]
    emit: Emit

proc initModuleDesc*(m: CgModule, procs, globals: seq[StringId],
                     emit: sink Emit): ModuleDesc =
  ## Creates a module description containing all functions and globals given
  ## by `procs` and `globals`, plus their dependencies.
  var decls, defs, headers: PackedSet[StringId]
  var data: Table[Datum, uint8]
    ## '1' means declared, '2' means defined. A uint8 is used over a bool
    ## due to the former having space for a default value

  # discovery of dependencies makes up the bulk of the work. All identifiers
  # that are going to appear in the C code need to be (at least) *declared*

  proc require(m; name: StringId, weak: bool, res: var ModuleDesc) {.closure.}
  proc requireProc(m; name: StringId, res: var ModuleDesc) {.closure.}
  proc requireGlobal(m; name: StringId, res: var ModuleDesc) {.closure.}
  proc requireDatum(m; d: Datum, res: var ModuleDesc) {.closure.}

  proc inclHeader(m; str: StringId, res: var ModuleDesc) =
    # add the header (if any) to the header list
    if m.get(str).len > 0 and not headers.containsOrIncl(str):
      res.headers.add(str)

  proc scanType(m; pos; weak: bool, res: var ModuleDesc) =
    let n = advance(m.tast, pos)
    case n.kind
    of cnkPtrTy:
      # the pointed-to element doesn't require a definition
      scanType(m, pos, true, res)
    of cnkProcTy:
      for _ in 0..<len(n):
        scanType(m, pos, weak, res)
    of cnkType:
      require(m, n.val.StringId, weak, res)
    of cnkArrayTy:
      skip(m.tast, pos)
      scanType(m, pos, weak, res)
    of cnkStructTy, cnkUnionTy:
      skip(m.tast, pos)
      for _ in 1..<len(n):
        if advance(m.tast, pos).kind == cnkField:
          scanType(m, pos, false, res)
          skip(m.tast, pos)
          skip(m.tast, pos)
          skip(m.tast, pos)
          skip(m.tast, pos)
        else:
          scanType(m, pos, false, res)
          skip(m.tast, pos)
          skip(m.tast, pos)
    of cnkFloatTy, cnkIntTy, cnkUIntTy:
      skip(m.tast, pos)
    of cnkOpaqueTy:
      skip(m.tast, pos)
      inclHeader(m, advance(m.tast, pos).val.StringId, res)
    of cnkVarargs, cnkInt, cnkString, cnkVoidTy, cnkCharTy, cnkBoolTy:
      discard
    else:
      unreachable()

  proc require(m; name: StringId, weak: bool, res: var ModuleDesc) =
    var pos = m.types[name]
    case m.tast[pos].kind
    of cnkStructTy, cnkUnionTy:
      if not decls.containsOrIncl(name):
        res.tdecls.add name
      if not weak and not defs.containsOrIncl(name):
        # also mark as declared, so that no additional declaration is emitted
        scanType(m, pos, false, res)
        res.tdefs.add name
    of cnkArrayTy:
      # an array typedef always needs a defined element type
      scanType(m, pos, false, res)
      if not defs.containsOrIncl(name):
        res.tdefs.add name
    of cnkProcTy:
      if not decls.containsOrIncl(name):
        var pos = m.types[name]
        # function type declarations don't need a full definition of their
        # used types
        scanType(m, pos, true, res)
        res.tdecls.add name
    elif not decls.containsOrIncl(name):
      scanType(m, pos, false, res)

  proc scanParams(m; name: StringId, weak: bool, res: var ModuleDesc) =
    var pos = m.types[name]
    scanType(m, pos, weak, res)

  proc requireGlobal(m; name: StringId, res: var ModuleDesc) =
    if not decls.containsOrIncl(name):
      # keep scanning a little simpler by always pulling in the full type
      # definition, even if not needed by how the global is used
      require(m, m.ast[m.globals[name], 3].val.StringId, false, res)
      res.gdecls.add name

  proc scanBody(m; pos; res: var ModuleDesc) =
    ## Scans a statement/expression for proc, type, etc. dependencies and
    ## registers them.
    const Relevant = {cnkAlignof, cnkSizeof, cnkOffsetof, cnkLoad,
                      cnkProc, cnkGlobal, cnkDatum, cnkDef, cnkConv,
                      cnkPtrCast, cnkPath, cnkUnknown, cnkEmit}
    case m.ast[pos].kind
    of cnkAlignof, cnkSizeof:
      pos = m.ast.child(pos, 1)
      require(m, advance(m.ast, pos).val.StringId, false, res)
    of cnkOffsetof:
      let L = len(m.ast[pos])
      pos = m.ast.child(pos, 1)
      require(m, advance(m.ast, pos).val.StringId, false, res)
      # ignore the rest
      for _ in 2..<L:
        skip(m.ast, pos)
    of cnkLoad:
      # a C deref requires a complete type
      pos = m.ast.child(pos, 0)
      require(m, advance(m.ast, pos).val.StringId, false, res)
      scanBody(m, pos, res)
    of cnkConv, cnkPtrCast:
      # the type operand needs to be available
      pos = m.ast.child(pos, 0)
      require(m, advance(m.ast, pos).val.StringId, false, res)
      scanBody(m, pos, res)
    of cnkProc:
      requireProc(m, advance(m.ast, pos).val.StringId, res)
    of cnkGlobal:
      requireGlobal(m, advance(m.ast, pos).val.StringId, res)
    of cnkDatum:
      requireDatum(m, advance(m.ast, pos).val.Datum, res)
    of cnkDef:
      pos = m.ast.child(pos, 2)
      require(m, advance(m.ast, pos).val.StringId, false, res)
      skip(m.ast, pos)
    of cnkUnknown:
      pos = m.ast.child(pos, 0)
      inclHeader(m, advance(m.ast, pos).val.StringId, res)
      skip(m.ast, pos)
    of cnkPath:
      let n = advance(m.ast, pos)
      skip(m.ast, pos) # skip the result type
      var tn = getType(m, pos)
      # the root may be a pointer, which is automatically dereferenced first,
      # requiring a full definition
      if m.tast[tn].kind == cnkPtrTy:
        tn = m.tast.child(tn, 0)
        scanType(m, tn, false, res)
      for _ in 1..<len(n):
        scanBody(m, pos, res)
    of cnkEmit:
      # types used in emit statements pull in the full definition
      let len = len(advance(m.ast, pos))
      for _ in 0..<len:
        if m.ast[pos].kind == cnkType:
          var pos2 = m.types[advance(m.ast, pos).val.StringId]
          scanType(m, pos2, false, res)
        else:
          scanBody(m, pos, res)
    of AllNodes - Relevant:
      # go over the subtree but only process the relevant parts. This is
      # faster and requires less recursion than manually handling all
      # node kinds
      var last = ord(pos)
      while ord(pos) <= last:
        if m.ast[pos].kind in Relevant:
          let prev = ord(pos)
          scanBody(m, pos, res)
          last += (ord(pos) - prev) - 1
        else:
          if not isLeaf(m.ast[pos]):
            last += len(m.ast[pos])
          inc pos

  proc requireDatum(m; d: Datum, res: var ModuleDesc) =
    case data.getOrDefault(d, 0)
    of 0:
      # not yet seen
      var pos = m.data[d]
      # the full type of the datum is required
      require(m, m.ast[pos, 0].val.StringId, false, res)
      data[d] = 1
      # scan the input first
      scanBody(m, pos, res)
      res.data.add d
      data[d] = 2
    of 1:
      # cyclic dependency; add a forward declaration
      res.dataFwd.add d
      data[d] = 2
    of 2:
      discard "already defined, nothing to do"
    else:
      unreachable()

  proc requireProc(m; name: StringId, res: var ModuleDesc) =
    if not decls.containsOrIncl(name):
      var pos = m.procs[name]
      case m.ast[pos].kind
      of cnkProcDef:
        scanParams(m, m.ast[pos, 1].val.StringId, false, res)
        if Inline in m.toSet(m.ast[pos, 0].val, CgProcAttrib):
          # pull in the definition for inline functions so that the C compiler
          # can do the inlining
          res.fdecls.add (true, name)
          pos = m.ast.last(pos)
          scanBody(m, pos, res)
          res.fdefs.add (true, name)
        else:
          res.fdecls.add (false, name)
      of cnkProcImp, cnkProcExp:
        scanParams(m, m.ast[pos, 1].val.StringId, false, res)
        res.fdecls.add (false, name)
      else:
        unreachable()

  proc scanEmits(m; emits: seq[NodeIndex], res: var ModuleDesc) =
    for it in emits.items:
      var pos = it
      scanBody(m, pos, res)

  # no need to scan the extra include section emits; they cannot refer
  # to anything
  scanEmits(m, emit.types, result)

  # scan the entities in the order the sections they'll be emitted in
  # are arranged

  for it in globals.items:
    var pos = m.globals[it]
    case m.ast[pos].kind
    of cnkGlobalDef, cnkGlobalExp:
      result.gdefs.add it
      require(m, m.ast[pos, 3].val.StringId, false, result)
      if m.ast[pos].len == 6:
        pos = m.ast.child(pos, 5)
        scanBody(m, pos, result)
    of cnkGlobalImp:
      requireGlobal(m, it, result)
    else:
      unreachable()

  scanEmits(m, emit.globals, result)
  scanEmits(m, emit.procs, result)

  for it in procs.items:
    var pos = m.procs[it]
    case m.ast[pos].kind
    of cnkProcDef, cnkProcExp:
      scanParams(m, m.ast[pos, 1].val.StringId, false, result)
      scanBody(m, pos, result)
      result.fdefs.add (false, it)
    of cnkProcImp:
      if not containsOrIncl(decls, it):
        result.fdecls.add (false, it)
        scanParams(m, m.ast[pos, 1].val.StringId, false, result)
    else:
      unreachable()

  result.emit = emit

proc moduleToC*(m: CgModule, desc: ModuleDesc, preamble: string,
                withLineDir: bool): string =
  ## Generates the code for a full C translation unit for `m` and `desc`.
  ## `preamble` is text that's placed at the start of the unit.
  ## `withLineDir` controls whether C line directives are enabled.
  var r = Writer(withLine: withLineDir)
  r.add preamble
  r.add "#include <nimbase.h>"

  for it in desc.headers.items:
    let hdr = m.get(it)
    if hdr[0] == '#':
      # custom include
      r.add "\n"
      r.add hdr.replace('`', '"')
    elif hdr[0] in {'<', '"'}:
      r.add "\n#include "
      r.add hdr
    else:
      # put the header name in quotes
      r.add "\n#include "
      r.add "\""
      r.add hdr
      r.add "\""

  proc emits(m; list: seq[NodeIndex]) =
    for it in list.items:
      var pos = it
      stmtToC(m, pos, r)

  emits(m, desc.emit.includes)

  # undefine various macros that could conflict with identifiers
  r.add "\n"
  r.add """#undef LANGUAGE_C
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
"""

  emits(m, desc.emit.types)

  # emit the type forward declarations:
  for name in desc.tdecls.items:
    r.newLineRaw()
    let pos = m.types[name]
    case m.tast[pos].kind
    of cnkStructTy:
      r.add "typedef struct "
      r.add m.get(name)
      r.add " "
      r.add m.get(name)
      r.add ";"
    of cnkUnionTy:
      r.add "typedef union "
      r.add m.get(name)
      r.add " "
      r.add m.get(name)
      r.add ";"
    of cnkProcTy:
      r.add "typedef "
      genProcDecl(m, name, name, r)
      r.add ";"
    else:
      unreachable(m.tast[pos].kind)

  # emit the type definitions:
  for name in desc.tdefs.items:
    r.newLineRaw()
    var pos = m.types[name]
    case m.tast[pos].kind
    of cnkStructTy, cnkUnionTy:
      structToC(m, pos, m.get(name), r)
      r.add ";"
    of cnkArrayTy:
      r.add "typedef "
      genDecl(m, pos, name, {}, 0, r)
      r.add ";"
    else:
      unreachable(m.tast[pos].kind)

  # emit declarations for functions:
  for (inlined, name) in desc.fdecls.items:
    r.newLineRaw()
    let pos = m.procs[name]
    if inlined:
      r.add "static N_CINLINE "
    elif m.ast[pos].kind == cnkProcDef:
      r.add "N_LIB_PRIVATE "
    genProcDecl(m, m.ast[pos, 1].val.StringId, name, r)
    r.add ";"

  # emit declarations for globals:
  for name in desc.gdecls.items:
    r.newLineRaw()
    var pos = m.ast.child(m.globals[name], 0)
    r.add "extern "
    globalToC(m, pos, r)
    r.add ";"

  # populate the datum suffix table. The idea with the table is to have names
  # that are stable across compilations as long as the module's content
  # doesn't change
  for name in desc.data.items:
    r.anon[name] = uint32(r.anon.len)

  # emit forward declarations for inline constants:
  for name in desc.dataFwd.items:
    r.newLineRaw()
    r.add "static "
    typeRefToC(m, m.ast[m.data[name], 0].val.StringId, r)
    r.add " NIM_CONST _const_"
    r.addInt r.anon[name]
    r.add ";"

  # emit definitions for inline constants:
  for name in desc.data.items:
    r.newLineRaw()
    let it = m.data[name]
    r.add "static "
    typeRefToC(m, m.ast[it, 0].val.StringId, r)
    r.add " NIM_CONST _const_"
    r.addInt r.anon[name]
    r.add " = "
    var pos = it
    constrToC(m, pos, r)
    r.add ";"

  emits(m, desc.emit.globals)

  # emit definitions for globals:
  for name in desc.gdefs.items:
    r.newLineRaw()
    var pos = m.globals[name]
    let n = advance(m.ast, pos)
    if n.kind == cnkGlobalDef:
      r.add "N_LIB_PRIVATE "
    else: # cnkGlobalExp
      r.add "N_LIB_EXPORT_VAR "
    globalToC(m, pos, r)
    if len(n) == 6: # has an initializer?
      r.add " = "
      constrToC(m, pos, r)
    r.add ";"

  emits(m, desc.emit.procs)

  # emit definitions for functions:
  for (inlined, name) in desc.fdefs.items:
    r.newLine(m, 0) # no line information
    var pos = m.procs[name]
    if inlined:
      r.add "static " # only visible within the current C module
    elif m.ast[pos].kind == cnkProcDef:
      # the symbol doesn't need to be visible outside the dynlib (if any)
      r.add "N_LIB_PRIVATE "
    else:
      r.add "N_LIB_EXPORT "

    pos = m.ast.child(pos, 0)
    let attribs = m.readSet(m.ast, pos, CgProcAttrib)
    if NoInline in attribs:
      r.add "N_CNOINLINE "
    elif Inline in attribs:
      r.add "N_CINLINE "

    let typ = advance(m.ast, pos).val.StringId
    skip(m.ast, pos) # skip the name
    genProcDecl(m, typ, name, pos, r)
    skip(m.ast, pos) # skip the params
    r.startBlock()
    stmtToC(m, pos, r)
    r.closeBlock()

  result = r.output
