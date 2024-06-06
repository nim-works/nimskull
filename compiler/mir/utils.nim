## Non-essential utilities for interacting with or inspecting MIR code. No
## code should require any routines or types defined here in order to function.
##
## For maximum availability, the imports of compiler modules should be kept as
## low as possible, so that the module can be imported without causing
## circular dependencies.

import
  std/[
    strutils
  ],
  compiler/ast/[
    ast_query,
    ast_types,
    renderer,
    types,
    typesrenderer,
  ],
  compiler/mir/[
    mirbodies,
    mirenv,
    mirtrees,
    mirtypes
  ]

func `$`(n: MirNode): string =
  result.add substr($n.kind, 3) # cut off the prefix
  case n.kind
  of mnkProc, mnkProcVal:
    result.add " prc: "
    result.addInt n.prc.uint32
  of mnkConst:
    result.add " cnst: "
    result.addInt n.cnst.uint32
  of mnkGlobal:
    result.add " global: "
    result.addInt n.global.uint32
  of mnkParam, mnkLocal, mnkTemp, mnkAlias:
    result.add " local: "
    result.addInt n.local.uint32
  of mnkField:
    result.add " field:"
    result.addInt n.field
  of mnkIntLit, mnkUIntLit, mnkFloatLit:
    result.add " number: "
    result.addInt n.number.uint32
  of mnkStrLit:
    result.add " strVal: "
    result.addInt n.strVal.uint32
  of mnkAstLit:
    result.add " ast: "
    result.addInt n.ast.uint32
  of mnkMagic:
    result.add " magic: "
    result.add $n.magic
  of mnkLabel, mnkLeave:
    result.add " label: "
    result.addInt n.label.uint32
  of mnkNone, mnkNilLit, mnkType, mnkResume:
    discard
  of SubTreeNodes:
    result.add " len: "
    result.addInt n.len

  if n.typ != VoidType:
    result.add " typ: "
    result.addInt n.typ.uint32

proc treeRepr*(tree: MirTree, pos = NodePosition(0)): string =
  ## Renders the node or sub-tree at `pos` to a string in a tree-layout-
  ## centric representation. This is meant for debugging purposes, with
  ## erroneous trees being handled gracefully.
  func appendLine(s: var string, items: varargs[string, `$`]) =
    for x in items:
      s.add x
    s.add "\n"

  proc aux(result: var string, nodes: MirNodeSeq, indent, num: int,
           i: var int) =
    template line(items: varargs[untyped]) =
      result.appendLine(items)

    let n {.cursor.} = nodes[i]
    inc i # move the node cursor forward
    line repeat("  ", indent), num, ": ", n

    case n.kind
    of SubTreeNodes:
      var sub = 0
      while true:
        if i >= nodes.len:
          line repeat("  ", indent), "out of bounds: end expected for ",
               n.kind
          break
        elif nodes[i].kind == mnkEnd:
          if nodes[i].start == n.kind:
            inc i
            break
          else:
            line repeat("  ", indent+1), "loose end: ", nodes[i].start,
                 " expected: ", n.kind
            inc i
        else:
          aux(result, nodes, indent+1, sub, i)

        inc sub

    of AtomNodes + {mnkEnd}:
      discard "already rendered"

  var i = pos.int
  # do nothing if there are no nodes or if `pos` points outside the sequence
  if i < tree.len:
    aux(result, tree, 0, 0, i)

# ------- MIR pretty printer --------

type
  RenderCtx = object
    ## Contextual immutable data for the renderer.
    env: ptr MirEnv   ## may be nil
    body: ptr MirBody ## may be nil

template treeParam(): untyped =
  ## Expands to ``nodes`` or ``tree``, depending on how the parameter is
  ## named.
  mixin nodes, tree
  when declared(nodes):
    when nodes is MirTree: nodes
    else:                  tree
  else:                    tree

func next(tree: MirTree, i: var int): lent MirNode =
  result = tree[i]
  inc i

proc error(result: var string, n: MirNode) =
  result.add "<unexpected: " & $n.kind & ">"

func idToStr[I](result: var string, id: I, open: string) =
  result.add open
  result.addInt id.uint32
  result.add ">"

func addName[I](result: var string, id: I, open: string, c: RenderCtx) =
  if c.env.isNil:
    # just render the ID
    idToStr(result, id, open)
  else:
    result.add c.env[][id].name.s

proc typeToStr(result: var string, t: TypeId, env: ptr MirEnv) =
  if env.isNil:
    result.add "<T"
    result.addInt t.uint32
    result.add ">"
  else:
    result.add typeToString(env.types[t])

func addLocalName(result: var string, id: LocalId, open: string,
                  c: RenderCtx) =
  if c.body.isNil:
    # render just the ID
    idToStr(result, id, open)
  else:
    result.add c.body[][id].name.s

proc addTypedNumber(result: var string, bits: BiggestInt, typ: PType) =
  ## Interprets the bit representation `bits` as `typ` and renders it
  ## accordingly. Errors are output directly into `result`.
  let typ = typ.skipTypes(abstractRange)
  case typ.kind
  of tyInt..tyInt64:
    result.addInt bits
    result.add [tyInt: "", "'i8", "'i16", "'i32", "'i64"][typ.kind]
  of tyUInt..tyUInt64:
    result.addInt cast[BiggestUInt](bits)
    result.add [tyUInt: "", "'u8", "'u16", "'u32", "'u64"][typ.kind]
  of tyFloat:
    result.addFloat cast[BiggestFloat](bits)
    result.add [tyFloat: "", "'f32", "'f64"][typ.kind]
  of tyEnum, tyBool:
    # use the name of the enum field
    block render:
      # search for the enum field with the given value
      for it in typ.n.items:
        if it.sym.position == bits:
          # found it!
          result.add it.sym.name.s
          break render
      result.add "<invalid enum>"
  of tyProc, tyPtr, tyPointer:
    result.addInt cast[BiggestUInt](bits)
  else:
    result.add "<invalid literal>"

func add(result: var string, id: LabelId) =
  result.add 'L'
  result.addInt id.uint32

proc singleToStr(n: MirNode, result: var string, c: RenderCtx) =
  case n.kind
  of mnkParam:
    result.addLocalName(n.local, "<Param", c)
  of mnkLocal:
    result.addLocalName(n.local, "<L", c)
  of mnkConst:
    if isAnon(n.cnst):
      idToStr(result, extract(n.cnst), "<D") # "D" for "Data"
    else:
      result.addName(n.cnst, "<C", c)
  of mnkGlobal:
    result.addName(n.global, "<G", c)
  of mnkProc, mnkProcVal:
    # procedure references are also handled here for simplicity
    result.addName(n.prc, "<P", c)
  of mnkTemp, mnkAlias:
    result.add "_" & $n.local.int
  of mnkNone:
    result.add "<none>"
  of mnkNilLit:
    result.add "nil"
  of mnkIntLit:
    if c.env.isNil:
      idToStr(result, n.number, "<Int: ")
    else:
      result.addTypedNumber(c.env[].getInt(n.number), c.env[][n.typ])
  of mnkUIntLit:
    if c.env.isNil:
      idToStr(result, n.number, "<UInt: ")
    else:
      result.addTypedNumber(c.env[].getInt(n.number), c.env[][n.typ])
  of mnkFloatLit:
    if c.env.isNil:
      idToStr(result, n.number, "<Float: ")
    else:
      result.addTypedNumber(c.env[].getInt(n.number), c.env[][n.typ])
  of mnkStrLit:
    if c.env.isNil:
      result.add "<Str: "
      result.addInt n.strVal.uint32
      result.add ">"
    else:
      result.addQuoted c.env[][n.strVal]
  of mnkAstLit:
    # could also be pretty-printed, but, given the sparse usage, doesn't
    # warrant the extra effort at the moment
    result.add "<Ast>"
  of mnkType:
    result.add "type("
    typeToStr(result, n.typ, c.env)
    result.add ")"
  of AllNodeKinds - Atoms - mnkProc + {mnkResume, mnkLeave}:
    result.error(n)

proc singleToStr(tree: MirTree, i: var int, result: var string, c: RenderCtx) =
  singleToStr(next(tree, i), result, c)

template singleToStr() =
  singleToStr(treeParam(), i, result, c)

proc fieldToStr(field: int32, typ: TypeId, result: var string, c: RenderCtx) =
  if c.env.isNil:
    result.add "Field"
    result.addInt field
  else:
    result.add lookupInType(c.env[][typ], field.int).name.s

template valueToStr() =
  mixin valueToStr
  valueToStr(treeParam(), i, result, c)

proc valueToStr(nodes: MirTree, i: var int, result: var string, c: RenderCtx) =
  template tree(start: string, body: untyped) =
    result.add start
    body
    inc i # the end node

  let n {.cursor.} = next(nodes, i)
  case n.kind
  of mnkPathArray:
    tree "":
      valueToStr()
      result.add "["
      singleToStr()
      result.add "]"
  of mnkPathPos:
    tree "":
      valueToStr()
      result.add "."
      result.addInt n.position
  of mnkPathNamed, mnkPathVariant:
    tree "":
      let typ = nodes[i].typ # type of the object operand
      valueToStr()
      result.add "."
      fieldToStr(n.field, typ, result, c)
  of mnkPathConv:
    tree "":
      valueToStr()
      result.add ".("
      typeToStr(result, n.typ, c.env)
      result.add ")"
  of mnkDeref, mnkDerefView:
    tree "":
      singleToStr()
      result.add "[]"
  of AtomNodes:
    singleToStr(n, result, c)
  else:
    result.error(n)

proc calleeToStr(tree: MirTree, i: var int, result: var string, c: RenderCtx) =
  case tree[i].kind
  of mnkMagic:
    # cut off the 'm' prefix and use a lowercase first character
    var name = substr($tree[i].magic, 1)
    name[0] = toLowerAscii(name[0])
    result.add name
    inc i
  else:
    valueToStr()

proc argToStr(tree: MirTree, i: var int, result: var string, c: RenderCtx) =
  var n {.cursor.} = next(tree, i)
  case n.kind
  of mnkArg:     result.add "arg "
  of mnkName:    result.add "name "
  of mnkConsume: result.add "consume "
  of AllNodeKinds - ArgumentNodes:
    result.error(n)

  if tree[i].kind == mnkTag:
    discard next(tree, i)
    valueToStr()
    inc i # skip the tag's end node
  else:
    valueToStr()

  inc i # skip the end node

template argToStr() =
  argToStr(treeParam(), i, result, c)

proc labelToStr(nodes: MirTree, i: var int, result: var string) =
  let n {.cursor.} = next(nodes, i)
  case n.kind
  of mnkLabel:
    result.add n.label
  else:
    error(result, n)

proc targetToStr(nodes: MirTree, i: var int, result: var string) =
  var n {.cursor.} = next(nodes, i)
  case n.kind
  of mnkLabel:
    result.add n.label
  of mnkTargetList:
    result.add "["
    let start = i
    while (n = next(nodes, i); n.kind != mnkEnd):
      if i > start + 1:
        result.add ", "

      case n.kind
      of mnkLabel:  result.add n.label
      of mnkLeave:  result.add "Leave(L" & $n.label.int & ")"
      of mnkResume: result.add "Resume"
      else:         result.error(n)

    result.add "]"
  else:
    result.error(n)

template targetToStr() =
  targetToStr(nodes, i, result)

proc exprToStr(nodes: MirTree, i: var int, result: var string, c: RenderCtx) =
  template tree(start: string, body: untyped) =
    result.add start
    inc i # skip the start node
    body
    inc i # skip the end node

  template commaSeparated(body: untyped) =
    var first = true
    while nodes[i].kind != mnkEnd:
      if first:
        first = false
      else:
        result.add ", "
      body

  case nodes[i].kind
  of LvalueExprKinds + Atoms:
    valueToStr()
  of mnkAddr:
    tree "addr ":
      valueToStr()
  of mnkView, mnkMutView:
    tree "borrow ":
      valueToStr()
  of mnkToSlice, mnkToMutSlice:
    tree "toOpenArray ":
      commaSeparated:
        valueToStr()
  of mnkConv:
    tree "conv ":
      valueToStr()
  of mnkCast:
    tree "cast ":
      valueToStr()
  of mnkStdConv:
    tree "stdConv ":
      valueToStr()
  of mnkArrayConstr:
    tree "[":
      commaSeparated:
        argToStr()
      result.add "]"
  of mnkSeqConstr:
    tree "@[":
      commaSeparated:
        argToStr()
      result.add "]"
  of mnkTupleConstr:
    tree "(":
      commaSeparated:
        argToStr()
      result.add ")"
  of mnkClosureConstr:
    tree "closure (":
      commaSeparated:
        argToStr()
      result.add ")"
  of mnkSetConstr:
    tree "{":
      commaSeparated:
        exprToStr(nodes, i, result, c)
      result.add "}"
  of mnkRange:
    tree "":
      valueToStr()
      result.add " .. "
      valueToStr()
  of mnkObjConstr, mnkRefConstr:
    let typ = nodes[i].typ
    tree "(":
      commaSeparated:
        tree "":
          fieldToStr(next(nodes, i).field, typ, result, c)
          result.add ": "
          argToStr()
      result.add ")"
  of mnkCall:
    tree "":
      calleeToStr(nodes, i, result, c)
      result.add "("
      commaSeparated:
        argToStr()
      result.add ")"
  of mnkCheckedCall:
    tree "":
      calleeToStr(nodes, i, result, c)
      result.add "("
      # arguments:
      let first = i
      while nodes[i].kind in ArgumentNodes:
        if i > first:
          result.add ", "
        argToStr()

      # jump target:
      result.add ") -> "
      targetToStr()
  of UnaryOps:
    const Map = [mnkNeg: "-"]
    let kind = nodes[i].kind
    tree Map[kind]:
      valueToStr()
  of BinaryOps:
    let kind = nodes[i].kind
    tree "":
      valueToStr() # first operand
      const Map = [mnkAdd: " + ", mnkSub: " - ",
                   mnkMul: " * ", mnkDiv: " div ", mnkModI: " mod "]
      result.add Map[kind]
      valueToStr() # second operand
  of mnkCopy:
    tree "copy ":
      valueToStr()
  of mnkMove:
    tree "move ":
      valueToStr()
  of mnkSink:
    tree "sink ":
      valueToStr()
  else:
    # TODO: make this branch exhaustive
    result.error(nodes[i])
    inc i

template exprToStr() =
  exprToStr(nodes, i, result, c)

proc renderNameWithType(tree: MirTree, i: var int, result: var string,
                        c: RenderCtx) =
  let n {.cursor.} = next(tree, i)
  singleToStr(n, result, c)
  result.add ": "
  typeToStr(result, n.typ, c.env)

proc stmtToStr(nodes: MirTree, i: var int, indent: var int, result: var string,
               c: RenderCtx) =
  template tree(str: string, body: untyped) =
    result.add repeat("  ", indent)
    result.add str
    body

  let n {.cursor.} = next(nodes, i)
  case n.kind
  of mnkDef:
    tree "def ":
      renderNameWithType(nodes, i, result, c)
      if nodes[i].kind != mnkNone:
        result.add " = "
        exprToStr()
      else:
        inc i # skip the node
    result.add "\n"
  of mnkDefCursor:
    tree "def_cursor ":
      renderNameWithType(nodes, i, result, c)
      result.add " = "
      exprToStr()
    result.add "\n"
  of mnkBind:
    tree "bind ":
      renderNameWithType(nodes, i, result, c)
      result.add " = "
      valueToStr()
    result.add "\n"
  of mnkBindMut:
    tree "bind_mut ":
      renderNameWithType(nodes, i, result, c)
      result.add " = "
      valueToStr()
    result.add "\n"
  of mnkAsgn, mnkSwitch:
    tree "":
      valueToStr()
      result.add " = "
      exprToStr()
    result.add "\n"
  of mnkInit:
    tree "":
      valueToStr()
      result.add " := "
      exprToStr()
    result.add "\n"
  of mnkExcept:
    tree "except (":
      labelToStr(nodes, i, result)
      result.add ")"
      # render the filter types:
      for j in 1..<n.len-1:
        if j == 1:
          result.add " "
        else:
          result.add ", "
        singleToStr()
      # render the next handler target:
      if n.len > 1:
        result.add " else "
        targetToStr()
      result.add ":\n"

    inc indent
  of mnkFinally:
    tree "finally (":
      labelToStr(nodes, i, result)
      result.add "):\n"

    inc indent
  of mnkScope:
    tree "scope:\n": discard
    inc indent
  of mnkEndScope:
    dec indent # just dedent
  of mnkIf:
    tree "if ":
      valueToStr()
      inc i # ignore the label
      result.add ":\n"

    inc indent
  of mnkCase:
    tree "case ":
      valueToStr()
      result.add "\n"
      # render the branches:
      for _ in 1..<n.len:
        let b {.cursor.} = next(nodes, i)
        case b.kind
        of mnkBranch:
          tree "of ":
            # render the values:
            for j in 0..<b.len-1:
              if j > 0:
                result.add ", "
              singleToStr()
            # render the jump destination:
            result.add ": goto "
            labelToStr(nodes, i, result)
            result.add "\n"
          inc i # skip the end node
        else:
          # make no attempt at error correction
          result.error(b)

  of mnkAsm, mnkEmit:
    tree (if n.kind == mnkAsm: "asm " else: "emit "):
      var first = true
      while nodes[i].kind != mnkEnd:
        if first:
          first = false
        else:
          result.add ", "
        valueToStr()
      result.add "\n"
  of mnkVoid:
    # drop the 'discard' prefix if the expression is a void expression
    let lead =
      if nodes[i].typ == VoidType: ""
      else: "discard "
    tree lead:
      exprToStr()
    result.add "\n"
  of mnkRaise:
    tree "raise ":
      valueToStr()
      result.add " -> "
      targetToStr()
    result.add "\n"
  of mnkDestroy:
    tree "destroy ":
      valueToStr()
      result.add "\n"
  of mnkGoto:
    tree "goto ":
      targetToStr()
      result.add "\n"
  of mnkLoopJoin:
    tree "while true:\n":
      inc i # skip the label node
    inc indent
  of mnkLoop, mnkEndStruct:
    inc i # skip the label node
    dec indent
  of mnkJoin:
    tree "":
      labelToStr(nodes, i, result)
      result.add ":\n"
  of mnkContinue:
    tree "continue ":
      inc i # skip the label
      result.add "{"
      for j in 1..<n.len:
        if j > 1:
          result.add ", "
        labelToStr(nodes, i, result)
      result.add "}\n"

    dec indent
  of AllNodeKinds - StmtNodes:
    result.error(n)

  # skip the end node
  i += ord(n.kind in SubTreeNodes)

proc renderList(tree: MirTree, i: var int, indent: int, result: var string,
                c: RenderCtx) =
  var indent = indent # support mutation
  while i < tree.len and tree[i].kind != mnkEnd:
    stmtToStr(tree, i, indent, result, c)

proc exprToStr*(tree: MirTree, n: NodePosition; env: ptr MirEnv = nil;
                body: ptr MirBody = nil): string =
  ## Renders the expression at `n` into a human-readable text representation.
  var i = n.int
  exprToStr(tree, i, result, RenderCtx(env: env, body: body))

proc stmtToStr*(tree: MirTree, n: NodePosition; env: ptr MirEnv = nil;
                body: ptr MirBody = nil): string =
  ## Renders the statement at `n` into a human-readable text representation.
  var
    i = n.int
    indent = 0
  stmtToStr(tree, i, indent, result, RenderCtx(env: env, body: body))

proc render*(tree: MirTree; env: ptr MirEnv = nil;
             body: ptr MirBody = nil): string =
  ## Renders `tree` into a human-readable text representation. The output is
  ## meant for debugging and tracing and is not guaranteed to have a stable
  ## format.
  var
    i = 0
    indent = 0
  renderList(tree, i, indent, result, RenderCtx(env: env, body: body))
