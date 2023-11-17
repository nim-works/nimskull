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
    ast_types,
    renderer,
    typesrenderer,
  ],
  compiler/mir/[
    mirtrees
  ]

func `$`(n: MirNode): string =
  result.add substr($n.kind, 3) # cut off the prefix
  case n.kind
  of mnkProc, mnkConst, mnkGlobal, mnkParam, mnkLocal:
    result.add " sym: "
    result.add $n.sym.name.s
  of mnkField, mnkPathNamed, mnkPathVariant:
    result.add " field:"
    result.add $n.field.name.s
  of mnkLiteral:
    result.add " lit: "
    {.cast(noSideEffect).}:
      result.add renderTree(n.lit)
  of mnkTemp, mnkAlias:
    result.add " temp: "
    result.add $ord(n.temp)
  of mnkPathPos:
    result.add " position: "
    result.add $n.position
  of mnkCall:
    result.add " effects: "
    result.add $n.effects
  of mnkMagic:
    result.add " magic: "
    result.add $n.magic
  of mnkBlock, mnkBreak:
    result.add " block: "
    result.add $ord(n.label)
  of mnkEnd:
    result.add " start: "
    result.add $n.start
  of mnkPNode:
    result.add " node: "
    result.add $n.node.kind
  of mnkTag:
    result.add " effect: "
    result.add $n.effect
  else:
    result.add " len: "
    result.add $n.len

  if n.typ != nil:
    result.add " typ: "
    result.add $n.typ.kind

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
            line repeat("  ", indent+1), "loose end: ", nodes[i].start
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

func next(tree: MirTree, i: var int): lent MirNode =
  result = tree[i]
  inc i

proc singleToStr(n: MirNode, result: var string) =
  case n.kind
  of SymbolLike:
    result.add n.sym.name.s
  of mnkTemp, mnkAlias:
    result.add "_" & $n.temp.int
  of mnkNone:
    result.add "<none>"
  of mnkLiteral:
    result.add $n.lit
  of mnkType:
    result.add "type("
    result.add $n.typ
    result.add ")"
  of AllNodeKinds - Atoms:
    result.add "<error: " & $n.kind & ">"

proc singleToStr(tree: MirTree, i: var int, result: var string) =
  singleToStr(next(tree, i), result)

proc argToStr(tree: MirTree, i: var int, result: var string) =
  var n {.cursor.} = next(tree, i)
  case n.kind
  of mnkArg:     result.add "arg "
  of mnkName:    result.add "name "
  of mnkConsume: result.add "consume "
  of AllNodeKinds - ArgumentNodes:
    result.add "<error: " & $n.kind & ">"

  n = next(tree, i)
  if n.kind == mnkTag:
    singleToStr(tree, i, result)
    inc i # skip the tag's end node
  else:
    # must be an atom node
    singleToStr(n, result)

  inc i # skip the end node

proc valueToStr(nodes: MirTree, i: var int, result: var string) =
  template tree(start: string, body: untyped) =
    result.add start
    body
    inc i # the end node

  let n {.cursor.} = next(nodes, i)
  case n.kind
  of mnkPathArray:
    tree "":
      valueToStr(nodes, i, result)
      result.add "["
      singleToStr(nodes, i, result)
      result.add "]"
  of mnkPathPos:
    tree "":
      valueToStr(nodes, i, result)
      result.add "."
      result.addInt n.position
  of mnkPathNamed, mnkPathVariant:
    tree "":
      valueToStr(nodes, i, result)
      result.add "."
      result.add n.field.name.s
  of mnkPathConv:
    tree "":
      valueToStr(nodes, i, result)
      result.add ".("
      result.add typeToString(n.typ)
      result.add ")"
  of mnkDeref, mnkDerefView:
    tree "":
      singleToStr(nodes, i, result)
      result.add "[]"
  of AtomNodes:
    singleToStr(n, result)
  else:
    result.add "<error: " & $n.kind & ">"

proc exprToStr(nodes: MirTree, i: var int, result: var string) =
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
    valueToStr(nodes, i, result)
  of mnkAddr:
    tree "addr ":
      valueToStr(nodes, i, result)
  of mnkView:
    tree "borrow ":
      valueToStr(nodes, i, result)
  of mnkToSlice:
    tree "toOpenArray ":
      valueToStr(nodes, i, result)
  of mnkConv:
    tree "conv ":
      valueToStr(nodes, i, result)
  of mnkCast:
    tree "cast ":
      valueToStr(nodes, i, result)
  of mnkStdConv:
    tree "stdConv ":
      valueToStr(nodes, i, result)
  of mnkConstr:
    tree "construct (":
      commaSeparated:
        argToStr(nodes, i, result)
      result.add ")"
  of mnkObjConstr:
    tree "(":
      commaSeparated:
        result.add next(nodes, i).field.name.s & ": "
        argToStr(nodes, i, result)
      result.add ")"
  of mnkCall:
    tree "":
      singleToStr(nodes, i, result) # callee
      result.add "("
      commaSeparated:
        argToStr(nodes, i, result)
      result.add ")"
  of mnkMagic:
    # cut off the 'm' prefix and use a lowercase first character
    var name = substr($nodes[i].magic, 1)
    name[0] = toLowerAscii(name[0])
    tree name:
      result.add "("
      commaSeparated:
        argToStr(nodes, i, result)
      result.add ")"
  else:
    # TODO: make this branch exhaustive
    result.add "<error: " & $nodes[i].kind & ">"
    inc i

proc renderNameWithType(tree: MirTree, i: var int, result: var string) =
  let n {.cursor.} = next(tree, i)
  singleToStr(n, result)
  result.add ": "
  result.add typeToString(n.typ)

proc renderList(tree: MirTree, i: var int, indent: int, result: var string)

proc stmtToStr(nodes: MirTree, i: var int, indent: int, result: var string) =
  template tree(str: string, body: untyped) =
    result.add repeat("  ", indent)
    result.add str
    body

  var indent = indent
  template tab(body: untyped) =
    ## Runs `body` with the indentation increased by 1.
    inc indent
    body
    dec indent

  let n {.cursor.} = next(nodes, i)
  case n.kind
  of mnkDef, mnkDefUnpack:
    tree "def ":
      renderNameWithType(nodes, i, result)
      if nodes[i].kind != mnkNone:
        result.add " = "
        exprToStr(nodes, i, result)
      else:
        inc i # skip the node
    result.add "\n"
  of mnkDefCursor:
    tree "def_cursor ":
      renderNameWithType(nodes, i, result)
      result.add " = "
      exprToStr(nodes, i, result)
    result.add "\n"
  of mnkBind:
    tree "bind ":
      renderNameWithType(nodes, i, result)
      result.add " = "
      valueToStr(nodes, i, result)
    result.add "\n"
  of mnkBindMut:
    tree "bind_mut ":
      renderNameWithType(nodes, i, result)
      result.add " = "
      valueToStr(nodes, i, result)
    result.add "\n"
  of mnkAsgn, mnkSwitch:
    tree "":
      valueToStr(nodes, i, result)
      result.add " = "
      exprToStr(nodes, i, result)
    result.add "\n"
  of mnkInit:
    tree "":
      valueToStr(nodes, i, result)
      result.add " := "
      exprToStr(nodes, i, result)
    result.add "\n"
  of mnkFastAsgn:
    tree "":
      valueToStr(nodes, i, result)
      result.add " =fast "
      exprToStr(nodes, i, result)
    result.add "\n"
  of mnkStmtList:
    renderList(nodes, i, indent, result)
  of mnkTry:
    tree "try:\n":
      tab:
        stmtToStr(nodes, i, indent, result)
      renderList(nodes, i, indent, result)
  of mnkExcept:
    tree "except\n":
      renderList(nodes, i, indent, result)
  of mnkFinally:
    tree "finally:\n":
      tab:
        stmtToStr(nodes, i, indent, result)
  of mnkScope:
    tree "scope:\n":
      tab:
        renderList(nodes, i, indent, result)
  of mnkIf:
    tree "if ":
      valueToStr(nodes, i, result)
      result.add ":\n"
      tab:
        renderList(nodes, i, indent, result)
  of mnkCase:
    tree "case ":
      valueToStr(nodes, i, result)
      result.add "\n"
      # use ``renderList`` for simplicity, even though it allows for
      # structures that are invalid
      renderList(nodes, i, indent, result)
  of mnkBranch:
    tree "of ":
      for j in 0..<n.len:
        if j > 0:
          result.add ", "
        singleToStr(nodes, i, result)
      result.add ":\n"
      tab:
        renderList(nodes, i, indent, result)
  of mnkBlock:
    tree "block L" & $n.label.int & ":\n":
      tab:
        renderList(nodes, i, indent, result)
  of mnkRepeat:
    tree "while true:\n":
      tab:
        renderList(nodes, i, indent, result)
  of mnkAsm, mnkEmit:
    tree (if n.kind == mnkAsm: "asm " else: "emit "):
      var first = true
      while nodes[i].kind != mnkEnd:
        if first:
          first = false
        else:
          result.add ", "
        valueToStr(nodes, i, result)
      result.add "\n"
  of mnkVoid:
    # drop the 'discard' prefix if the expression is a void expression
    let lead =
      if nodes[i].typ.kind == tyVoid: ""
      else: "discard "
    tree lead:
      exprToStr(nodes, i, result)
    result.add "\n"
  of mnkRaise:
    tree "raise ":
      valueToStr(nodes, i, result)
    result.add "\n"
  of mnkPNode:
    result.add repeat("  ", indent)
    result.add "PNode " & $n.node & "\n"
  of mnkBreak:
    result.add repeat("  ", indent)
    result.add "break L" & $n.label.int & "\n"
  of mnkReturn:
    result.add repeat("  ", indent)
    result.add "return\n"
  of AllNodeKinds - StmtNodes - {mnkBranch, mnkExcept, mnkFinally}:
    result.add "<error: " & $n.kind & ">\n"

  # skip the end node
  i += ord(n.kind in SubTreeNodes)

proc renderList(tree: MirTree, i: var int, indent: int, result: var string) =
  while i < tree.len and tree[i].kind != mnkEnd:
    stmtToStr(tree, i, indent, result)

proc exprToStr*(tree: MirTree, n: NodePosition): string =
  ## Renders the expression at `n` into a human-readable text representation.
  var i = n.int
  exprToStr(tree, i, result)

proc stmtToStr*(tree: MirTree, n: NodePosition): string =
  ## Renders the statement at `n` into a human-readable text representation.
  var i = n.int
  stmtToStr(tree, i, 0, result)

proc render*(tree: MirTree): string =
  ## Renders `tree` into a human-readable text representation. The output is
  ## meant for debugging and tracing and is not guaranteed to have a stable
  ## format.
  var i = 0
  renderList(tree, i, 0, result)