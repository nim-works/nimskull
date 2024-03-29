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
    mirbodies,
    mirenv,
    mirtrees
  ]

func `$`(n: MirNode): string =
  result.add substr($n.kind, 3) # cut off the prefix
  case n.kind
  of mnkProc:
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
  of mnkField, mnkPathNamed, mnkPathVariant:
    result.add " field:"
    result.add $n.field.name.s
  of mnkLiteral:
    result.add " lit: "
    {.cast(noSideEffect).}:
      result.add renderTree(n.lit)
  of mnkPathPos:
    result.add " position: "
    result.add $n.position
  of mnkCall, mnkCheckedCall:
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

func addName[I](result: var string, id: I, open: string, c: RenderCtx) =
  if c.env.isNil:
    # just render the ID
    result.add open
    result.addInt id.uint32
    result.add ">"
  else:
    result.add c.env[][id].name.s

func addLocalName(result: var string, id: LocalId, open: string,
                  c: RenderCtx) =
  if c.body.isNil:
    # render just the ID
    result.add open
    result.addInt id.uint32
    result.add ">"
  else:
    result.add c.body[][id].name.s

proc singleToStr(n: MirNode, result: var string, c: RenderCtx) =
  case n.kind
  of mnkParam:
    result.addLocalName(n.local, "<Param", c)
  of mnkLocal:
    result.addLocalName(n.local, "<L", c)
  of mnkConst:
    if isAnon(n.cnst):
      result.add "<D" # "D" for "Data"
      result.addInt extract(n.cnst).uint32
      result.add ">"
    else:
      result.addName(n.cnst, "<C", c)
  of mnkGlobal:
    result.addName(n.global, "<G", c)
  of mnkProc:
    result.addName(n.prc, "<P", c)
  of mnkTemp, mnkAlias:
    result.add "_" & $n.local.int
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

proc singleToStr(tree: MirTree, i: var int, result: var string, c: RenderCtx) =
  singleToStr(next(tree, i), result, c)

template singleToStr() =
  singleToStr(treeParam(), i, result, c)

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
      valueToStr()
      result.add "."
      result.add n.field.name.s
  of mnkPathConv:
    tree "":
      valueToStr()
      result.add ".("
      result.add typeToString(n.typ)
      result.add ")"
  of mnkDeref, mnkDerefView:
    tree "":
      singleToStr()
      result.add "[]"
  of AtomNodes:
    singleToStr(n, result, c)
  else:
    result.add "<error: " & $n.kind & ">"

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
    result.add "<error: " & $n.kind & ">"

  if tree[i].kind == mnkTag:
    discard next(tree, i)
    valueToStr()
    inc i # skip the tag's end node
  else:
    valueToStr()

  inc i # skip the end node

template argToStr() =
  argToStr(treeParam(), i, result, c)

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
  of mnkView:
    tree "borrow ":
      valueToStr()
  of mnkToSlice:
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
  of mnkConstr:
    tree "construct (":
      commaSeparated:
        argToStr()
      result.add ")"
  of mnkObjConstr:
    tree "(":
      commaSeparated:
        result.add next(nodes, i).field.name.s & ": "
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
      commaSeparated:
        argToStr()
      result.add ") (raises)"
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
    result.add "<error: " & $nodes[i].kind & ">"
    inc i

template exprToStr() =
  exprToStr(nodes, i, result, c)

proc renderNameWithType(tree: MirTree, i: var int, result: var string,
                        c: RenderCtx) =
  let n {.cursor.} = next(tree, i)
  singleToStr(n, result, c)
  result.add ": "
  result.add typeToString(n.typ)

proc renderList(tree: MirTree, i: var int, indent: int, result: var string,
                c: RenderCtx)

template renderList(indent: int) =
  mixin renderList
  renderList(treeParam(), i, indent, result, c)

template stmtToStr(indent: int) =
  mixin stmtToStr
  stmtToStr(treeParam(), i, indent, result, c)

proc stmtToStr(nodes: MirTree, i: var int, indent: int, result: var string,
               c: RenderCtx) =
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
  of mnkStmtList:
    renderList(indent)
  of mnkTry:
    tree "try:\n":
      tab:
        stmtToStr(indent)
      renderList(indent)
  of mnkExcept:
    tree "except\n":
      renderList(indent)
  of mnkFinally:
    tree "finally:\n":
      tab:
        stmtToStr(indent)
  of mnkScope:
    tree "scope:\n":
      tab:
        renderList(indent)
  of mnkIf:
    tree "if ":
      valueToStr()
      result.add ":\n"
      tab:
        renderList(indent)
  of mnkCase:
    tree "case ":
      valueToStr()
      result.add "\n"
      # use ``renderList`` for simplicity, even though it allows for
      # structures that are invalid
      renderList(indent)
  of mnkBranch:
    tree "of ":
      for j in 0..<n.len:
        if j > 0:
          result.add ", "
        singleToStr()
      result.add ":\n"
      tab:
        renderList(indent)
  of mnkBlock:
    tree "block L" & $n.label.int & ":\n":
      tab:
        renderList(indent)
  of mnkRepeat:
    tree "while true:\n":
      tab:
        renderList(indent)
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
      if nodes[i].typ.kind == tyVoid: ""
      else: "discard "
    tree lead:
      exprToStr()
    result.add "\n"
  of mnkRaise:
    tree "raise ":
      valueToStr()
    result.add "\n"
  of mnkDestroy:
    tree "destroy ":
      valueToStr()
      result.add "\n"
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

proc renderList(tree: MirTree, i: var int, indent: int, result: var string,
                c: RenderCtx) =
  while i < tree.len and tree[i].kind != mnkEnd:
    stmtToStr(indent)

proc exprToStr*(tree: MirTree, n: NodePosition; env: ptr MirEnv = nil;
                body: ptr MirBody = nil): string =
  ## Renders the expression at `n` into a human-readable text representation.
  var i = n.int
  exprToStr(tree, i, result, RenderCtx(env: env, body: body))

proc stmtToStr*(tree: MirTree, n: NodePosition; env: ptr MirEnv = nil;
                body: ptr MirBody = nil): string =
  ## Renders the statement at `n` into a human-readable text representation.
  var i = n.int
  stmtToStr(tree, i, 0, result, RenderCtx(env: env, body: body))

proc render*(tree: MirTree; env: ptr MirEnv = nil;
             body: ptr MirBody = nil): string =
  ## Renders `tree` into a human-readable text representation. The output is
  ## meant for debugging and tracing and is not guaranteed to have a stable
  ## format.
  var i = 0
  renderList(tree, i, 0, result, RenderCtx(env: env, body: body))
