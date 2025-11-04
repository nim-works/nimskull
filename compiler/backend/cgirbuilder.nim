## Provides some convenience types and routines for building CGIR trees.
## The underlying idea is to use one big staging buffer that contains "ref"
## nodes, which are nodes storing a local pointer to some node sequence. Once
## building is finished, all references are resolved (i.e., the ref is replaced
## with the ref'd tree).
##
## The downside is that this requires a resolution pass. On the upside,
## this allows for much easier construction, and there's also less temporary
## sequences.

import
  std/macros,
  compiler/ast/lineinfos,
  compiler/backend/cgir2

type
  Builder* = object
    nodes: seq[CgNode]
    loc: uint32        ## source-loc to use for nodes
    info: TLineInfo    ## info corresponding to `loc`
  NodeRef* = distinct CgNode
    ## Represents a reference to some node. May only be used with the
    ## `Builder <#Builder>`_ it originated from.
    # to improve performance for the case where leaf or zero-child nodes are
    # returned from a build expression (which is a common occurrence), the ref
    # is a node itself, instead of just a position

const RefTag = cnkInvalid

{.push inline, checks: off, stacktrace: off.}

proc add(bu: var Builder, i: uint32, x: NodeRef): uint32 =
  bu.nodes[i] = CgNode(x)
  result = 1

proc add(bu: var Builder, i: uint32, x: CgNode): uint32 =
  bu.nodes[i] = x
  result = 1

proc add(bu: var Builder, i: uint32, x: openArray[NodeRef]): uint32 =
  for p, it in x.pairs:
    bu.nodes[int(i) + p] = CgNode(it)
  result = x.len.uint32

proc add(bu: var Builder, i: uint32, kind: CgNodeKind, info: uint32,
         val: uint32) =
  bu.nodes[i] = node(kind, info, val)

{.pop.}

template valToNode(x: NodeRef): NodeRef = x
template valToNode(x: CgNode): CgNode = x
template valToNode(x: seq[NodeRef]): seq[NodeRef] = x

template flatLen(x: NodeRef or CgNode): int = 1
template flatLen(x: openArray[NodeRef]): int = x.len

macro buildImpl(b: var Builder, info: uint32, e: untyped): NodeRef =
  ## Emits the actual construction code.
  let b = b
  let info = info
  var idx = genSym(nskVar, "idx")
  var staticLen = 0 ## numbers of nodes statically known
  var dynLen = newIntLitNode(0)

  proc emit(n: NimNode, stmts: NimNode): NimNode =
    case n.kind
    of nnkCall:
      inc staticLen
      let name =
        if n[0].kind == nnkIdent: ident("cnk" & n[0].strVal)
        else:                     n[0]
      copyLineInfo(name, n[0])
      let start = stmts.len
      var count = newIntLitNode(0)
      for i in 1..<n.len:
        count = newCall(ident"+", count, emit(n[i], stmts))
      stmts.insert start, newCall(bindSym"add", b, idx, name, info,
                                  quote do: uint32(`count`))
      stmts.insert start+1, quote do:
        inc `idx`
      result = newIntLitNode(1)
    of nnkSym:
      stmts.add newCall(ident"+=", idx, newCall(bindSym"add", b, idx, n))
      result = newCall(bindSym"flatLen", n)
      dynLen = newCall(ident"+", dynLen, result)
    else:
      unreachable(n.kind)

  if e.kind == nnkSym:
    # it's a pass-through build invocation
    result = quote do:
      when `e` is NodeRef: `e`
      elif `e` is CgNode:  NodeRef(`e`)
      else:
        {.error: "expected a `NodeRef` or `CgNode`".}
  elif e.kind == nnkCall and e.len == 1:
    # it's a zero-child tree
    let name =
      if e[0].kind == nnkIdent: ident("cnk" & e[0].strVal)
      else:                     e[0]
    result = quote do:
      NodeRef(node(`name`, `info`, 0))
  else:
    let start = genSym(nskLet, "start")
    result = newStmtList()
    result.add newLetStmt(start, quote do: `b`.nodes.len)
    result.add newVarStmt(idx, nnkConv.newTree(ident"uint32", start))
    discard emit(e, result)
    # insert the space reservation after the `idx` var stmt
    result.insert 2, quote do:
      `b`.nodes.setLen(`start` + (`staticLen` + `dynLen`))
    result.add quote do:
      NodeRef(node(`RefTag`, uint32(`start`)))

macro build*(b: var Builder, e: untyped): NodeRef =
  ## Constructs the sub-tree represented by `e` and returns a reference to it.
  ##
  ## The following syntax is used for `e`:
  ##
  ## * a call expression describes a tree; it can be arbitrarily nested, and
  ##   every argument represents a child node/sub-tree
  ## * an unary hat (`^`) is used as the unquote operator; the expression
  ##   following it is evaluated normally and passed to `valToNode` (mixed in
  ##   at the callsite)
  ## * identifiers and literal values are implicitly unquoted (e.g., `x` is the
  ##   same as `^x`)
  ## * the star (`*`) operator is used for custom forms. `*x(y, z)` expands to
  ##   `x(<builder>, build(<builder>, y), build(<builder>, z))`
  ##
  ## Example construction expression:
  ##
  ## .. code-block:: nim
  ##
  ##   Call(val, ^complex(...), *call(...))
  # hoist all unquoted expressions to the start:
  let b = b

  proc hoisted(n, stmts: NimNode): NimNode =
    result = genSym(nskLet, "tmp")
    copyLineInfo(result, n)
    stmts.add newLetStmt(result, n)

  proc conv(n: NimNode): NimNode =
    newCall(bindSym("valToNode", brForceOpen), n)

  proc hoist(n, stmts: NimNode, mode: bool): NimNode =
    case n.kind
    of nnkPrefix:
      if n[0].eqIdent("^"):
        if mode:
          result = hoisted(conv(n[1]), stmts)
        else:
          result = n[1]
      elif n[0].eqIdent("*"):
        if mode:
          # switch to native mode
          result = hoisted(hoist(n, stmts, false), stmts)
        else:
          let n = n[1]
          result = n
          for i in 1..<n.len:
            result[i] = hoist(n[i], stmts, mode)
          result.insert 1, copyNimTree(b)
      else:
        error("unexpected syntax", n)
    of nnkCall:
      n[0].expectKind {nnkIdent, nnkPar}
      if mode:
        result = n
        for i in 1..<n.len:
          result[i] = hoist(n[i], stmts, mode)
      else:
        result = newCall(bindSym"build", b, n)
        copyLineInfo(result, n)
    of nnkIdent, nnkSym:
      if mode:
        # identifiers are auto-hoisted
        result = hoisted(conv(n), stmts)
      else:
        result = n
    of nnkStmtList, nnkStmtListExpr:
      # skip single-item statement lists, which may appear after
      # template expansion
      n.expectLen 1
      result = hoist(n[0], stmts, mode)
    of nnkLiterals:
      # for convenience' sake, literals are converted/hoisted too
      result = hoisted(conv(n), stmts)
    else:
      error("unexpected syntax", n)

  var e = e
  if e.kind == nnkStmtList and e.len == 1:
    e = e[0]

  let info = genSym(nskLet, "info")
  result = newStmtList()
  result.add newLetStmt(info, newDotExpr(b, ident"loc"))
  let got = hoist(e, result, true)
  result.add newCall(bindSym"buildImpl", b, info, got)

func get*(bu: Builder): (uint32, TLineInfo) {.inline.} =
  ## Returns the current source-loc/info state.
  (bu.loc, bu.info)

func update*(bu: var Builder, loc: uint32, info: TLineInfo) {.inline.} =
  ## Sets the current source-loc/info state.
  bu.loc = loc
  bu.info = info

# `append` is used often and should be as fast as possible
{.push checks: off, stacktrace: off.}

proc append*(to: var seq[CgNode], bu: sink Builder, start: NodeRef): NodeIndex =
  ## Copies the node/tree referenced by `start` stored in `bu` appending it
  ## to `to`.
  let start = CgNode(start)

  result = to.len.NodeIndex
  if start.kind != RefTag:
    to.add start
    return

  template append(start, fin: uint32) =
    let pos = to.len
    let num = int(fin - start)
    to.setLen(pos + num)
    copyMem(addr to[pos], addr bu.nodes[start], num * sizeof(CgNode))

  var stack = @[(start.val, start.val)]
  while stack.len > 0:
    block outer:
      var (i, last) = stack[^1]
      let prev = i
      while i <= last:
        if bu.nodes[i].kind == RefTag:
          if i > prev:
            # copy everything we got so far
            append(prev, i)

          stack[^1] = (i + 1, last)
          let next = bu.nodes[i].val
          stack.add (next, next)
          break outer
        elif not isLeaf(bu.nodes[i]):
          last += bu.nodes[i].val

        inc i

      if i > prev:
        # copy the rest
        append(prev, i)

      stack.shrink(stack.len - 1)

{.pop.}

proc initBuilder*(): Builder =
  discard
