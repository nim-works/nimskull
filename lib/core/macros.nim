#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

include system/inclrtl
import std/private/since

## This module contains the interface to the compiler's abstract syntax
## tree (`AST`:idx:). Macros operate on this tree.
##
## See also:
## * `macros tutorial <tut3.html>`_
## * `macros section in Nim manual <manual.html#macros>`_

## .. include:: ../../doc/astspec.txt

import system/magics except error, warning, hint

export
  magics.NimNodeKind, magics.NimSymKind, magics.NimTypeKind,
  magics.BindSymRule, magics.bindSym,
  magics.kind, magics.strVal, magics.floatVal, magics.intVal, magics.len,
  magics.symKind, magics.owner, magics.callsite,
  magics.`[]`, magics.`[]=`,
  magics.`floatVal=`, magics.`intVal=`, magics.`strVal=`,
  magics.`==`, magics.eqIdent, magics.sameType,
  magics.newNimNode, magics.newIdentNode, magics.ident,
  magics.genSym,
  magics.copyNimNode, magics.copyNimTree,
  magics.isInstantiationOf,
  magics.signatureHash,
  magics.copyLineInfo,
  magics.getAst, magics.quote,
  magics.add, magics.del,
  magics.getImpl,
  magics.getType, magics.getTypeInst, magics.getTypeImpl, magics.typeKind,
  magics.getSize, magics.getAlign, magics.getOffset

export magics.evalToAst
# ^leaked implementation detail, should ideally not be exported 

type
  NimNodeKinds* = set[NimNodeKind]

const
  nnkLiterals* = {nnkCharLit..nnkNilLit}
    ## `NimNodeKind`s that represent syntax literals
  nnkCallKinds* = {nnkCall, nnkInfix, nnkPrefix, nnkPostfix, nnkCommand,
                   nnkCallStrLit}
  nnkPragmaCallKinds = {nnkExprColonExpr, nnkCall, nnkCallStrLit}
  nnkRequireInitKinds* = {nnkError, nnkIdent, nnkSym, nnkType}
    ## `NimNodeKind`s that require initialization and cannot be created via
    ## general construction routines e.g. `newNimNode`.

proc `[]`*(n: NimNode, i: BackwardsIndex): NimNode = n[n.len - i.int]
  ## Get `n`'s `i`'th child.

template `^^`(n: NimNode, i: untyped): untyped =
  (when i is BackwardsIndex: n.len - int(i) else: int(i))

proc `[]`*[T, U: Ordinal](n: NimNode, x: HSlice[T, U]): seq[NimNode] =
  ## Slice operation for NimNode.
  ## Returns a seq of child of `n` who inclusive range [n[x.a], n[x.b]].
  let xa = n ^^ x.a
  let L = (n ^^ x.b) - xa + 1
  result = newSeq[NimNode](L)
  for i in 0..<L:
    result[i] = n[i + xa]

proc `[]=`*(n: NimNode, i: BackwardsIndex, child: NimNode) =
  ## Set `n`'s `i`'th child to `child`.
  n[n.len - i.int] = child

template `or`*(x, y: NimNode): NimNode =
  ## Evaluate `x` and when it is not an empty node, return
  ## it. Otherwise evaluate to `y`. Can be used to chain several
  ## expressions to get the first expression that is not empty.
  ##
  ## .. code-block:: nim
  ##
  ##   let node = mightBeEmpty() or mightAlsoBeEmpty() or fallbackNode

  let arg = x
  if arg != nil and arg.kind != nnkEmpty:
    arg
  else:
    y

when (NimMajor, NimMinor, NimPatch) >= (1, 3, 5) or defined(nimSymImplTransform):
  export magics.getImplTransformed

proc symBodyHash*(s: NimNode): string {.noSideEffect.} =
  ## Returns a stable digest for symbols derived not only from type signature
  ## and owning module, but also implementation body. All procs/variables used in
  ## the implementation of this symbol are hashed recursively as well, including
  ## magics from system module.
  discard

# XXX: the error, warning, and hint magic have to be declared here for
#      compatibility with the csources compiler; remove them after the next
#      csources update

proc error*(msg: string, n: NimNode = nil) {.magic: "NError", benign.}
  ## Writes an error message at compile time. The optional `n: NimNode`
  ## parameter is used as the source for file and line number information in
  ## the compilation error message.

proc warning*(msg: string, n: NimNode = nil) {.magic: "NWarning", benign.}
  ## Writes a warning message at compile time.

proc hint*(msg: string, n: NimNode = nil) {.magic: "NHint", benign.}
  ## Writes a hint message at compile time.

proc newStrLitNode*(s: string): NimNode {.noSideEffect.} =
  ## Creates a string literal node from `s`.
  result = newNimNode(nnkStrLit)
  result.strVal = s

proc newCommentStmtNode*(s: string): NimNode {.noSideEffect.} =
  ## Creates a comment statement node.
  result = newNimNode(nnkCommentStmt)
  result.strVal = s

proc newIntLitNode*(i: BiggestInt): NimNode =
  ## Creates an int literal node from `i`.
  result = newNimNode(nnkIntLit)
  result.intVal = i

proc newFloatLitNode*(f: BiggestFloat): NimNode =
  ## Creates a float literal node from `f`.
  result = newNimNode(nnkFloatLit)
  result.floatVal = f

when defined(nimskullHasUnaryGenSym):
  template genSym*(kind: NimSymKind = nskGenerated; ident: string): NimNode {.
    deprecated: "genSym no longer takes a `kind` parameter".} =
    ## Generates a fresh symbol that is guaranteed to be unique. The symbol
    ## needs to occur in a declaration context.
    ##
    ## This is a compatibility alias for `genSym <#genSym,string>`_, the `kind`
    ## parameter is ignored.
    {.line.}:
      discard kind 
      genSym(ident)

  template genSym*(kind: NimSymKind): NimNode {.
    deprecated: "genSym no longer takes a `kind` parameter".} =
    ## Generates a fresh symbol that is guaranteed to be unique. The symbol
    ## needs to occur in a declaration context.
    ##
    ## This is a compatibility alias for `genSym <#genSym,string>`_, the `kind`
    ## parameter is ignored.
    {.line.}:
      discard kind
      genSym()

proc toStrLit*(n: NimNode): NimNode =
  ## Converts the AST `n` to the concrete Nim code and wraps that
  ## in a string literal node.
  return newStrLitNode(repr(n))

type
  LineInfo* = object
    filename*: string
    line*,column*: int

proc `$`*(arg: LineInfo): string =
  ## Return a string representation in the form `filepath(line, column)`.
  # BUG: without `result = `, gives compile error
  result = arg.filename & "(" & $arg.line & ", " & $arg.column & ")"

proc lineInfoObj*(n: NimNode): LineInfo =
  ## Returns `LineInfo` of `n`, using absolute path for `filename`.
  result = LineInfo(filename: n.getFile, line: n.getLine, column: n.getColumn)

proc lineInfo*(arg: NimNode): string =
  ## Return line info in the form `filepath(line, column)`.
  $arg.lineInfoObj

proc parseExpr*(s: string): NimNode {.noSideEffect.} =
  ## Compiles the passed string to its AST representation.
  ## Expects a single expression. Raises `ValueError` for parsing errors.
  var errmsg: string
  result = parseExpr(s, errmsg)
  if errmsg.len > 0:
    raise newException(ValueError, errmsg)

proc parseStmt*(s: string): NimNode {.noSideEffect.} =
  ## Compiles the passed string to its AST representation.
  ## Expects one or more statements. Raises `ValueError` for parsing errors.
  var errmsg: string
  result = parseStmt(s, errmsg)
  if errmsg.len > 0:
    raise newException(ValueError, errmsg)

proc expectKind*(n: NimNode, k: NimNodeKind) =
  ## Checks that `n` is of kind `k`. If this is not the case,
  ## compilation aborts with an error message. This is useful for writing
  ## macros that check the AST that is passed to them.
  if n.kind != k: error("Expected a node of kind " & $k & ", got " & $n.kind, n)

proc expectMinLen*(n: NimNode, min: int) =
  ## Checks that `n` has at least `min` children. If this is not the case,
  ## compilation aborts with an error message. This is useful for writing
  ## macros that check its number of arguments.
  if n.len < min: error("Expected a node with at least " & $min & " children, got " & $n.len, n)

proc expectLen*(n: NimNode, len: int) =
  ## Checks that `n` has exactly `len` children. If this is not the case,
  ## compilation aborts with an error message. This is useful for writing
  ## macros that check its number of arguments.
  if n.len != len: error("Expected a node with " & $len & " children, got " & $n.len, n)

proc expectLen*(n: NimNode, min, max: int) =
  ## Checks that `n` has a number of children in the range `min..max`.
  ## If this is not the case, compilation aborts with an error message.
  ## This is useful for writing macros that check its number of arguments.
  if n.len < min or n.len > max:
    error("Expected a node with " & $min & ".." & $max & " children, got " & $n.len, n)

proc newTree*(kind: NimNodeKind,
              children: varargs[NimNode]): NimNode =
  ## Produces a new node with children.
  result = newNimNode(kind)
  result.add(children)

proc newCall*(theProc: NimNode, args: varargs[NimNode]): NimNode =
  ## Produces a new call node. `theProc` is the proc that is called with
  ## the arguments `args[0..]`.
  result = newNimNode(nnkCall)
  result.add(theProc)
  result.add(args)

proc newCall*(theProc: string,
              args: varargs[NimNode]): NimNode =
  ## Produces a new call node. `theProc` is the proc that is called with
  ## the arguments `args[0..]`.
  result = newNimNode(nnkCall)
  result.add(newIdentNode(theProc))
  result.add(args)

proc newLit*(c: char): NimNode =
  ## Produces a new character literal node.
  result = newNimNode(nnkCharLit)
  result.intVal = ord(c)

proc newLit*(i: int): NimNode =
  ## Produces a new integer literal node.
  result = newNimNode(nnkIntLit)
  result.intVal = i

proc newLit*(i: int8): NimNode =
  ## Produces a new integer literal node.
  result = newNimNode(nnkInt8Lit)
  result.intVal = i

proc newLit*(i: int16): NimNode =
  ## Produces a new integer literal node.
  result = newNimNode(nnkInt16Lit)
  result.intVal = i

proc newLit*(i: int32): NimNode =
  ## Produces a new integer literal node.
  result = newNimNode(nnkInt32Lit)
  result.intVal = i

proc newLit*(i: int64): NimNode =
  ## Produces a new integer literal node.
  result = newNimNode(nnkInt64Lit)
  result.intVal = i

proc newLit*(i: uint): NimNode =
  ## Produces a new unsigned integer literal node.
  result = newNimNode(nnkUIntLit)
  result.intVal = BiggestInt(i)

proc newLit*(i: uint8): NimNode =
  ## Produces a new unsigned integer literal node.
  result = newNimNode(nnkUInt8Lit)
  result.intVal = BiggestInt(i)

proc newLit*(i: uint16): NimNode =
  ## Produces a new unsigned integer literal node.
  result = newNimNode(nnkUInt16Lit)
  result.intVal = BiggestInt(i)

proc newLit*(i: uint32): NimNode =
  ## Produces a new unsigned integer literal node.
  result = newNimNode(nnkUInt32Lit)
  result.intVal = BiggestInt(i)

proc newLit*(i: uint64): NimNode =
  ## Produces a new unsigned integer literal node.
  result = newNimNode(nnkUInt64Lit)
  result.intVal = BiggestInt(i)

proc newLit*(b: bool): NimNode =
  ## Produces a new boolean literal node.
  result = if b: bindSym"true" else: bindSym"false"

proc newLit*(s: string): NimNode =
  ## Produces a new string literal node.
  result = newNimNode(nnkStrLit)
  result.strVal = s

when false:
  # the float type is not really a distinct type as described in https://github.com/nim-lang/Nim/issues/5875
  proc newLit*(f: float): NimNode =
    ## Produces a new float literal node.
    result = newNimNode(nnkFloatLit)
    result.floatVal = f

proc newLit*(f: float32): NimNode =
  ## Produces a new float literal node.
  result = newNimNode(nnkFloat32Lit)
  result.floatVal = f

proc newLit*(f: float64): NimNode =
  ## Produces a new float literal node.
  result = newNimNode(nnkFloat64Lit)
  result.floatVal = f

proc newLit*(arg: enum): NimNode =
  result = newCall(
    arg.typeof.getTypeInst,
    newLit(int(arg))
  )

proc newLit*[N,T](arg: array[N,T]): NimNode
proc newLit*[T](arg: seq[T]): NimNode
proc newLit*[T](s: set[T]): NimNode
proc newLit*[T: tuple](arg: T): NimNode

proc newLit*(arg: object): NimNode =
  result = nnkObjConstr.newTree(arg.typeof.getTypeInst)
  for a, b in arg.fieldPairs:
    result.add nnkExprColonExpr.newTree( newIdentNode(a), newLit(b) )

proc newLit*(arg: ref object): NimNode =
  ## produces a new ref type literal node.
  result = nnkObjConstr.newTree(arg.typeof.getTypeInst)
  for a, b in fieldPairs(arg[]):
    result.add nnkExprColonExpr.newTree(newIdentNode(a), newLit(b))

proc newLit*[N,T](arg: array[N,T]): NimNode =
  result = nnkBracket.newTree
  for x in arg:
    result.add newLit(x)

proc newLit*[T](arg: seq[T]): NimNode =
  let bracket = nnkBracket.newTree
  for x in arg:
    bracket.add newLit(x)
  result = nnkPrefix.newTree(
    bindSym"@",
    bracket
  )
  if arg.len == 0:
    # add type cast for empty seq
    var typ = getTypeInst(typeof(arg))
    result = newCall(typ,result)

proc newLit*[T](s: set[T]): NimNode =
  result = nnkCurly.newTree
  for x in s:
    result.add newLit(x)
  if result.len == 0:
    # add type cast for empty set
    var typ = getTypeInst(typeof(s))
    result = newCall(typ,result)

proc newLit*[T: tuple](arg: T): NimNode =
  ## use -d:nimHasWorkaround14720 to restore behavior prior to PR, forcing
  ## a named tuple even when `arg` is unnamed.
  result = nnkTupleConstr.newTree
  when defined(nimHasWorkaround14720) or isNamedTuple(T):
    for a, b in arg.fieldPairs:
      result.add nnkExprColonExpr.newTree(newIdentNode(a), newLit(b))
  else:
    for b in arg.fields:
      result.add newLit(b)

proc nestList*(op: NimNode; pack: NimNode): NimNode =
  ## Nests the list `pack` into a tree of call expressions:
  ## `[a, b, c]` is transformed into `op(a, op(c, d))`.
  ## This is also known as fold expression.
  if pack.len < 1:
    error("`nestList` expects a node with at least 1 child")
  result = pack[^1]
  for i in countdown(pack.len - 2, 0):
    result = newCall(op, pack[i], result)

proc nestList*(op: NimNode; pack: NimNode; init: NimNode): NimNode =
  ## Nests the list `pack` into a tree of call expressions:
  ## `[a, b, c]` is transformed into `op(a, op(c, d))`.
  ## This is also known as fold expression.
  result = init
  for i in countdown(pack.len - 1, 0):
    result = newCall(op, pack[i], result)

const collapseSymChoice = not defined(nimLegacyMacrosCollapseSymChoice)

proc treeTraverse(n: NimNode; res: var string; level = 0; isLisp = false, indented = false) {.benign.} =
  if level > 0:
    if indented:
      res.add("\n")
      for i in 0 .. level-1:
        if isLisp:
          res.add(" ")          # dumpLisp indentation
        else:
          res.add("  ")         # dumpTree indentation
    else:
      res.add(" ")

  if isLisp:
    res.add("(")
  res.add(($n.kind).substr(3))

  case n.kind
  of nnkEmpty, nnkNilLit:
    discard # same as nil node in this representation
  of nnkCharLit .. nnkInt64Lit:
    res.add(" " & $n.intVal)
  of nnkFloatLit .. nnkFloat64Lit:
    res.add(" " & $n.floatVal)
  of nnkStrLit .. nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym:
    res.add(" " & $n.strVal.newLit.repr)
  elif n.kind in {nnkOpenSymChoice, nnkClosedSymChoice} and collapseSymChoice:
    res.add(" " & $n.len)
    if n.len > 0:
      var allSameSymName = true
      for i in 0..<n.len:
        if n[i].kind != nnkSym or not eqIdent(n[i], n[0]):
          allSameSymName = false
          break
      if allSameSymName:
        res.add(" " & $n[0].strVal.newLit.repr)
      else:
        for j in 0 ..< n.len:
          n[j].treeTraverse(res, level+1, isLisp, indented)
  else:
    for j in 0 ..< n.len:
      n[j].treeTraverse(res, level+1, isLisp, indented)

  if isLisp:
    res.add(")")

proc treeRepr*(n: NimNode): string {.benign.} =
  ## Convert the AST `n` to a human-readable tree-like string.
  ##
  ## See also `repr`, `lispRepr`, and `astGenRepr`.
  result = ""
  n.treeTraverse(result, isLisp = false, indented = true)

proc lispRepr*(n: NimNode; indented = false): string {.benign.} =
  ## Convert the AST `n` to a human-readable lisp-like string.
  ##
  ## See also `repr`, `treeRepr`, and `astGenRepr`.
  result = ""
  n.treeTraverse(result, isLisp = true, indented = indented)

proc astGenRepr*(n: NimNode): string {.benign.} =
  ## Convert the AST `n` to the code required to generate that AST.
  ##
  ## See also `repr`, `treeRepr`, and `lispRepr`.

  const
    NodeKinds = {nnkEmpty, nnkIdent, nnkSym, nnkCommentStmt}
    LitKinds = {nnkCharLit..nnkInt64Lit, nnkFloatLit..nnkFloat64Lit, nnkStrLit..nnkTripleStrLit}

  proc traverse(res: var string, level: int, n: NimNode) {.benign.} =
    for i in 0..level-1: res.add "  "
    if n.kind in NodeKinds:
      res.add("new" & ($n.kind).substr(3) & "Node(")
    elif n.kind in LitKinds:
      res.add("newLit(")
    elif n.kind == nnkNilLit:
      res.add("newNilLit()")
    else:
      res.add($n.kind)

    case n.kind
    of nnkEmpty, nnkNilLit: discard
    of nnkCharLit: res.add("'" & $chr(n.intVal) & "'")
    of nnkIntLit..nnkInt64Lit: res.add($n.intVal)
    of nnkFloatLit..nnkFloat64Lit: res.add($n.floatVal)
    of nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym:
      res.add(n.strVal.newLit.repr)
    elif n.kind in {nnkOpenSymChoice, nnkClosedSymChoice} and collapseSymChoice:
      res.add(", # unrepresentable symbols: " & $n.len)
      if n.len > 0:
        res.add(" " & n[0].strVal.newLit.repr)
    else:
      res.add(".newTree(")
      for j in 0..<n.len:
        res.add "\n"
        traverse(res, level + 1, n[j])
        if j != n.len-1:
          res.add(",")

      res.add("\n")
      for i in 0..level-1: res.add "  "
      res.add(")")

    if n.kind in NodeKinds+LitKinds:
      res.add(")")

  result = ""
  traverse(result, 0, n)

macro dumpTree*(s: untyped): untyped = echo s.treeRepr
  ## Accepts a block of nim code and prints the parsed abstract syntax
  ## tree using the `treeRepr` proc. Printing is done *at compile time*.
  ##
  ## You can use this as a tool to explore the Nim's abstract syntax
  ## tree and to discover what kind of nodes must be created to represent
  ## a certain expression/statement.
  ##
  ## For example:
  ##
  ## .. code-block:: nim
  ##    dumpTree:
  ##      echo "Hello, World!"
  ##
  ## Outputs:
  ##
  ## .. code-block::
  ##    StmtList
  ##      Command
  ##        Ident "echo"
  ##        StrLit "Hello, World!"
  ##
  ## Also see `dumpAstGen` and `dumpLisp`.

macro dumpLisp*(s: untyped): untyped = echo s.lispRepr(indented = true)
  ## Accepts a block of nim code and prints the parsed abstract syntax
  ## tree using the `lispRepr` proc. Printing is done *at compile time*.
  ##
  ## You can use this as a tool to explore the Nim's abstract syntax
  ## tree and to discover what kind of nodes must be created to represent
  ## a certain expression/statement.
  ##
  ## For example:
  ##
  ## .. code-block:: nim
  ##    dumpLisp:
  ##      echo "Hello, World!"
  ##
  ## Outputs:
  ##
  ## .. code-block::
  ##    (StmtList
  ##     (Command
  ##      (Ident "echo")
  ##      (StrLit "Hello, World!")))
  ##
  ## Also see `dumpAstGen` and `dumpTree`.

macro dumpAstGen*(s: untyped): untyped = echo s.astGenRepr
  ## Accepts a block of nim code and prints the parsed abstract syntax
  ## tree using the `astGenRepr` proc. Printing is done *at compile time*.
  ##
  ## You can use this as a tool to write macros quicker by writing example
  ## outputs and then copying the snippets into the macro for modification.
  ##
  ## For example:
  ##
  ## .. code-block:: nim
  ##    dumpAstGen:
  ##      echo "Hello, World!"
  ##
  ## Outputs:
  ##
  ## .. code-block:: nim
  ##    nnkStmtList.newTree(
  ##      nnkCommand.newTree(
  ##        newIdentNode("echo"),
  ##        newLit("Hello, World!")
  ##      )
  ##    )
  ##
  ## Also see `dumpTree` and `dumpLisp`.

proc newEmptyNode*(): NimNode {.noSideEffect.} =
  ## Create a new empty node.
  result = newNimNode(nnkEmpty)

proc newStmtList*(stmts: varargs[NimNode]): NimNode =
  ## Create a new statement list.
  result = newNimNode(nnkStmtList).add(stmts)

proc newPar*(expr: NimNode): NimNode =
  ## Create a new parentheses-enclosed expression.
  ##
  ## This does not construct tuples, for that use `nnkTupleConstr` nodes.
  newNimNode(nnkPar).add(expr)

proc newPar*(exprs: varargs[NimNode]): NimNode {.error:
  "newPar/nnkPar does not construct tuples anymore, for that use nnkTupleConstr nodes."}

proc newBlockStmt*(label, body: NimNode): NimNode =
  ## Create a new block statement with label.
  return newNimNode(nnkBlockStmt).add(label, body)

proc newBlockStmt*(body: NimNode): NimNode =
  ## Create a new block: stmt.
  return newNimNode(nnkBlockStmt).add(newEmptyNode(), body)

proc newVarStmt*(name, value: NimNode): NimNode =
  ## Create a new var stmt.
  return newNimNode(nnkVarSection).add(
    newNimNode(nnkIdentDefs).add(name, newNimNode(nnkEmpty), value))

proc newLetStmt*(name, value: NimNode): NimNode =
  ## Create a new let stmt.
  return newNimNode(nnkLetSection).add(
    newNimNode(nnkIdentDefs).add(name, newNimNode(nnkEmpty), value))

proc newConstStmt*(name, value: NimNode): NimNode =
  ## Create a new const stmt.
  newNimNode(nnkConstSection).add(
    newNimNode(nnkConstDef).add(name, newNimNode(nnkEmpty), value))

proc newAssignment*(lhs, rhs: NimNode): NimNode =
  return newNimNode(nnkAsgn).add(lhs, rhs)

proc newDotExpr*(a, b: NimNode): NimNode =
  ## Create new dot expression.
  ## a.dot(b) -> `a.b`
  return newNimNode(nnkDotExpr).add(a, b)

proc newColonExpr*(a, b: NimNode): NimNode =
  ## Create new colon expression.
  ## newColonExpr(a, b) -> `a: b`
  newNimNode(nnkExprColonExpr).add(a, b)

proc newIdentDefs*(name, kind: NimNode;
                   default = newEmptyNode()): NimNode =
  ## Creates a new `nnkIdentDefs` node of a specific kind and value.
  ##
  ## `nnkIdentDefs` need to have at least three children, but they can have
  ## more: first comes a list of identifiers followed by a type and value
  ## nodes. This helper proc creates a three node subtree, the first subnode
  ## being a single identifier name. Both the `kind` node and `default`
  ## (value) nodes may be empty depending on where the `nnkIdentDefs`
  ## appears: tuple or object definitions will have an empty `default` node,
  ## `let` or `var` blocks may have an empty `kind` node if the
  ## identifier is being assigned a value. Example:
  ##
  ## .. code-block:: nim
  ##
  ##   var varSection = newNimNode(nnkVarSection).add(
  ##     newIdentDefs(ident("a"), ident("string")),
  ##     newIdentDefs(ident("b"), newEmptyNode(), newLit(3)))
  ##   # --> var
  ##   #       a: string
  ##   #       b = 3
  ##
  ## If you need to create multiple identifiers you need to use the lower level
  ## `newNimNode`:
  ##
  ## .. code-block:: nim
  ##
  ##   result = newNimNode(nnkIdentDefs).add(
  ##     ident("a"), ident("b"), ident("c"), ident("string"),
  ##       newStrLitNode("Hello"))
  newNimNode(nnkIdentDefs).add(name, kind, default)

proc newNilLit*(): NimNode =
  ## New nil literal shortcut.
  result = newNimNode(nnkNilLit)

proc last*(node: NimNode): NimNode = node[node.len-1]
  ## Return the last item in nodes children. Same as `node[^1]`.


const
  RoutineNodes* = {nnkProcDef, nnkFuncDef, nnkMethodDef, nnkDo, nnkLambda,
                   nnkIteratorDef, nnkTemplateDef, nnkConverterDef, nnkMacroDef}
  AtomicNodes* = {nnkEmpty..nnkNilLit}
  CallNodes* = {nnkCall, nnkInfix, nnkPrefix, nnkPostfix, nnkCommand,
    nnkCallStrLit, nnkHiddenCallConv}

proc expectKind*(n: NimNode; k: set[NimNodeKind]) =
  ## Checks that `n` is of kind `k`. If this is not the case,
  ## compilation aborts with an error message. This is useful for writing
  ## macros that check the AST that is passed to them.
  if n.kind notin k: error("Expected one of " & $k & ", got " & $n.kind, n)

proc newProc*(name = newEmptyNode();
              params: openArray[NimNode] = [newEmptyNode()];
              body: NimNode = newStmtList();
              procType = nnkProcDef;
              pragmas: NimNode = newEmptyNode()): NimNode =
  ## Shortcut for creating a new proc.
  ##
  ## The `params` array must start with the return type of the proc,
  ## followed by a list of IdentDefs which specify the params.
  if procType notin RoutineNodes:
    error("Expected one of " & $RoutineNodes & ", got " & $procType)
  pragmas.expectKind({nnkEmpty, nnkPragma})
  result = newNimNode(procType).add(
    name,
    newEmptyNode(),
    newEmptyNode(),
    newNimNode(nnkFormalParams).add(params),
    pragmas,
    newEmptyNode(),
    body)

proc newIfStmt*(branches: varargs[tuple[cond, body: NimNode]]): NimNode =
  ## Constructor for `if` statements.
  ##
  ## .. code-block:: nim
  ##
  ##    newIfStmt(
  ##      (Ident, StmtList),
  ##      ...
  ##    )
  ##
  result = newNimNode(nnkIfStmt)
  if len(branches) < 1:
    error("If statement must have at least one branch")
  for i in branches:
    result.add(newTree(nnkElifBranch, i.cond, i.body))

proc newEnum*(name: NimNode, fields: openArray[NimNode],
              public, pure: bool): NimNode =

  ## Creates a new enum. `name` must be an ident. Fields are allowed to be
  ## either idents or EnumFieldDef
  ##
  ## .. code-block:: nim
  ##
  ##    newEnum(
  ##      name    = ident("Colors"),
  ##      fields  = [ident("Blue"), ident("Red")],
  ##      public  = true, pure = false)
  ##
  ##    # type Colors* = Blue Red
  ##

  expectKind name, nnkIdent
  if len(fields) < 1:
    error("Enum must contain at least one field")
  for field in fields:
    expectKind field, {nnkIdent, nnkEnumFieldDef}

  let enumBody = newNimNode(nnkEnumTy).add(newEmptyNode()).add(fields)
  var typeDefArgs = [name, newEmptyNode(), enumBody]

  if public:
    let postNode = newNimNode(nnkPostfix).add(
      newIdentNode("*"), typeDefArgs[0])

    typeDefArgs[0] = postNode

  if pure:
    let pragmaNode = newNimNode(nnkPragmaExpr).add(
      typeDefArgs[0],
      add(newNimNode(nnkPragma), newIdentNode("pure")))

    typeDefArgs[0] = pragmaNode

  let
    typeDef   = add(newNimNode(nnkTypeDef), typeDefArgs)
    typeSect  = add(newNimNode(nnkTypeSection), typeDef)

  return typeSect

proc copyChildrenTo*(src, dest: NimNode) =
  ## Copy all children from `src` to `dest`.
  for i in 0 ..< src.len:
    dest.add src[i].copyNimTree

template expectRoutine(node: NimNode) =
  expectKind(node, RoutineNodes)

proc name*(someProc: NimNode): NimNode =
  someProc.expectRoutine
  result = someProc[0]
  if result.kind == nnkPostfix:
    if result[1].kind == nnkAccQuoted:
      result = result[1][0]
    else:
      result = result[1]
  elif result.kind == nnkAccQuoted:
    result = result[0]

proc `name=`*(someProc: NimNode; val: NimNode) =
  someProc.expectRoutine
  if someProc[0].kind == nnkPostfix:
    someProc[0][1] = val
  else: someProc[0] = val

proc params*(someProc: NimNode): NimNode =
  someProc.expectRoutine
  result = someProc[3]
proc `params=`* (someProc: NimNode; params: NimNode) =
  someProc.expectRoutine
  expectKind(params, nnkFormalParams)
  someProc[3] = params

proc pragma*(someProc: NimNode): NimNode =
  ## Get the pragma of a proc type.
  ## These will be expanded.
  if someProc.kind == nnkProcTy:
    result = someProc[1]
  else:
    someProc.expectRoutine
    result = someProc[4]
proc `pragma=`*(someProc: NimNode; val: NimNode) =
  ## Set the pragma of a proc type.
  expectKind(val, {nnkEmpty, nnkPragma})
  if someProc.kind == nnkProcTy:
    someProc[1] = val
  else:
    someProc.expectRoutine
    someProc[4] = val

proc addPragma*(someProc, pragma: NimNode) =
  ## Adds pragma to routine definition.
  someProc.expectKind(RoutineNodes + {nnkProcTy})
  var pragmaNode = someProc.pragma
  if pragmaNode.isNil or pragmaNode.kind == nnkEmpty:
    pragmaNode = newNimNode(nnkPragma)
    someProc.pragma = pragmaNode
  pragmaNode.add(pragma)

template badNodeKind(n, f) =
  error("Invalid node kind " & $n.kind & " for macros.`" & $f & "`", n)

proc body*(someProc: NimNode): NimNode =
  case someProc.kind:
  of RoutineNodes:
    return someProc[6]
  of nnkBlockStmt, nnkWhileStmt:
    return someProc[1]
  of nnkForStmt:
    return someProc.last
  else:
    badNodeKind someProc, "body"

proc `body=`*(someProc: NimNode, val: NimNode) =
  case someProc.kind
  of RoutineNodes:
    someProc[6] = val
  of nnkBlockStmt, nnkWhileStmt:
    someProc[1] = val
  of nnkForStmt:
    someProc[len(someProc)-1] = val
  else:
    badNodeKind someProc, "body="

proc basename*(a: NimNode): NimNode =
  ## Pull an identifier from prefix/postfix expressions.
  case a.kind
  of nnkIdent: result = a
  of nnkPostfix, nnkPrefix: result = a[1]
  of nnkPragmaExpr: result = basename(a[0])
  else:
    error("Do not know how to get basename of (" & treeRepr(a) & ")\n" &
      repr(a), a)

proc `$`*(node: NimNode): string =
  ## Get the string of an identifier node.
  case node.kind
  of nnkPostfix:
    result = node.basename.strVal & "*"
  of nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkSym, nnkIdent:
    result = node.strVal
  of nnkOpenSymChoice, nnkClosedSymChoice:
    result = $node[0]
  of nnkAccQuoted:
    result = $node[0]
  else:
    badNodeKind node, "$"

iterator items*(n: NimNode): NimNode {.inline.} =
  ## Iterates over the children of the NimNode `n`.
  for i in 0 ..< n.len:
    yield n[i]

iterator pairs*(n: NimNode): (int, NimNode) {.inline.} =
  ## Iterates over the children of the NimNode `n` and its indices.
  for i in 0 ..< n.len:
    yield (i, n[i])

iterator children*(n: NimNode): NimNode {.inline.} =
  ## Iterates over the children of the NimNode `n`.
  for i in 0 ..< n.len:
    yield n[i]

template findChild*(n: NimNode; cond: untyped): NimNode {.dirty.} =
  ## Find the first child node matching condition (or nil).
  ##
  ## .. code-block:: nim
  ##   var res = findChild(n, it.kind == nnkPostfix and
  ##                          it.basename.ident == ident"foo")
  block:
    var res: NimNode
    for it in n.children:
      if cond:
        res = it
        break
    res

proc insert*(a: NimNode; pos: int; b: NimNode) =
  ## Insert node `b` into node `a` at `pos`.
  if len(a)-1 < pos:
    # add some empty nodes first
    for i in len(a)-1..pos-2:
      a.add newEmptyNode()
    a.add b
  else:
    # push the last item onto the list again
    # and shift each item down to pos up one
    a.add(a[a.len-1])
    for i in countdown(len(a) - 3, pos):
      a[i + 1] = a[i]
    a[pos] = b

proc `basename=`*(a: NimNode; val: string) =
  case a.kind
  of nnkIdent:
    a.strVal = val
  of nnkPostfix, nnkPrefix:
    a[1] = ident(val)
  of nnkPragmaExpr: `basename=`(a[0], val)
  else:
    error("Do not know how to get basename of (" & treeRepr(a) & ")\n" &
      repr(a), a)

proc postfix*(node: NimNode; op: string): NimNode =
  newNimNode(nnkPostfix).add(ident(op), node)

proc prefix*(node: NimNode; op: string): NimNode =
  newNimNode(nnkPrefix).add(ident(op), node)

proc infix*(a: NimNode; op: string;
            b: NimNode): NimNode =
  newNimNode(nnkInfix).add(ident(op), a, b)

proc unpackPostfix*(node: NimNode): tuple[node: NimNode; op: string] =
  node.expectKind nnkPostfix
  result = (node[1], $node[0])

proc unpackPrefix*(node: NimNode): tuple[node: NimNode; op: string] =
  node.expectKind nnkPrefix
  result = (node[1], $node[0])

proc unpackInfix*(node: NimNode): tuple[left: NimNode; op: string; right: NimNode] =
  expectKind(node, nnkInfix)
  result = (node[1], $node[0], node[2])

proc copy*(node: NimNode): NimNode =
  ## An alias for `copyNimTree<#copyNimTree,NimNode>`_.
  return node.copyNimTree()

proc expectIdent*(n: NimNode, name: string) {.since: (1,1).} =
  ## Check that `eqIdent(n,name)` holds true. If this is not the
  ## case, compilation aborts with an error message. This is useful
  ## for writing macros that check the AST that is passed to them.
  if not eqIdent(n, name):
    error("Expected identifier to be `" & name & "` here", n)

proc hasArgOfName*(params: NimNode; name: string): bool =
  ## Search `nnkFormalParams` for an argument.
  expectKind(params, nnkFormalParams)
  for i in 1..<params.len:
    for j in 0..<params[i].len-2:
      if name.eqIdent($params[i][j]):
        return true

proc addIdentIfAbsent*(dest: NimNode, ident: string) =
  ## Add `ident` to `dest` if it is not present. This is intended for use
  ## with pragmas.
  for node in dest.children:
    case node.kind
    of nnkIdent:
      if ident.eqIdent($node): return
    of nnkExprColonExpr:
      if ident.eqIdent($node[0]): return
    else: discard
  dest.add(ident(ident))

proc boolVal*(n: NimNode): bool {.noSideEffect.} =
  if n.kind == nnkIntLit: n.intVal != 0
  else: n == bindSym"true" # hacky solution for now

when declared(magics.nodeId):
  export magics.nodeId

macro expandMacros*(body: typed): untyped =
  ## Expands one level of macro - useful for debugging.
  ## Can be used to inspect what happens when a macro call is expanded,
  ## without altering its result.
  ##
  ## For instance,
  ##
  ## .. code-block:: nim
  ##   import std/[sugar, macros]
  ##
  ##   let
  ##     x = 10
  ##     y = 20
  ##   expandMacros:
  ##     dump(x + y)
  ##
  ## will actually dump `x + y`, but at the same time will print at
  ## compile time the expansion of the `dump` macro, which in this
  ## case is `debugEcho ["x + y", " = ", x + y]`.
  echo body.toStrLit
  result = body

proc extractTypeImpl(n: NimNode): NimNode =
    ## attempts to extract the type definition of the given symbol
    case n.kind
    of nnkSym: # can extract an impl
      result = n.getImpl.extractTypeImpl()
    of nnkObjectTy, nnkRefTy, nnkPtrTy: result = n
    of nnkBracketExpr:
      if n.typeKind == ntyTypeDesc:
        result = n[1].extractTypeImpl()
      else:
        doAssert n.typeKind == ntyGenericInst
        result = n[0].getImpl()
    of nnkTypeDef:
      result = n[2]
    else: error("Invalid node to retrieve type implementation of: " & $n.kind)

proc customPragmaNode(n: NimNode): NimNode =
  expectKind(n, {nnkSym, nnkDotExpr, nnkBracketExpr, nnkTypeOfExpr, nnkCheckedFieldExpr})
  let
    typ = n.getTypeInst()

  if typ.kind == nnkBracketExpr and typ.len > 1 and typ[1].kind == nnkProcTy:
    return typ[1][1]
  elif typ.typeKind == ntyTypeDesc:
    let impl = getImpl(
      if kind(typ[1]) == nnkBracketExpr: typ[1][0]
      else: typ[1]
    )
    if impl[0].kind == nnkPragmaExpr:
      return impl[0][1]
    else:
      return impl[0] # handle types which don't have macro at all

  if n.kind == nnkSym: # either a variable or a proc
    let impl = n.getImpl()
    if impl.kind in RoutineNodes:
      return impl.pragma
    # xxx: this and the next branch are a hack, it may seem "helpful" to lookup
    #      pragmas on the type, but that doesn't actually make sense. metadata
    #      on the type is not metadata on the symbol. This also demonstrates
    #      how compiler internals are leaking out unnecessarily, as the
    #      compiler further normalizes the ast, the implied schema of NimNode
    #      will keep churning and these traversals are all very fragile.
    elif impl.kind == nnkIdentDefs and impl[0].kind == nnkPragmaExpr and
         impl[0][1].len > 0:
      return impl[0][1]
    else:
      let timpl = typ.getImpl()
      if timpl.len>0 and timpl[0].len>1:
        return timpl[0][1]
      else:
        return timpl

  if n.kind in {nnkDotExpr, nnkCheckedFieldExpr}:
    let name = $(if n.kind == nnkCheckedFieldExpr: n[0][1] else: n[1])
    let typInst = getTypeInst(if n.kind == nnkCheckedFieldExpr or n[0].kind == nnkHiddenDeref: n[0][0] else: n[0])
    var typDef = getImpl(
      if typInst.kind in {nnkVarTy, nnkBracketExpr}: typInst[0]
      else: typInst
    )
    while typDef != nil:
      typDef.expectKind(nnkTypeDef)
      let typ = typDef[2].extractTypeImpl()
      typ.expectKind({nnkRefTy, nnkPtrTy, nnkObjectTy})
      let isRef = typ.kind in {nnkRefTy, nnkPtrTy}
      if isRef and typ[0].kind in {nnkSym, nnkBracketExpr}: # defines ref type for another object(e.g. X = ref X)
        typDef = getImpl(typ[0])
      else: # object definition, maybe an object directly defined as a ref type
        let
          obj = (if isRef: typ[0] else: typ)
        var identDefsStack = newSeq[NimNode](obj[2].len)
        for i in 0..<identDefsStack.len: identDefsStack[i] = obj[2][i]
        while identDefsStack.len > 0:
          var identDefs = identDefsStack.pop()

          case identDefs.kind
          of nnkRecList:
            for child in identDefs.children:
              identDefsStack.add(child)
          of nnkRecCase:
            # Add condition definition
            identDefsStack.add(identDefs[0])
            # Add branches
            for i in 1 ..< identDefs.len:
              identDefsStack.add(identDefs[i].last)
          else:
            for i in 0 .. identDefs.len - 3:
              let varNode = identDefs[i]
              if varNode.kind == nnkPragmaExpr:
                var varName = varNode[0]
                if varName.kind == nnkPostfix:
                  # This is a public field. We are skipping the postfix *
                  varName = varName[1]
                if eqIdent($varName, name):
                  return varNode[1]

        if obj[1].kind == nnkOfInherit: # explore the parent object
          typDef = getImpl(obj[1][0])
        else:
          typDef = nil

macro hasCustomPragma*(n: typed, cp: typed{nkSym}): untyped =
  ## Expands to `true` if expression `n` which is expected to be `nnkDotExpr`
  ## (if checking a field), a proc or a type has custom pragma `cp`.
  ##
  ## See also `getCustomPragmaVal`.
  ##
  ## .. code-block:: nim
  ##   template myAttr() {.pragma.}
  ##   type
  ##     MyObj = object
  ##       myField {.myAttr.}: int
  ##
  ##   proc myProc() {.myAttr.} = discard
  ##
  ##   var o: MyObj
  ##   assert(o.myField.hasCustomPragma(myAttr))
  ##   assert(myProc.hasCustomPragma(myAttr))
  let pragmaNode = customPragmaNode(n)
  for p in pragmaNode:
    if (p.kind == nnkSym and p == cp) or
        (p.kind in nnkPragmaCallKinds and p.len > 0 and p[0].kind == nnkSym and p[0] == cp):
      return newLit(true)
  return newLit(false)

macro getCustomPragmaVal*(n: typed, cp: typed{nkSym}): untyped =
  ## Expands to value of custom pragma `cp` of expression `n` which is expected
  ## to be `nnkDotExpr`, a proc or a type.
  ##
  ## See also `hasCustomPragma`
  ##
  ## .. code-block:: nim
  ##   template serializationKey(key: string) {.pragma.}
  ##   type
  ##     MyObj {.serializationKey: "mo".} = object
  ##       myField {.serializationKey: "mf".}: int
  ##   var o: MyObj
  ##   assert(o.myField.getCustomPragmaVal(serializationKey) == "mf")
  ##   assert(o.getCustomPragmaVal(serializationKey) == "mo")
  ##   assert(MyObj.getCustomPragmaVal(serializationKey) == "mo")
  result = nil
  let pragmaNode = customPragmaNode(n)
  for p in pragmaNode:
    if p.kind in nnkPragmaCallKinds and p.len > 0 and p[0].kind == nnkSym and p[0] == cp:
      if p.len == 2:
        result = p[1]
      else:
        let def = p[0].getImpl[3]
        result = newTree(nnkTupleConstr)
        for i in 1 ..< def.len:
          let key = def[i][0]
          let val = p[i]
          result.add newTree(nnkExprColonExpr, key, val)
      break
  if result.kind == nnkEmpty:
    error(n.repr & " doesn't have a pragma named " & cp.repr()) # returning an empty node results in most cases in a cryptic error,

macro varargsLen*(x: varargs[untyped]): int {.since: (1, 1).} =
  ## returns number of variadic arguments in `x`
  newLit(x.len)

macro unpackVarargs*(callee: untyped; args: varargs[untyped]): untyped =
  ## Calls `callee` with `args` unpacked as individual arguments.
  ## This is useful in 2 cases:
  ## * when forwarding `varargs[T]` for some typed `T`
  ## * when forwarding `varargs[untyped]` when `args` can potentially be empty,
  ##   due to a compiler limitation
  runnableExamples:
    template call1(fun: typed; args: varargs[untyped]): untyped =
      unpackVarargs(fun, args)
      # when varargsLen(args) > 0: fun(args) else: fun() # this would also work
    template call2(fun: typed; args: varargs[typed]): untyped =
      unpackVarargs(fun, args)
    proc fn1(a = 0, b = 1) = discard (a, b)
    call1(fn1, 10, 11)
    call1(fn1) # `args` is empty in this case
    if false: call2(echo, 10, 11) # would print 1011
  result = newCall(callee)
  for i in 0 ..< args.len:
    result.add args[i]

proc getProjectPath*(): string = discard
  ## Returns the path to the currently compiling project.
  ##
  ## This is not to be confused with `system.currentSourcePath <system.html#currentSourcePath.t>`_
  ## which returns the path of the source file containing that template
  ## call.
  ##
  ## For example, assume a `dir1/foo.nim` that imports a `dir2/bar.nim`,
  ## have the `bar.nim` print out both `getProjectPath` and
  ## `currentSourcePath` outputs.
  ##
  ## Now when `foo.nim` is compiled, the `getProjectPath` from
  ## `bar.nim` will return the `dir1/` path, while the `currentSourcePath`
  ## will return the path to the `bar.nim` source file.
  ##
  ## Now when `bar.nim` is compiled directly, the `getProjectPath`
  ## will now return the `dir2/` path, and the `currentSourcePath`
  ## will still return the same path, the path to the `bar.nim` source
  ## file.
  ##
  ## The path returned by this proc is set at compile time.
  ##
  ## See also:
  ## * `getCurrentDir proc <os.html#getCurrentDir>`_

proc isExported*(n: NimNode): bool {.noSideEffect.} =
  ## Returns whether the symbol is exported or not.

proc extractDocCommentsAndRunnables*(n: NimNode): NimNode =
  ## returns a `nnkStmtList` containing the top-level doc comments and
  ## runnableExamples in `a`, stopping at the first child that is neither.
  ## Example:
  ##
  ## .. code-block:: nim
  ##  import std/macros
  ##  macro transf(a): untyped =
  ##    result = quote do:
  ##      proc fun2*() = discard
  ##    let header = extractDocCommentsAndRunnables(a.body)
  ##    # correct usage: rest is appended
  ##    result.body = header
  ##    result.body.add quote do: discard # just an example
  ##    # incorrect usage: nesting inside a nnkStmtList:
  ##    # result.body = quote do: (`header`; discard)
  ##
  ##  proc fun*() {.transf.} =
  ##    ## first comment
  ##    runnableExamples: discard
  ##    runnableExamples: discard
  ##    ## last comment
  ##    discard # first statement after doc comments + runnableExamples
  ##    ## not docgen'd

  result = newStmtList()
  for ni in n:
    case ni.kind
    of nnkCommentStmt:
      result.add ni
    of nnkCall, nnkCommand:
      if ni[0].kind == nnkIdent and ni[0].eqIdent "runnableExamples":
        result.add ni
      else: break
    else: break

macro stamp*(body: untyped): NimNode =
  ## Accepts a template body, immediately applies it, and returns the resulting AST.
  ##
  ## Identifiers within `body` are bound to symbols from the caller's scope in
  ## the same fashion as a template. As a special case, `result` is excluded
  ## from automatic binding.
  ##
  ## The template body is hygienic, as such identifiers declared within might
  ## turn into `gensym` symbols. This behavior can be overridden using
  ## `{.gensym.}` or `{.inject.}` pragmas at declaration sites. Consult the
  ## language manual for details on template hygiene.
  ##
  ## Within `body`, placeholders, which are references to values in the caller's
  ## scope delimited by backticks, are substituted with the referenced values.
  runnableExamples:
    import std/strutils
    import std/times

    macro logQuote(msg: string) =
      ## Log the given message with timestamp
      # Using `quote` requires binding many symbols explicitly so that users
      # don't have to import the providers themselves
      let
        # Make sure that `$` is selected from this scope to get `$` for `DateTime`
        stringify = bindSym"$"
        # Bind to `now` so that users don't have to import times
        now = bindSym"now"
        # Bind to `strutils.%` so users don't have to import strutils
        format = bindSym"%"

      quote:
        echo `format`("[$1]\t$2", [stringify(`now`()), `msg`])

    macro log(msg: string) =
      ## Log the given message with timestamp
      # Using `stamp`, `echo`, `%`, `$` and `now` are bound automatically to
      # macro scope and users won't have to import times or strutils manually.
      stamp:
        echo "[$1]\t$2" % [$now(), `msg`]

  runnableExamples:
    import std/strutils
    import std/times

    when false:
      # `quote` yields AST as-is, as such declarations must be explicitly
      # gensym-ed or they might collide with something within the caller scope
      macro log(msg: string) =
        let
          # Make sure that `$` is selected from this scope to get `$` for `DateTime`
          stringify = bindSym"$"
          # Bind to `now` so that users don't have to import times
          now = bindSym"now"

        quote:
          let time = `stringify`(`now`())
          echo time, "\t", `msg`

      log("first")
      log("second") # <- error: `time` is redefined
    else:
      # `stamp`'s body is a template, and as such template gensym rules are
      # applied to declarations within
      macro log(msg: string) =
        stamp:
          let time = $now() # implicitly gensym-ed
          echo time, "\t", `msg`

      log("first")
      log("second") # All OK!

  runnableExamples:
    # `stamp` automatically binds to symbols within the caller scope, which can
    # introduce unexpected errors to correct-looking code.
    when false:
      macro log(msg: string) =
        result = newStmtList()

        let echo = newCall(bindSym"echo", newLit"== log")
        result.add echo

        result.add:
          stamp:
            echo `msg`
          # ^~~~ this binds to the `let echo` above instead of `system.echo` and
          #      will error when used.

      log("hi!") # this will error!

  var args: seq[NimNode]
  proc extract(n: NimNode, args: var seq[NimNode]): NimNode =
    ## Extract backticks-delimited expressions.
    case n.kind
    of nnkAccQuoted:
      result = ident("_" & $args.len)
      args.add n[0]
    else:
      for i in 0..<n.len:
        n[i] = extract(n[i], args)
      result = n

  let body = extract(body, args)

  var params = @[bindSym"untyped"]
  for i in 0..<args.len:
    params.add newIdentDefs(ident("_" & $i), bindSym"untyped")
  # Add result as a template parameter to prevent automatic binding
  params.add newIdentDefs(ident"result", bindSym"untyped")

  let name = genSym(nskTemplate, "stamped")
  # Prepend the callee
  args.insert(name, 0)
  # Explicitly bind result as an identifier
  args.add(newCall(bindSym"ident", newLit"result"))

  result = nnkStmtListExpr.newTree(
    newProc(name, params, body, nnkTemplateDef),
    nnkCall.newTree(
      bindSym"getAst",
      nnkCall.newTree(args)
    )
  )
