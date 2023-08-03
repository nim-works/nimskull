## Non-essential code-generator IR related routines.

import
  std/[
    strutils
  ],
  compiler/ast/[
    ast_types,
    ast_query,
    typesrenderer
  ],
  compiler/backend/[
    cgir
  ],
  compiler/utils/[
    idioms
  ]

from compiler/ast/renderer import renderTree, TRenderFlag

proc treeRepr*(n: CgNode): string =
  ## Renders the tree representation of `n` to text.
  proc treeRepr(n: CgNode, indent: int, result: var string) {.nimcall.} =
    result.add $n.kind
    result.add " "
    if n.typ != nil:
      result.add "typ: "
      result.add $n.typ
      result.add " "

    case n.kind
    of cnkIntLit:
      result.add "intVal: "
      result.add $n.intVal
    of cnkUIntLit:
      result.add "uintVal: "
      result.add $cast[BiggestUInt](n.intVal)
    of cnkFloatLit:
      result.add "floatVal: "
      result.add $n.floatVal
    of cnkStrLit:
      result.add "strVal: \""
      result.add n.strVal
      result.add "\""
    of cnkPragmaStmt:
      result.add "pragma: "
      result.add $n.pragma
    of cnkSym:
      result.add "sym: "
      result.add n.sym.name.s
      result.add " id: "
      result.add $n.sym.itemId
    of cnkEmpty, cnkInvalid, cnkType, cnkAstLit, cnkNilLit, cnkReturnStmt:
      discard
    of cnkWithItems:
      result.add "\n"
      for i in 0..<n.len:
        if i > 0:
          result.add "\n"
        result.add repeat("  ", indent)
        result.add $i
        result.add ": "
        treeRepr(n[i], indent+1, result)

  treeRepr(n, 0, result)

type
  RenderCtx = object
    syms: seq[PSym]
      ## remembers the already-rendered symbols. Used to provide unique names.

proc disambiguate(c: var RenderCtx, s: PSym): int =
  ## Computes and returns a number to append to the symbol name in order to
  ## make it unique in the output. This way, multiple different symbols sharing
  ## the same name can be disambiguated.
  result = 0
  for it in c.syms.items:
    if it == s:
      return
    elif it.name.id == s.name.id: # same name?
      inc result

  c.syms.add s # remember the symbol

proc render(c: var RenderCtx, ind: int, n: PNode, res: var string) =
  template add(s: var string, n: PNode) =
    render(c, ind, n, res)

  template indent(extra = 1) =
    if res.len > 0 and res[^1] == ' ':
      # remove trailing space
      res.setLen(res.len - 1)
    res.add "\n"
    res.add repeat("  ", ind + extra)

  template newLine() =
    indent(0)

  template renderList(n: PNode, sep: untyped; start: int = 0; fin: int = 0) =
    ## Renders the items in the slice ``start..<n.len - fin``
    for i in start..<n.len-fin:
      if i > start:
        sep
      res.add n[i]

  template renderList(n: PNode, sep: string; start: int = 0; fin: int = 0) =
    ## Renders the items in the slice ``start..<n.len - fin``
    renderList(n, res.add(sep), start, fin)

  case n.kind
  of nkIntLit..nkInt64Lit:
    res.addInt n.intVal
  of nkCharLit, nkUIntLit..nkUInt64Lit:
    res.addInt cast[BiggestUInt](n.intVal)
  of nkFloatLit..nkFloat128Lit:
    res.add $n.floatVal
  of nkStrLiterals:
    res.add '"'
    res.add n.strVal
    res.add '"'
  of nkNilLit:
    res.add "nil"
  of nkNimNodeLit:
    res.add "<ast>"
  of nkSym:
    res.add n.sym.name.s
    let postfix = disambiguate(c, n.sym)
    if postfix > 0 and n.sym.magic == mNone:
      res.add "_" & $postfix

    # the rendered code is currently used for the ``--expandArc``, so we also
    # highlight cursor locals
    if sfCursor in n.sym.flags:
      res.add "_cursor"
  of nkType:
    if n.typ.sym != nil:
      res.add $n.typ
    else:
      res.add "[type node]"
  of nkCheckedFieldExpr, nkHiddenAddr, nkHiddenDeref, nkHiddenStdConv, nkChckRange, nkChckRange64, nkChckRangeF:
    res.add n[0]
  of nkAddr:
    res.add "addr "
    res.add n[0]
  of nkDerefExpr:
    res.add n[0]
    res.add "[]"
  of nkDotExpr:
    res.add n[0]
    res.add '.'
    res.add n[1]
  of nkBracketExpr:
    res.add n[0]
    res.add '['
    res.add n[1]
    res.add ']'
  of nkRange:
    res.add n[0]
    res.add ".."
    res.add n[1]
  of nkCast:
    res.add "cast["
    res.add $n[0].typ
    res.add "]("
    res.add n[1]
    res.add ")"
  of nkStringToCString:
    res.add "cstring("
    res.add n[0]
    res.add ')'
  of nkCStringToString:
    res.add "string("
    res.add n[0]
    res.add ')'
  of nkConv:
    res.add n[0]
    res.add '('
    res.add n[1]
    res.add ')'
  of nkObjUpConv, nkObjDownConv:
    res.add $n.typ
    res.add "("
    res.add n[0]
    res.add ")"
  of nkExprColonExpr:
    res.add n[0]
    res.add ": "
    res.add n[1]
  of nkCall:
    res.add n[0]
    res.add '('
    let ind = ind + 1
    renderList(n, ", ", 1)
    res.add ')'

  of nkObjConstr:
    res.add $n[0].typ
    res.add '('
    renderList(n, ", ", 1)
    res.add ')'
  of nkTupleConstr, nkClosure:
    res.add '('
    renderList(n, ", ")
    res.add ')'
  of nkBracket:
    res.add '['
    renderList(n, ", ")
    res.add ']'
  of nkCurly:
    res.add '{'
    renderList(n, ", ")
    res.add '}'

  of nkAsgn, nkFastAsgn:
    res.add n[0]
    res.add " = "
    let ind = ind + 1
    res.add n[1]
  of nkVarSection, nkLetSection:
    for i, def in n.pairs:
      if i > 0:
        newLine()
      res.add "var "
      res.add def[0]
      if def[2].kind != nkEmpty:
        res.add " = "
        let ind = ind + 1
        res.add def[2]
  of nkPragma:
    # re-use the ``PNode`` rendering
    res.add renderTree(n, {renderIr, renderNoComments})
  of nkReturnStmt:
    res.add "return"
  of nkDiscardStmt:
    res.add "discard "
    res.add n[0]
  of nkBreakStmt:
    if n[0].kind == nkEmpty:
      res.add "break"
    else:
      res.add "break "
      res.add n[0]
  of nkRaiseStmt:
    if n[0].kind == nkEmpty:
      res.add "raise"
    else:
      res.add "raise "
      res.add n[0]
  of nkAsmStmt:
    res.add "asm "
    let ind = ind + 1
    renderList(n, ", ")
    res.add ""
  of nkWhileStmt:
    res.add "while true:"
    indent()
    render(c, ind + 1, n[1], res)
  of nkBlockStmt:
    if n[0].kind == nkEmpty:
      res.add "block:"
    else:
      res.add "block "
      res.add n[0]
      res.add ":"
    indent()
    render(c, ind + 1, n[1], res)
  of nkIfStmt:
    res.add "if "
    res.add n[0][0]
    res.add ':'
    indent()
    render(c, ind + 1, n[0][1], res)
  of nkCaseStmt:
    res.add "case "
    res.add n[0]
    for i in 1..<n.len:
      newLine()
      case n.kind
      of nkOfBranch:
        res.add "of "
        renderList(n[i], ", ", 1, 1)
      of nkElse:
        res.add "else"
      else:
        unreachable()

      res.add ":"
      indent()
      render(c, ind + 1, n[i][^1], res)
  of nkTryStmt:
    res.add "try:"
    indent()
    render(c, ind + 1, n[0], res)
    for i in 1..<n.len:
      case n[i].kind
      of nkExceptBranch:
        newLine()
        res.add "except:"
        indent()
        render(c, ind + 1, n[i][^1], res)
      of nkFinally:
        newLine()
        res.add "finally:"
        indent()
        render(c, ind + 1, n[i][0], res)
      else:
        unreachable()
  of nkStmtListExpr:
    newLine()
    renderList(n, newLine())
  of nkStmtList:
    renderList(n, newLine())
  of nkWithSons + nkWithoutSons - codegenExprNodeKinds -
     {nkExprColonExpr, nkRange} + {nkEmpty}:
    unreachable(n.kind)

proc render*(n: PNode): string =
  ## Renders `n` to human-readable code that tries to emulate the shape of the
  ## high-level language. The output is meant for debugging and tracing and is
  ## not guaranteed to have a stable format.
  var c = RenderCtx()
  render(c, 0, n, result)