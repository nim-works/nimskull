## Non-essential routines related to the code-generator IR.

import
  std/[
    strutils
  ],
  compiler/ast/[
    ast_types,
    typesrenderer
  ],
  compiler/backend/[
    cgir
  ]

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
    of cnkField:
      result.add "field: "
      result.add n.field.name.s
      result.add " id: "
      result.add $n.field.itemId
    of cnkProc:
      result.add "prc: "
      result.addInt n.prc.int
    of cnkConst:
      result.add "cnst: "
      result.addInt n.cnst.int
    of cnkGlobal:
      result.add "global: "
      result.addInt n.global.int
    of cnkLabel:
      result.add "label: "
      result.addInt n.label.int
    of cnkLocal:
      result.add "local: "
      result.add $n.local.int
    of cnkMagic:
      result.add "magic: "
      result.add $n.magic
    of cnkEmpty, cnkInvalid, cnkType, cnkAstLit, cnkNilLit, cnkReturnStmt:
      discard
    of cnkWithOperand:
      result.add "\n"
      result.add repeat("  ", indent)
      treeRepr(n.operand, indent+1, result)
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
