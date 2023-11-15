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
    renderer
  ],
  compiler/mir/[
    mirtrees
  ],
  compiler/utils/[
    idioms
  ]

# TODO: improve the code

func appendLine(s: var string, items: varargs[string, `$`]) =
  for x in items:
    s.add x
  s.add "\n"

func `$`(n: MirNode): string =
  result.add "(kind: "
  result.add $n.kind
  case n.kind
  of mnkProc, mnkConst, mnkGlobal, mnkParam, mnkLocal:
    result.add ", sym: "
    result.add $n.sym.name.s
  of mnkField, mnkPathNamed, mnkPathVariant:
    result.add ", field:"
    result.add $n.field.name.s
  of mnkLiteral:
    result.add ", lit: "
    {.cast(noSideEffect).}:
      result.add renderTree(n.lit)
  of mnkTemp, mnkAlias:
    result.add ", temp: "
    result.add $ord(n.temp)
  of mnkPathPos:
    result.add ", position: "
    result.add $n.position
  of mnkCall:
    result.add ", effects: "
    result.add $n.effects
  of mnkMagic:
    result.add ", magic: "
    result.add $n.magic
  of mnkOpParam:
    result.add ", param: "
    result.add $n.param
  of mnkBlock, mnkBreak:
    result.add ", block: "
    result.add $ord(n.label)
  of mnkEnd:
    result.add ", start: "
    result.add $n.start
  of mnkPNode:
    result.add ", node: "
    result.add $n.node.kind
  of mnkTag:
    result.add ", effect: "
    result.add $n.effect
  else:
    result.add ", len: "
    result.add $n.len

  if n.typ != nil:
    result.add ", typ: "
    result.add $n.typ.kind

  result.add ")"

proc print(result: var string, nodes: MirNodeSeq, indent: int, num: int, i: var uint32) =
  if i >= nodes.len.uint32:
    result.appendLine "error: missing node"
    return

  let n {.cursor.} = nodes[i]
  inc i

  result.appendLine repeat("  ", indent), num, ": ", n

  case n.kind
  of SubTreeNodes:
    var sub = 0
    while true:
      if i >= nodes.len.uint32:
        result.appendLine repeat("  ", indent), "out of bounds: end expected for ", n.kind
        break
      elif nodes[i].kind == mnkEnd:
        if nodes[i].start == n.kind:
          inc i
          break
        else:
          result.appendLine repeat("  ", indent+1), "loose end: ", nodes[i].start
          inc i

      else:
        print(result, nodes, indent+1, sub, i)

      inc sub

  of AtomNodes - {mnkEnd}:
    discard
  of mnkEnd:
    unreachable()

proc print*(nodes: MirTree, start = NodeIndex(0)): string =
  var i = start.uint32
  print(result, nodes, 0, 0, i)

  while i < nodes.len.uint32:
    result.appendLine "dangling node/sub-tree:"
    print(result, nodes, 0, 0, i)