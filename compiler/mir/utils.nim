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
