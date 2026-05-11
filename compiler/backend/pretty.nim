## Implements pretty printers for CGIR AST.

import
  compiler/backend/[
    cgir2
  ]

proc len(n: CgNode): int = n.val.int

proc render*(m: CgModule, ast: Ast, n: NodeIndex, indent=0): string =
  ## Renders structurally sound AST (i.e., terminals are valid and child
  ## counts are proper) into a generic S-expression representation.
  var prefix = ""
  var res = ""
  proc aux(m: CgModule, ast: Ast, pos: var NodeIndex, br: bool) =
    let n = ast[pos]
    inc pos
    if isLeaf(n):
      if br:
        res.add ' '

      case n.kind
      of cnkBool:
        if n.val == 1: res.add "true"
        else:          res.add "false"
      of cnkInt:
        res.addInt m.unpackInt(n.val)
      of cnkFloat:
        res.addFloat m.unpackFloat(n.val)
      of cnkString:
        res.addQuoted m.get(n.val.StringId)
      of cnkType:
        res.add "(type "
        res.add m.get(n.val.StringId)
        res.add ")"
      of cnkProc:
        res.add "(proc "
        res.add m.get(n.val.StringId)
        res.add ")"
      of cnkLocal:
        res.add "(local "
        res.add m.get(n.val.StringId)
        res.add ")"
      of cnkGlobal:
        res.add "(global "
        res.add m.get(n.val.StringId)
        res.add ")"
      of cnkLabel:
        res.add "(label "
        res.addInt n.val
        res.add ")"
      of cnkDatum:
        res.add "(datum "
        res.addInt n.val
        res.add ")"
      else:
        unreachable()
    else:
      if br:
        res.add "\n"
        res.add prefix

      res.add "("
      # add the kind without the prefix:
      let str = $n.kind
      for i in 3..<str.len:
        res.add str[i]
      if len(n) > 0:
        prefix.add "  "
      for _ in 0..<len(n):
        aux(m, ast, pos, true)
      if len(n) > 0:
        prefix.setLen(prefix.len - 2)
      res.add ")"

  for _ in 0..<indent:
    prefix.add ' '

  var pos = n
  aux(m, ast, pos, false)
  result = move res
