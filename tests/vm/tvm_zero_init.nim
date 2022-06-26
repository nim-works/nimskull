discard """
  targets: "c cpp"
  matrix: "--gc:refc; --gc:arc"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/9622
      complex constant doesn't work at c++ backend
    . This is the VM being dumb.
      When an object is created by the VM all the slots are filled with
      their initial value: as you can see in the c code there are way too many
      zero-init values, that's what GCC is telling you.
    . https://github.com/nim-lang/Nim/issues/11226
      Initialization of types with conditional fields (nkRecWhen) differs
      in const and non-const contexts
  '''
"""

type
  GlobNodeKind = enum
    LiteralIdent,
    Group

  GlobNode = object
    case kind: GlobNodeKind
    of LiteralIdent:
      value: string
    of Group:
      values: seq[string]

  PathSegment = object
    children: seq[GlobNode]

  GlobPattern = seq[PathSegment]

proc parseImpl(): GlobPattern =
  if result.len == 0:
    result.add PathSegment()
  result[^1].children.add GlobNode(kind: LiteralIdent)

block:
  const pattern = parseImpl()
  doAssert $pattern == """@[(children: @[(kind: LiteralIdent, value: "")])]"""

