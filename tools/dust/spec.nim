## Key types relevant to dust's implementation, shared across various modules.

import
  std/sets,
  compiler/modules/modulegraphs,
  compiler/ast/[
    lineinfos,
    renderer,
    ast_types,
    ast_query,
  ]


type
  ErrorCode* = enum
    success = 0
    fileNotProvided = 1
    setupError = 2
    noError = 3

  DustContext* = ref object of PPassContext
    mainIndex*: FileIndex
    ignore*: bool


proc size*(n: PNode): int =
  assert not n.isNil
  result = 1
  if n.kind in nkWithSons:
    for child in items(n.sons):
      inc result, size(child)


export `$`
