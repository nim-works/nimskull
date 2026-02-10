import std/sets

import
  compiler / modules / modulegraphs,
  compiler / ast / [ lineinfos, renderer, ast_types, ast_query ]

type
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
