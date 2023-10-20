import
  compiler / modules / modulegraphs,
  compiler / ast / [ lineinfos, renderer, ast, astalgo, ]

import spec
import hashing

proc node(k: TNodeKind): PNode = PNode(kind: k)

type
  Transformer = proc (n: PNode): PNode

proc transform*(n: PNode; fn: Transformer): PNode =
  assert not n.isNil
  result = fn(n)
  if result.isNil:
    result = shallowCopy n
    for i, child in pairs(n):
      result.sons[i] = transform(child, fn)

proc rebuild*(n: PNode; fn: Transformer): PNode =
  assert not n.isNil
  result = fn(n)
  if cast[uint](result) == cast[uint](n):
    result = shallowCopy n
    if n.kind in nkWithSons:
      result.sons.setLen 0
      for child in items(n):
        let rebuilt = rebuild(child, fn)
        if not rebuilt.isNil:
          result.add rebuilt

proc removeIndex(n: PNode; index: int): PNode =
  var index = index
  let z = size(n)
  proc remover(n: PNode): PNode =
    if index == 0:
      result = nil
    else:
      result = n
    dec index
  result = rebuild(n, remover)

proc discardIndex(n: PNode; index: int): PNode =
  var index = index
  proc discarder(n: PNode): PNode =
    if index == 0:
      result = nkDiscardStmt.node
    else:
      dec index
  result = transform(n, discarder)

proc emptyIndex(n: PNode; index: int): PNode =
  var index = index
  proc emptier(n: PNode): PNode =
    if index == 0:
      result = nkEmpty.node
    else:
      dec index
  result = transform(n, emptier)

iterator mutations*(n: PNode): PNode =
  for i in 1 ..< size(n):
    yield removeIndex(n, i)
    yield emptyIndex(n, i)
    yield discardIndex(n, i)

proc init*(remains: var Remains; n: PNode) =
  let h = hashNode(n)
  if h notin remains:
    remains.add(n, h)