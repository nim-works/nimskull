import std/unittest
import experimental/graph
import std/osproc
import std/sets
import std/algorithm
import std/sequtils

## tests for a generic graph data structure

type Id = int  ## generic graph will use this type for testing

proc arrangeRepr(graph: Graph[Id]) =
  let easy = graphEasyRepr(graph, proc(id: Id): string = $id)
  echo execCmdEx("graph-easy", input = easy).output

func g(nodes: openarray[(Id, Id)]): Graph[Id] = newGraph[Id](nodes)
func sort(nodes: openarray[(Id, Id)]): seq[Id] = g(nodes).topologicalOrdering()
func cycles(nodes: openarray[(Id, Id)]): seq[seq[Id]] = g(nodes).findCycles()
func dfs(nodes: openarray[(Id, Id)], root: int = -1): seq[Id] =
  let gr = g(nodes)
  for (node, depth) in depthFirst(
    gr, if root != -1: root else: getNoIncoming(gr)[0]):
    result.add node

func bfs(nodes: openarray[(Id, Id)], root: int = -1): seq[Id] =
  let gr = g(nodes)
  for node in breadthFirst(
    gr, if root != -1: root else: getNoIncoming(gr)[0]):
    result.add node


func `<`(s1, s2: seq[Id]): bool =
  var
    i1 = 0
    i2 = 0

  while i1 < s1.len() and i2 < s2.len():
    if s1[i1] < s2[i2]:
      return true

    elif s2[i2] < s1[i1]:
      return false

    else:
      inc i1
      inc i2

  if i1 == s1.len() and i2 < s2.len():
    return true

  else:
    return false

func connected(nodes: openarray[(Id, Id)]): seq[seq[Id]] =
  for comp in g(nodes).connectedComponents():
    var tmp: seq[Id]
    for it in comp:
      tmp.add it
    result.add sorted(tmp)

  return result.sorted()

check dfs({0: 1}) == @[0, 1]
check dfs({0: 1, 1: 0}, 0) == @[0, 1]
# Using ordered tables and `seq[]` so graph does not loose information
# about node insertion order. This helps stable testing and makes graph
# structure easyer to understand.
check dfs({0: 1, 1: 2, 2: 3, 1: 4}, 0) == @[0, 1, 4, 2, 3]
check dfs({0: 1, 1: 4, 1: 2, 2: 3}, 0) == @[0, 1, 2, 3, 4]
check dfs({0: 1, 1: 2, 2: 3, 3: 4}) == @[0, 1, 2, 3, 4]
block:
  let v = {
    0: 1,
      1: 3,
      1: 4,
    0: 2,
      2: 5,
      2: 6}

  check bfs(v) == @[0, 1, 2, 3, 4, 5, 6]
  check dfs(v) == @[0, 2, 6, 5, 1, 4, 3]

check sort({0: 1, 1: 2, 2: 3}) == @[0, 1, 2, 3]
check sort({3: 2, 2: 1, 1: 0}) == @[3, 2, 1, 0]

expect GraphCyclesError:
  discard sort({0: 1, 1: 0})

check cycles({0: 1, 1: 0}) == @[@[0, 1]]
check cycles({1: 0, 0: 1}) == @[@[1, 0]]
check cycles({1: 0}).len() == 0

check connected(
  {0: 1, 1: 2, 2: 3, 2: 4, 3: 0, 4: 2})[0] == @[0, 1, 2, 3, 4]

check connected({0: 1}) == @[@[0], @[1]]
check connected({0: 1, 1: 2, 2: 3}) == @[@[0], @[1], @[2], @[3]]
check connected({0: 1, 1: 0, 1: 2}) == @[@[0, 1], @[2]]
