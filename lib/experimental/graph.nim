## Generic implementation of graph data structure

import std/[
  tables,
  strformat,
  deques,
  hashes,
  options,
  math,
  lenientops,
  strutils,
  sequtils,
  algorithm,
  intsets,
  sets
]

type
  GraphEdge*[Id] = tuple[src, dst: Id]

  Path*[Id] = seq[Id]
  Graph*[Id] = object
    isDirected*: bool
    edges: OrderedTable[Id, seq[Id]]

type
  GraphError* = ref object of CatchableError
  GraphCyclesError* = ref object of GraphError

func newGraph*[Id](isDirected: bool = true): Graph[Id] =
  Graph[Id](isDirected: isDirected)

func newGraph*[Id](
    values: openarray[(Id, Id)], isDirected: bool = true): Graph[Id] =
  result = newGraph[Id](isDirected)
  for (src, dst) in values:
    result.edges.mgetOrPut(src, @[]).add dst

func default*[Id](graph: typedesc[Graph[Id]]): Graph[Id] =
  newGraph[Id]()

func contains*[Id](graph: Graph[Id], node: Id): bool =
  node in graph.edges


func contains*[Id](graph: Graph[Id], edge: GraphEdge[Id]): bool =
  result = (
    ( edge.src in graph.edges and
      edge.dst in graph.edges[edge.src] ) or
    ( not graph.isDirected and
      edge.dst in graph.edges and
      edge.src in graph.edges[edge.dst] )
  )

func getEdge*[Id](graph: Graph[Id], source, target: Id): GraphEdge[Id] =
  if source.id in graph.structure.outgoingIndex:
    for outEdge in graph.structure.outgoingIndex[source.id]:
      if graph.structure.edgeMap[outEdge.id][1] == target:
        return outEdge

  raise newException(ValueError, "No edge for source/target pair")

func edgeCount*[Id](graph: Graph[Id]): int =
  for node, targets in pairs(graph.edges):
    result += targets.len()

func nodeCount*[Id](graph: Graph[Id]): int =
  graph.edges.len()

iterator nodes*[Id](graph: Graph[Id]): Id =
  for i, _ in pairs(graph.edges):
    yield i

proc allNodes*[Id](graph: Graph[Id]): seq[Id] =
  for node in nodes(graph):
    result.add node

iterator edgePairs*[Id](graph: Graph[Id]): GraphEdge[Id] =
  for node, targets in pairs(graph.edges):
    for target in targets:
      yield (node, target)

func computeInDegrees*[Id](graph: Graph[Id]): CountTable[Id] =
  for (src, dst) in edgePairs(graph):
    result.inc dst

func inDeg*[Id](graph: Graph[Id], node: Id): int {.inline.} =
  ## Return in degree for @arg{node}
  result = 0
  for (_, dst) in edgePairs(graph):
    if dst == node:
      inc result

func outDeg*[Id](graph: Graph[Id], node: Id): int =
  if node in graph.edges:
    return graph.edges[node].len()

func newId[Id](graph: var Graph[Id]): int {.inline.} =
  result = graph.structure.maxId
  inc graph.structure.maxId

proc removeNode*[Id](graph: var Graph[Id], node: Id) =
  ## Remove `node` from graph.
  ##
  ## Note that all edges that `node` is part of are removed too.
  graph.edges.del node
  for targets in mitems(graph.edges[node]):
    targets.excl node

proc removeEdge*[Id](graph: var Graph[Id], edge: GraphEdge[Id]) =
  ## Remove `edge` from graph. Note that starting/ending nodes will not be
  ## removed.
  if edge.src in graph.edges:
    graph.edges[edge.src].del graph.edges[edge.src].find(edge.dst)
  # QUESTION error on missing edge?

iterator outEdges*[Id](graph: Graph[Id], source: Id): GraphEdge[Id] =
  ## Iterate over outgoing edges for `source`
  if source in graph.edges:
    for it in graph.edges[source]:
      yield (source, it)

iterator inEdges*[Id](graph: Graph[Id], target: Id): GraphEdge[Id] =
  ## Iterate over ingoing edges for `target`
  for (src, dst) in graph.edgePairs():
    if dst == target:
      yield (src, src)

proc `$`*[Id](graph: Graph[Id]): string =
  var count: int = 0
  var map: seq[(string, string, string)]
  for edge in edgePairs(graph):
    map.add(($graph.sourceVal(edge), $graph[edge], $graph.targetVal(edge)))

  map = map.sortedByIt(it[0])
  var sourcew = 0
  for it in map: sourcew = max(sourcew, it[0].len)

  var edgew = 0
  for it in map: edgew = max(edgew, it[1].len)

  for idx, line in map:
    if 0 < idx:
      result.add "\n"

    result.add alignLeft(line[0], sourcew)
    result.add " -["
    result.add alignLeft(line[1], edgeW)
    result.add "]-> "
    result.add line[2]



iterator depthFirst*[Id](
    graph: Graph[Id], root: Id): tuple[id: Id, depth: int] =
  ## Perform depth-first iteration of **mutable** nodes in graph, starting
  ## from `node`
  var
    visited: HashSet[Id]
    stack: seq[seq[Id]]
    yielded: HashSet[Id]

  stack.add @[root]

  while 0 < len(stack) and 0 < len(stack[^1]):
    let top = stack[^1].pop

    if top notin yielded:
      yield (top, stack.len())
      yielded.incl top

    var buf: seq[Id]
    for (src, node) in graph.outEdges(top):
      if node notin visited:
        visited.incl node
        buf.add node

    if stack[^1].len() == 0:
      discard stack.pop()

    if 0 < len(buf):
      stack.add buf

iterator breadthFirst*[Id](graph: Graph[Id], root: Id): Id =

  ## Perform breadth-first iteration of parent graph, starting from `node`
  var que = initDeque[Id]()
  que.addLast(root)
  var visited: HashSet[Id]
  while 0 < len(que):
    let top = que.popFirst()
    yield top
    for (src, target) in graph.outEdges(top):
      if target notin visited:
        visited.incl target
        que.addLast(target)


proc getNoIncoming*[Id](graph: Graph[Id]): seq[Id] =
  ## Get all nodes with zero in degree of zero
  let degrees = computeInDegrees(graph)
  for node in graph.nodes:
    if degrees[node] == 0:
      result.add node

proc topologicalOrdering*[Id](
    graph: Graph[Id], start: openarray[Id] = []): seq[Id] =
  ## Return graph nodes in topological ordering if possible. Otherwise (if
  ## graph contains cycles raise `GraphCyclesError`)
  var active = toHashSet(start)
  if active.len() == 0:
    active = toHashSet(getNoIncoming(graph))

  var graph = graph

  while 0 < len(active):
    let node = active.pop
    result.add node
    for (src, target) in graph.outEdges(node):
      graph.removeEdge((src, target))
      if graph.inDeg(target) == 0:
        active.incl target

  if 0 < graph.edgeCount:
    raise GraphCyclesError(
      msg: "Topological sorting is impossible, graph contains cycles")


template findMaxIt*(s: untyped, op: untyped): untyped =
  var res: int = 0
  var idx = 0
  var prev: typeof((let it {.inject.} = s[0]; op))

  for it {.inject.} in s:
    let val = op
    if val > prev:
      res = idx

    inc idx

  res

template findIt*(s: typed, op: untyped): int =
  ## Find first element of the sequence for which `op` evaluates as true
  ## and return it's index. If no such element is found return `-1`

  var result = -1
  for idx, it {.inject.} in s:
    if op: result = idx; break

  result


proc graphEasyRepr*[Id](
    graph: Graph[Id], name: proc(id: Id): string): string =

  for (src, dst) in edgePairs(graph):
    result.add "[ $# ] $# [ $# ]\n" % [
      name(src),
      "->",
      name(dst)
    ]

proc findCycles*[Id](
    graph: Graph[Id],
    ignoreSelf: bool = false,
    overrideDirected: bool = false,
    yieldMultiedge: bool = false
  ): seq[Path[Id]] =

  var
    visited: HashSet[Id]
    stack: seq[Id]
    resCycles: seq[Path[Id]]

  proc printCycle(
      vertex: Id, stack: var seq[Id], resCycles: var seq[Path[Id]]) =
    var stack2: seq[Id]
    stack2.add(stack.pop())

    while stack2[^1] != vertex:
      stack2.add(stack.pop())

    var path: seq[Id]
    while 0 < len(stack2):
      path.add stack2[^1]
      stack.add(stack2.pop())

    resCycles.add path

  proc processDfs() =
    let top = stack[^1]
    template body(vertex: untyped): untyped {.dirty.} =
      if vertex == top and ignoreSelf:
        discard

      elif vertex in stack:
        printCycle(vertex, stack, resCycles)

      elif vertex notin visited:
        stack.add vertex
        processDFS()


    for (_,vertex) in outEdges(graph, top):
      body(vertex)

    if overrideDirected:
      for (_, vertex) in inEdges(graph, top):
        body(vertex)

    visited.incl stack.pop()

  for vertex in nodes(graph):
    if vertex notin visited:
      stack.add vertex
      processDfs()

  return resCycles


proc mergeCycleSets*[Id](cycles: seq[seq[Id]]): seq[HashSet[Id]] =
  for cycle in cycles:
    let cycle = cycle.toHashSet()
    var merged = false
    for nodeSet in mitems(result):
      if 0 < len(cycle * nodeSet):
        nodeSet.incl cycle
        merged = true

    if not merged:
      result.add cycle

proc connectedComponents*[Id](
    graph: Graph[Id],
    overrideDirected: bool = false
  ): seq[HashSet[Id]] =
  ## Return nodes forming strongly connected components
  type
    Node = Id
    IdTable = Table[Id, int]

  var time: int

  proc aux(
    vertex: Node, disc: var IdTable, low: var IdTable,
    stack: var seq[Node], stackMember: var HashSet[Id],
    components: var seq[HashSet[Id]]
  ) =

    inc time
    disc[vertex] = time
    low[vertex] = time
    stack.add vertex
    stackMember.incl vertex

    template impl(outVertex: untyped): untyped =
      if outVertex notin disc:
        aux(outVertex, disc, low, stack, stackMember, components)
        low[vertex] = min(low[vertex], low[outVertex])

      elif outVertex in stackMember:
        low[vertex] = min(low[vertex], disc[outVertex])


    for (_, outVertex) in graph.outEdges(vertex):
      impl(outVertex)

    if overrideDirected:
      for (_, outVertex) in graph.inEdges(vertex):
        impl(outVertex)

    if (low[vertex] == disc[vertex]):
      components.add HashSet[Id]()
      while stack[^1] != vertex:
        let w = stack.pop()
        components[^1].incl w
        stackMember.excl w

      let w = stack.pop()
      components[^1].incl w
      stackMember.excl w

  var
    disc = initTable[Id, int]()
    low = initTable[Id, int]()
    stackMember: HashSet[Id]
    stack = newSeq[Node]()

  for node in nodes(graph):
    if node notin disc:
      aux(node, disc, low, stack, stackMember, result)

iterator ritems*[T](s: openarray[T]): T =
  ## Iterate over sequence starting from the right
  for idx in countdown(s.len - 1, 0):
    yield s[idx]
