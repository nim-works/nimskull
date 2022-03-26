discard """
  matrix: "--gc:arc"
  output: '''a
b
c'''
"""

# bug #18469

type
  Edge = object
    neighbor {.cursor.}: Node

  NodeObj = object
    neighbors: seq[Edge]
    label: string
    visited: bool
  Node = ref NodeObj

  Graph = object
    nodes: seq[Node]

proc `=destroy`(x: var NodeObj) =
  echo x.label
  `=destroy`(x.neighbors)
  `=destroy`(x.label)

proc addNode(self: var Graph; label: string): Node =
  self.nodes.add(Node(label: label))
  result = self.nodes[^1]

proc addEdge(self: Graph; source, neighbor: Node) =
  source.neighbors.add(Edge(neighbor: neighbor))

proc main =
  var graph: Graph
  let nodeA = graph.addNode("a")
  let nodeB = graph.addNode("b")
  let nodeC = graph.addNode("c")

  graph.addEdge(nodeA, neighbor = nodeB)
  graph.addEdge(nodeA, neighbor = nodeC)

main()
