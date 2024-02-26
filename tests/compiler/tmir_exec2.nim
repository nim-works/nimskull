discard """
  description: '''
    Tests for the behaviour of the traversal routines from ``mirexec.nim``
  '''
  target: native
"""

include compiler/sem/mirexec

# setup a very basic graph for testing:
var graph = DataFlowGraph(instructions:
  @[Instr(node: NodePosition 0, op: opDef),
    Instr(node: NodePosition 1, op: opUse)])

block forward_traverse_empty_slice:
  # ensure that the exit flag is not set to true when no instructions are
  # traversed
  let empty = graph.subgraphFor(NodePosition(4)..NodePosition(5))
  var s = TraverseState()
  for _ in traverse(graph, empty, empty.a, s):
    discard
  doAssert s.exit == false

  # the flag is also set to false if it was set to true externally
  s.exit = true
  for _ in traverse(graph, empty, empty.a, s):
    discard
  doAssert s.exit == false

block forward_traverse_abort_path_on_last:
  # aborting the main path on the very last operation of the traversed
  # subgraph must not lead to the exit flag being set
  let all = graph.subgraphFor(NodePosition(0)..NodePosition(1))
  var s = TraverseState()
  for op, _ in traverse(graph, all, all.a, s):
    if op == opUse:
      # abort the path on the last operation of the traversed subgraph
      s.exit = true

  doAssert s.exit == false
