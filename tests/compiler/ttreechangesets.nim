discard """
  description: "Tests for the MIR ``TreeChangeset`` API"
  targets: native
"""

# XXX: the test cases would benefit from a mini-DSL

import
  compiler/ast/ast_types,
  compiler/mir/[mirtrees, treechangesets, mirconstr, sourcemaps],
  compiler/utils/containers

type Changeset = TreeChangeset

proc temp(x: int): MirNode =
  MirNode(kind: mnkTemp, temp: x.TempId)

func `==`(a: TempId, b: int): bool =
  a.int == b

func insert(c: var Changeset, i: int, n: sink MirNode) =
  # inherit the origin information from node 0
  c.insert(NodePosition i, n)

func `==`(a, b: MirNode): bool =
  if a.kind != b.kind:
    return false

  # only implements the comparisons needed by the tests
  case a.kind
  of mnkTemp:
    result = a.temp == b.temp
  else:
    doAssert false

func setupSourceMap(tree: MirTree): SourceMap =
  ## Sets up a minimal ``SourceMap`` instance for the given tree
  discard result.add(PNode())

template test(input, output: typed, body: untyped) =
  ## Helper template to simplify writing test cases
  block:
    var
      tree =
        when input is array: @input
        else:                input
      c {.inject.}: Changeset
      sourceMap = setupSourceMap(tree)

    template replace(c: Changeset, i: int, n: MirNode) {.inject.} =
      # for convenience
      replace(c, tree, NodePosition i, n)

    template remove(c: Changeset, i: int) {.inject.} =
      # for convenience
      remove(c, tree, NodePosition i)

    body

    apply(tree, prepare(c))
    {.line.}:
      doAssert tree == output

# ------- beginning of tests --------

block insert_same_position:
  ## If there exists more than one 'insert' operation for a position, they're
  ## applied in recording-order
  test([temp(0), temp(1), temp(2)],
       [temp(3), temp(4), temp(5), temp(6), temp(1), temp(2)]):
    c.insert  1, temp(4)
    c.insert  1, temp(5)
    c.replace 0, temp(3)
    c.insert  1, temp(6)

block insert_at_end:
  test([temp(0), temp(1)],
       [temp(0), temp(1), temp(2)]):
    c.insert 2, temp(2)

block insert_replace_same_position:
  # Replacing a node and inserting at the same position has to work. The
  # insertion always happens first...
  test([temp(0), temp(1), temp(2)],
       [temp(0), temp(3), temp(4), temp(2)]):
    c.replace 1, temp(4)
    c.insert  1, temp(3)

  # ... independent of the order
  test([temp(0), temp(1), temp(2)],
       [temp(0), temp(3), temp(4), temp(2)]):
    c.insert  1, temp(3)
    c.replace 1, temp(4)

block insert_remove_same_position:
  # Similar to the test above, but uses ``remove`` instead of ``replace``
  test([temp(0), temp(1), temp(2)],
       [temp(0), temp(3), temp(2)]):
    c.remove 1
    c.insert 1, temp(3)

  test([temp(0), temp(1), temp(2)],
       [temp(0), temp(3), temp(2)]):
    c.insert 1, temp(3)
    c.remove 1

block insert_shared_start:
  # ``replace``/``remove`` operations are allowed to share their start node
  # with an ``insert`` operation. The ``insert`` operation takes place *first*,
  # independent of the order in which they're recorded
  var bu: MirBuilder
  bu.add temp(0)
  bu.subTree mnkStmtList: discard
  bu.add temp(3)
  var tree = finish(bu)

  test(tree, [temp(0), temp(1), temp(2), temp(3)]):
    c.replace 1, temp(2)
    c.insert  1, temp(1)

  test(tree, [temp(0), temp(1), temp(2), temp(3)]):
    c.insert  1, temp(1)
    c.replace 1, temp(2)

  test(tree, [temp(0), temp(1), temp(3)]):
    c.remove 1
    c.insert 1, temp(1)

  test(tree, [temp(0), temp(1), temp(3)]):
    c.insert 1, temp(1)
    c.remove 1

block insert_shared_end:
  # ``replace``/``remove`` operations are allowed to share their end node
  # with an ``insert`` operation. The ``insert`` operation takes place
  # *second*, independent of the order in which they're recorded
  var bu: MirBuilder
  bu.add temp(0)
  bu.subTree mnkStmtList: discard
  bu.add temp(3)
  var tree = finish(bu)

  test(tree, [temp(0), temp(1), temp(2), temp(3)]):
    c.replace 1, temp(1)
    c.insert  3, temp(2)

  test(tree, [temp(0), temp(1), temp(2), temp(3)]):
    c.insert  3, temp(2)
    c.replace 1, temp(1)

  test(tree, [temp(0), temp(2), temp(3)]):
    c.remove 1
    c.insert 3, temp(2)

  test(tree, [temp(0), temp(2), temp(3)]):
    c.insert 3, temp(2)
    c.remove 1