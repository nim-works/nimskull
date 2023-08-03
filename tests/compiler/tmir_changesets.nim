discard """
  description: "Tests for the MIR ``Changeset`` API"
  targets: native
"""

# XXX: the test cases would benefit from a mini-DSL

import
  compiler/ast/ast_types,
  compiler/mir/[mirtrees, mirchangesets, mirconstr, sourcemaps],
  compiler/utils/containers

proc temp(x: int): MirNode =
  MirNode(kind: mnkTemp, temp: x.TempId)

func `==`(a: TempId, b: int): bool =
  a.int == b

func seek(c: var Changeset, i: int) =
  c.seek NodePosition(i)

func insert(c: var Changeset, n: sink MirNode) =
  c.insert(n, NodeInstance(0)) # inherit the origin information from node 0

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
  let id = result.source.add PNode()
  result.map.setLen(tree.len)

template test(input, output: typed, body: untyped) =
  ## Helper template to simplify writing test cases
  block:
    var
      tree =
        when input is array: @input
        else:                input
      c {.inject.} = initChangeset(tree)
      sourceMap = setupSourceMap(tree)

    body

    let pc = prepare(c, sourceMap)
    apply(tree, pc)
    {.line.}:
      doAssert tree == output

# ------- beginning of tests --------

block insert_same_position:
  ## If there exists more than one 'insert' operation for a position, they're
  ## applied in recording-order
  test([temp(0), temp(1), temp(2)],
       [temp(3), temp(4), temp(5), temp(6), temp(1), temp(2)]):
    c.seek 1
    c.insert temp(4)
    c.insert temp(5)
    c.seek 0
    c.replace temp(3)
    c.seek 1
    c.insert temp(6)

block insert_at_end:
  test([temp(0), temp(1)],
       [temp(0), temp(1), temp(2)]):
    c.seek 2
    c.insert temp(2)

block insert_replace_same_position:
  # Replacing a node and inserting at the same position has to work. The
  # insertion always happens first...
  test([temp(0), temp(1), temp(2)],
       [temp(0), temp(3), temp(4), temp(2)]):
    c.seek 1
    c.replace temp(4)
    c.seek 1 # `replace` moves the cursor, so we have to move it back
    c.insert temp(3)

  # ... independent of the order
  test([temp(0), temp(1), temp(2)],
       [temp(0), temp(3), temp(4), temp(2)]):
    c.seek 1
    c.insert temp(3)
    c.replace temp(4)

block insert_remove_same_position:
  # Similar to the test above, but uses ``remove`` instead of ``replace``
  test([temp(0), temp(1), temp(2)],
       [temp(0), temp(3), temp(2)]):
    c.seek 1
    c.remove()
    c.seek 1 # ``remove`` moves the cursor, so we have to move it back
    c.insert temp(3)

  test([temp(0), temp(1), temp(2)],
       [temp(0), temp(3), temp(2)]):
    c.seek 1
    c.insert temp(3)
    c.remove()

block insert_shared_start:
  # ``replace``/``remove`` operations are allowed to share their start node
  # with an ``insert`` operation. The ``insert`` operation takes place *first*,
  # independent of the order in which they're recorded
  var tree: MirTree
  tree.add temp(0)
  tree.subTree MirNode(kind: mnkStmtList): discard
  tree.add temp(3)

  test(tree, [temp(0), temp(1), temp(2), temp(3)]):
    c.seek 1
    c.replace temp(2)
    c.seek 1
    c.insert temp(1)

  test(tree, [temp(0), temp(1), temp(2), temp(3)]):
    c.seek 1
    c.insert temp(1)
    c.seek 1
    c.replace temp(2)

  test(tree, [temp(0), temp(1), temp(3)]):
    c.seek 1
    c.remove()
    c.seek 1
    c.insert temp(1)

  test(tree, [temp(0), temp(1), temp(3)]):
    c.seek 1
    c.insert temp(1)
    c.seek 1
    c.remove()

block insert_shared_end:
  # ``replace``/``remove`` operations are allowed to share their end node
  # with an ``insert`` operation. The ``insert`` operation takes place
  # *second*, independent of the order in which they're recorded
  var tree: MirTree
  tree.add temp(0)
  tree.subTree MirNode(kind: mnkStmtList): discard
  tree.add temp(3)

  test(tree, [temp(0), temp(1), temp(2), temp(3)]):
    c.seek 1
    c.replace temp(1)
    c.seek 3
    c.insert temp(2)

  test(tree, [temp(0), temp(1), temp(2), temp(3)]):
    c.seek 3
    c.insert temp(2)
    c.seek 1
    c.replace temp(1)

  test(tree, [temp(0), temp(2), temp(3)]):
    c.seek 1
    c.remove()
    c.seek 3
    c.insert temp(2)

  test(tree, [temp(0), temp(2), temp(3)]):
    c.seek 3
    c.insert temp(2)
    c.seek 1
    c.remove()