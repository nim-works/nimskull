## This module implements the ``Changeset`` API, which is the main way of
## applying changes to a ``MirTree``.
##
## Instead of modifying a ``MirTree`` directly, the changes (which can be
## insertions, replacements, or removals) are first recorded into a
## ``Changeset``. This allows for recording changes independent of each other
## concurrently and later apply the changes all at once.
##
## Before applying a ``Changeset`` to a ``MirTree``, it has to be prepared via
## a call to ``prepare`` first, after which the ``Changeset`` is sealed an no
## further changes can be recorded. ``prepare`` is responsible from normalizing
## the internal representation of the ``Changeset`` and is required for the
## later application to work.
##
## Applying the ``PreparedChangeset`` is done via ``apply``. This integrates
## all recorded changes into the applied to tree,.
##
## Order of application
## --------------------
##
## Nodes are inserted *before* the node at the insertion position, meaning that
## it is allowed for an insertion to overlap with a removal/replacement at the
## *start* position. All other forms of overlapping changes are disallowed.
## If there exist two or more insertion at the same position, they are applied
## in the order the changes were recorded. That is, the nodes from the insertion
## recorded first are inserted first, then that of the second recorded, then
## the third one's, etc.


import
  std/[
    algorithm
  ],
  compiler/mir/[
    mirtrees,
    mirconstr,
    sourcemaps
  ],
  compiler/utils/[
    idioms
  ]

type
  Row = object
    ## A changeset row. This is the building block of a changeset. Each row
    ## represents a modification, of which there are three kinds:
    ## - removal: `orig` has a length > 0 and `src` is empty
    ## - insertion: `orig` has a length of 0 and `src` hasn't
    ## - replacement: both `orig` and `src` have a length > 0
    orig: HOslice[NodeIndex] ## the slice of nodes this change affects
    src:  HOslice[NodeIndex] ## a slice in the buffer of staged nodes

  Changeset* = object
    ## Represents a set of changes to be applied to a ``MirTree``.
    nodes: seq[MirNode]
    rows: seq[Row]

    numTemps: uint32 ## the number of existing temporaries

  PreparedChangeset* = object
    nodes: seq[MirNode]
    rows: seq[Row]

    diff: int        ## the number of additions/removals
    stagingSize: int ## the minimum amount of nodes the working area must be
                     ## able to hold

func single[T](x: T): HOslice[T] {.inline.} =
  ## Utility for creating a slice with a single item
  HOslice[T](a: x, b: succ(x))

func span[T](a, b: T): HOslice[T] {.inline.} =
  HOslice[T](a: a, b: b)

func empty[T](x: typedesc[HOslice[T]]): HOslice[T] {.inline.} =
  HOslice[T](a: default(T), b: default(T))

func row(start, fin: NodePosition, src: HOslice[NodeIndex]): Row {.inline.} =
  ## Convenience constructor for ``Row``
  Row(orig: span(NodeIndex(start), NodeIndex(fin)),
      src: src)

func addSingle(s: var MirNodeSeq, n: sink MirNode): HOslice[NodeIndex] =
  s.add n
  result = single(s.high.NodeIndex)

func initChangeset*(tree: MirTree): Changeset =
  ## Initializes a new ``Changeset`` instance. Until the resulting
  ## ``Changeset`` is applied, the associated tree must not be modified.

  # count the number of existing temporaries:
  for i, n in tree.pairs:
    if n.kind in DefNodes and
       (let ent = child(tree, i, 0);
        ent.kind in {mnkTemp, mnkAlias}):
      result.numTemps = max(ent.temp.uint32 + 1, result.numTemps)

func replace*(c: var Changeset, tree: MirTree, at: NodePosition,
              with: sink MirNode) =
  ## Records replacing the node or sub-tree at `at` with `with`. The origin
  ## information is taken from the replaced node.
  let next = sibling(tree, at)
  with.info = tree[at].info
  c.rows.add row(at, next, c.nodes.addSingle(with))

func replaceSingle*(c: var Changeset, at: NodePosition, with: sink MirNode) =
  ## Replaces the single node at `at` with `with`.
  c.rows.add row(at, at+1, c.nodes.addSingle(with))

func insert*(c: var Changeset, at: NodePosition, n: sink MirNode) =
  ## Records the insertion of `n` at `at`. The ``info`` field on the node
  ## is not modified.
  c.rows.add row(at, at, c.nodes.addSingle(n))

func initBuilder(c: var Changeset, info: SourceId): MirBuilder =
  ## Internal routines for setting up a builder. Must be paired with a
  ## ``finishBuilder`` call.
  result = initBuilder(info, move c.nodes)
  swap(c.numTemps, result.numTemps)

func finishBuilder(c: var Changeset, bu: sink MirBuilder) =
  # move the ID counter and buffer back into the changeset
  swap(c.numTemps, bu.numTemps)
  c.nodes = finish(bu)

template insert*(c: var Changeset, tree: MirTree, at, source: NodePosition,
                 name: untyped, body: untyped) =
  ## Records an insertion at the `at` position, providing direct
  ## access to the internal node buffer inside `body` via an injected variable
  ## of the name `name`. `source` is the node to inherit the source/origin
  ## information from
  block:
    let
      start = c.nodes.len.NodeIndex
      # evaluate `at` and `source` before `body`, as the latter might change
      # what `source` evaluates to:
      pos = at
      info = tree[source].info

    var name = initBuilder(c, info)
    body
    finishBuilder(c, name)

    c.rows.add row(pos, pos, span(start, c.nodes.len.NodeIndex))

template replaceMulti*(c: var Changeset, tree: MirTree, at: NodePosition,
                       name, body: untyped) =
  ## Records a replacement of the node or sub-tree at the `at`
  ## position, providing direct access to the internal node buffer
  ## inside `body` via an injected variable of the name `name`.
  block:
    let
      start = c.nodes.len.NodeIndex
      pos = at # prevent double evaluation
      info = tree[pos].info
      next = sibling(tree, pos)

    var name = initBuilder(c, info)
    body
    finishBuilder(c, name)

    c.rows.add row(pos, next, span(start, c.nodes.len.NodeIndex))

func remove*(c: var Changeset, tree: MirTree, at: NodePosition) =
  ## Records the removal of the node or sub-tree at `at`.
  let next = sibling(tree, at)
  # a removal is recorded as replacing a slice with nothing
  c.rows.add row(at, next, empty(HOslice[NodeIndex]))

func prepare*(c: sink Changeset): PreparedChangeset =
  ## Prepares `c` for being applied

  # applying the changes can be done much more efficiently if they are ordered
  # by ascending modification position
  c.rows.sort proc(a, b: auto): int =
    result = a.orig.a.int - b.orig.a.int
    if result == 0:
      # use the end position as the second-order, so that insertions sharing
      # their start position with replacements/removals are applied first
      result = a.orig.b.int - b.orig.b.int

  # calcuate the difference in the amount of nodes plus the required size for
  # the working/staging area:
  for row in c.rows.items:
    result.diff -= row.orig.len
    result.diff += row.src.len
    result.stagingSize = max(result.stagingSize, result.diff)

  result.nodes = move c.nodes
  result.rows = move c.rows

func moveRight[T](x: var openArray[T], src, dest, len: Natural) =
  assert src < dest
  for i in countdown(len-1, 0):
    x[dest + i] = move x[src + i]

func moveLeft[T](x: var openArray[T], src, dest, len: Natural) =
  assert dest <= src
  for i in 0..<len:
    x[dest + i] = move x[src + i]

iterator apply[T](s: var seq[T], diff, stagingSize: int, rows: seq[Row]): tuple[row: lent Row, writePos: NodeIndex] =
  ## Performs the surrounding actions for integrating changes into `s`
  ## .. warning:: Exiting the iterator via ``return`` or ``break`` will leave
  ##              `s` in an inconsistent state
  let oldLen = s.len
  var
    writePos = rows[0].orig.a.int
      ## the start position of the "staging" area
    readPos = writePos
      ## the start position of the "original" area
    origPos = writePos
      ## keeps track of the logical position in the original sequeunce where
      ## we're copying from

  if stagingSize > 0:
    # make space for the temporary/staging partition
    let start = rows[0].orig.a.int
    s.setLen(oldLen + stagingSize)

    readPos = start + stagingSize
    moveRight(s, start, readPos, oldLen - start)

  assert writePos <= readPos

  # iterate over all changes and apply them. A change either adds, removes, or
  # replaces nodes.
  for i, row in rows.lpairs:
    let start = row.orig.a.int

    assert writePos <= readPos
    assert start >= origPos, "overlapping changes"

    # first move over the original nodes (if any):
    if start > origPos:
      # move the nodes from the "original" area to the "staging" area:
      let L = start - origPos
      if readPos != writePos:
        moveLeft(s, readPos, writePos, L)
      # commit the nodes to "done" area:
      readPos += L
      writePos += L
      origPos += L

    yield (row, writePos.NodeIndex)

    # skip the nodes we're replacing or removing:
    readPos += row.orig.len
    origPos += row.orig.len

    # commit:
    writePos += row.src.len

  assert origPos <= oldLen

  # move the remaining nodes:
  if oldLen > origPos and readPos != writePos:
    moveLeft(s, readPos, writePos, oldLen - origPos)

  if diff != stagingSize:
    # resize to the correct number of items
    s.setLen(oldLen + diff)

func apply*(tree: var MirTree, c: sink PreparedChangeset) =
  ## Applies the changeset `c` to the `tree`, modifying the tree in-place. The
  ## tree's underlying sequence is resized 0 to 2 times.
  if c.rows.len == 0:
    # nothing to do
    return

  for row, writePos in apply(tree, c.diff, c.stagingSize, c.rows):
    for p in row.src.items:
      tree[writePos + (p - row.src.a)] = move c.nodes[p]