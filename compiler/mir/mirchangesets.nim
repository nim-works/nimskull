## Implements the `Changeset <#Changeset>`_ type, which is a changeset for
## `MirBody <mirbodies.html#MirBody>`_. It builds upon/extends
## `TreeChangeset <treechangesets.html#TreeChangeset>`_.

import
  compiler/mir/[
    mirbodies,
    mirconstr,
    mirtrees,
    sourcemaps,
    treechangesets
  ],
  compiler/utils/[
    containers
  ]

type
  Changeset* = object
    ## Represents a set of changes to be applied to a ``MirBody``.
    inner: TreeChangeset
    locals: PartialStore[LocalId, Local]
      ## new locals to be added on changeset application

# ----------------------------------------
# proxy routines

template replace*(c: var Changeset, tree: MirTree, at: NodePosition,
                  with: MirNode) =
  ## Same as `replace <treechangesets.html#replace,TreeChangeset,MirTree,NodePosition,sinkMirNode>`_.
  replace(c.inner, tree, at, with)

template changeTree*(c: var Changeset, tree: MirTree, at: NodePosition,
                     with: MirNode) =
  ## Same as `changeTree <treechangesets.html#changeTree,TreeChangeset,MirTree,NodePosition,sinkMirNode>`_.
  changeTree(c.inner, tree, at, with)

template insert*(c: var Changeset, at: NodePosition, n: MirNode) =
  ## Same as `insert <treechangesets.html#insert,TreeChangeset,NodePosition,sinkMirNode>`_.
  insert(c.inner, at, n)

template remove*(c: var Changeset, tree: MirTree, at: NodePosition) =
  ## Same as `remove <treechangesets.html#remove,TreeChangeset,MirTree,NodePosition>`_.
  remove(c.inner, tree, at)

# ----------------------------------------
# `Changeset`-specific routines

func initChangeset*(body: MirBody): Changeset =
  ## Sets up a changeset for `body`. The changeset either needs to be
  ## discarded, or applied to the same ``MirBody`` instance it was created for.
  result = Changeset(locals: fork(body.locals))

func initBuilder(c: var Changeset, buffer: var MirNodeSeq,
                 info: SourceId): MirBuilder =
  ## Internal routine for setting up a builder. Must be paired with a
  ## ``finishBuilder`` call.
  result = initBuilder(info, move buffer)
  swap(c.locals, result.locals)

func finishBuilder(c: var Changeset, buffer: var MirNodeSeq,
                   bu: sink MirBuilder) =
  # move the ID counter and buffer back into the changeset
  (buffer, c.locals) = finish(bu)

template insert*(c: var Changeset, tree: MirTree, at, source: NodePosition,
                 name: untyped, body: untyped) =
  ## Records an insertion at the `at` position. For building the new tree,
  ## a ``MirBuilder`` instance is made available to the provided `body` via
  ## an injected local of name `name`. `source` identifies the node to
  ## inherit source information from.
  insert(c.inner, at, bufferTmp):
    var name = initBuilder(c, bufferTmp, tree[source].info)
    body
    finishBuilder(c, bufferTmp, name)

template replaceMulti*(c: var Changeset, tree: MirTree, at: NodePosition,
                       name, body: untyped) =
  ## Records a replacement of the node or sub-tree at the `at` position. For
  ## building the replacement tree, a ``MirBuilder`` instance is made
  ## available to the provided `body` via an injected local of name `name`.
  let pos = at
  replaceMulti(c.inner, tree, pos, bufferTmp):
    var name = initBuilder(c, bufferTmp, tree[pos].info)
    body
    finishBuilder(c, bufferTmp, name)

func apply*(body: var MirBody, c: sink Changeset) =
  ## Applies the changeset `c` to `body`.
  apply(body.code, prepare(move c.inner))
  join(body.locals, move c.locals)
