## This module implements the structures for associating ``MirNode``s with a
## ``PNode`` origin.
##
## Information about which ``PNode`` a ``MirNode`` originated from is needed
## for source-code position information and other reporting-related tasks.
##
## Origin information doesn't affect the semantics of MIR code and is thus
## stored and implemented separately. This allows for using a different way
## of storing such information without requiring adjustments to the core MIR
## data structures.

import
  compiler/ast/[
    ast_types
  ],
  compiler/mir/[
    mirtrees
  ],
  compiler/utils/[
    containers
  ]

type
  SourceId* = distinct range[0'u32 .. high(uint32)-1]
    ## The local ID of a source-mapping. The IDs are not unique across multiple
    ## ``SourceMap``s

  SourceMap* = object
    ## Associates each ``MirNode`` with the ``PNode`` it originated from
    source*: Store[SourceId, PNode]
      ## stores the ``PNode``s used as the source/origin information
    map*: seq[SourceId]
      ## stores the ``SourceId`` for each ``MirNode``. The connection
      ## happens via the index, that is, indexing `map` with a ``NodeIndex``
      ## yields the ``SourceId`` attached to the node with the given
      ## index. For this to work, `map` is required to always have the same
      ## size as the associated ``MirNode`` seq

# make ``SourceId`` available to be used with ``OptIndex``:
template indexLike*(_: typedesc[SourceId]) = discard

func `==`*(a, b: SourceId): bool {.borrow.}

# ------- ``SourceMap``-related routines:

func `[]`*(m: SourceMap, i: NodeInstance): SourceId {.inline.} =
  m.map[ord(i)]

func sourceFor*(m: SourceMap, i: NodeInstance): PNode {.inline.} =
  m.source[m.map[ord(i)]]