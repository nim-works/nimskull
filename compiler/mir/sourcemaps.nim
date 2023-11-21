## This module implements a simple database for mapping MIR ``SourceId``s to
## non-critical meta-data (currently only a ``PNode``).
##
## Information about which ``PNode`` a ``MirNode`` originated from is needed
## for source-code position information and other reporting-related tasks.
##
## The meta-data doesn't affect the semantics of MIR code and it's thus
## stored and implemented separately. This allows for using a different way
## of storing such information without requiring adjustments to the core MIR
## data structures.

import
  std/[
    options
  ],
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
  SourceMap* = object
    ## Associates meta-data (currently only ``PNode``) with ``SourceId``s.
    ## The map is meant to be used together with ``MirTree``s.
    source: Store[SourceId, PNode]
      ## stores the ``PNode``s used as the source/origin information

func `[]`*(m: SourceMap, i: SourceId): PNode {.inline.} =
  m.source[i]

func add*(m: var SourceMap, origin: PNode): SourceId {.inline.} =
  ## Adds `origin` to the database and returns the ID through which it can
  ## be looked up again.
  m.source.add(origin)

func merge*(dst: var SourceMap, tree: var MirTree, src: sink SourceMap) =
  ## Merges all entries from `src` into `dst` and updates all references in
  ## `tree`.
  let off = merge(dst.source, src.source)
  if off.isSome:
    let val = unsafeGet(off).uint32
    # apply the new base ID index as an offset to all existing IDs:
    for it in tree.mitems:
      it.info = SourceId(it.info.uint32 + val)