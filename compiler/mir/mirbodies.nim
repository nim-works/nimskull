## Implements the ``MirBody`` type and basic routines for querying and
## modifying it.

import
  compiler/ast/[
    ast_types
  ],
  compiler/mir/[
    mirtrees,
    sourcemaps
  ]

type
  MirBody* = object
    ## A ``MirBody`` represents a self-contained piece of MIR code. This can
    ## either be:
    ## - the full body of a procedure
    ## - the partial body a procedure
    ## - a standalone statement/expression (currently supported for compile
    ##   time code execution)
    ##
    ## In each case, ``MirBody`` stores all the local data referenced and
    ## needed by the body's MIR code. It also store additional information
    ## associated with a body, such as how far the lowering is along.
    source*: SourceMap
    code*: MirTree

func `[]`*(body: MirBody, n: NodePosition): lent MirNode {.inline.} =
  body.code[n]

func sourceFor*(body: MirBody, n: NodePosition): PNode {.inline.} =
  body.source[body.code[n].info]