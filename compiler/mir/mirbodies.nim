## Implements the ``MirBody`` type and basic routines for querying and
## modifying it.

import
  compiler/ast/[
    ast_types
  ],
  compiler/mir/[
    mirtrees,
    sourcemaps
  ],
  compiler/utils/[
    containers
  ]

type
  Local* = object
    ## Static information about a local location ('let' or 'var'). Not modified
    ## after initialization.
    typ*: TypeId
      ## type of the local
    alignment*: uint32
      ## alignment of the location, measured in bytes. 0 means "use default"
    flags*: TSymFlags
    isImmutable*: bool
      ## whether the local was originally defined with ``let``. Used for
      ## optimization purposes
    # future direction: merge `flags` and `isImmutable` into a single set of
    # flags
    name*: PIdent
      ## either the user-defined name or 'nil'

  Locals* = Store[LocalId, Local]

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
    locals*: Locals
      ## all locals part of the body
    source*: SourceMap
    code*: MirTree

const
  resultId* = LocalId(0)
    ## the ID of the result variable. A slot for the result variable is always
    ## reserved, even if there is no result variable for a body

func `[]`*(body: MirBody, n: NodePosition): lent MirNode {.inline.} =
  body.code[n]

func sourceFor*(body: MirBody, n: NodePosition): PNode {.inline.} =
  body.source[body.code[n].info]

func `[]`*(body: MirBody, id: LocalId): lent Local {.inline.} =
  ## Returns the local corresponding to `id`.
  body.locals[id]
