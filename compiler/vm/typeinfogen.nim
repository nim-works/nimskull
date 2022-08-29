## This module implements the various parts needed for ``TNimType``-based
## RTTI support:
## * a lifting pass operating on the back-end IR that collects and lifts all
##  accessed RTTI into globals
##
## ``TNimType``-based RTTI is used by the C-like targets as well as the JS
## target.

# TODO: module needs a better name

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_types
  ],
  compiler/vm/[
    irpasses,
    pass_helpers,
    vmir
  ]

proc liftTypeInfoV1(c: var LiftPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  ## Turns all ``mGetTypeInfo`` calls into globals and collects the newly
  ## created symbols
  # XXX: can this really be considered lifting?
  case n.kind
  of ntkCall:
    if getMagic(ir, c.env[], n) == mGetTypeInfo:
      cr.replace()

      let
        typ = ir.getLit(ir.at(ir.argAt(cr, 0))).typ

      assert typ != NoneType

      # XXX: the types weren't canonicalized, so we're creating lots of
      #      duplicate type info globals for the same type
      var s = c.typeInfoMarker.getOrDefault(typ)
      if s == NoneSymbol:
        # TODO: either use a `Rope` here or use a string buffer stored in
        #       `LiftPassCtx` that is reserved for temporary usage like this
        let name = "NTI" & $(typ.int) & "_" # XXX: too many short-lived and unnecessary allocations

        # TODO: cache the `TNimType` type
        let globalType = c.graph.getCompilerType("TNimType")
        # the symbol is owned by the module the type is owned by
        s = c.addGlobal(globalType, name)

        c.typeInfoMarker[typ] = s

      discard cr.insertSym(s)

  else:
    discard

const typeV1Pass* = LinearPass2[LiftPassCtx](visit: liftTypeInfoV1)