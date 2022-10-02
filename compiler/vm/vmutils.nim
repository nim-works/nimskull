## This module contains some utility procedures for inspecting VM-related
## data.
##
## The VM doesn't depend on the procedures here in order to
## function - they are only meant as a logging and debugging aid.

import
  std/[
    intsets
  ],
  compiler/ast/[
    ast,
    reports
  ],
  compiler/vm/[
    vmdef
  ]

func codeListing*(c: TCtx; start = 0; last = -1): seq[DebugVmCodeEntry] =
  ## Produces a listing of the instructions in `c` that are located in the
  ## instruction range ``start..last``. If ``last < 0``, then all instructions
  ## after position `start` are included in the listing. The instructions are
  ## ordered by their position
  let last =
    if last < 0: c.code.high
    else:        min(last, c.code.high)

  # first iteration: compute all necessary labels:
  var jumpTargets = initIntSet()
  for i in start..last:
    let x = c.code[i]
    if x.opcode in relativeJumps:
      jumpTargets.incl(i+x.regBx-wordExcess)

  # because some instructions take up more than one instruction word, there's
  # not a 1-to-1 mapping between instruction words and the resulting list
  # entries
  result = newSeqOfCap[DebugVmCodeEntry](last - start + 1)

  var i = start
  while i <= last:
    let
      x = c.code[i]
      opc = opcode(x)

    var code = DebugVmCodeEntry(
      pc: i,
      opc: opc,
      ra: x.regA,
      rb: x.regB,
      rc: x.regC,
      idx: x.regBx - wordExcess,
      info: c.debug[i],
      isTarget: i in jumpTargets
    )

    case opc:
    of opcConv, opcCast:
      code.types = (c.rtti[c.code[i + 1].regBx-wordExcess].nimType,
                    c.rtti[c.code[i + 2].regBx-wordExcess].nimType)
      inc i, 2

    of opcLdConst, opcAsgnConst:
      let cnst = c.constants[code.idx]
      code.ast =
        case cnst.kind
        of cnstInt:    newIntNode(nkIntLit, cnst.intVal)
        of cnstFloat:  newFloatNode(nkFloatLit, cnst.floatVal)
        of cnstString: newStrNode(nkStrLit, cnst.strVal)
        of cnstNode:   cnst.node
        of cnstSliceListInt..cnstSliceListStr:
          # XXX: translate into an `nkOfBranch`?
          newNode(nkEmpty)

    else:
      discard

    result.add code
    inc i

func initVmCodeListingReport*(c: TCtx, prc: PSym, ast: PNode;
                              start = 0; last = -1): DebugReport =
  ## Convenience procedure for initializing a code-listing debug report with
  ## the given arguments.
  ##
  ## See also:
  ## * `codeListing <#codeListing,TCtx,int,int>`_
  result = DebugReport(kind: rdbgVmCodeListing)
  result.vmgenListing.sym = prc
  result.vmgenListing.ast = ast
  result.vmgenListing.entries = codeListing(c, start, last)