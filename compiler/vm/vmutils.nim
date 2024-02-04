## This module contains some utility procedures for inspecting VM-related
## data.
##
## The VM doesn't depend on the procedures here in order to
## function - they are only meant as a logging and debugging aid.

import
  std/[
    intsets,
    strutils
  ],
  compiler/ast/[
    ast,
    lineinfos,
    renderer,
    typesrenderer,
  ],
  compiler/front/[
    options
  ],
  compiler/vm/[
    vmdef
  ]

from compiler/front/msgs import toFileLineCol

type
  DebugVmCodeEntry* = object
    isTarget*: bool
    info*: TLineInfo
    pc*: int
    idx*: int
    case opc*: TOpcode:
      of opcConv, opcCast:
        types*: tuple[tfrom, tto: PType]
      of opcLdConst:
        ast*: PNode
      else:
        discard
    ra*: int
    rb*: int
    rc*: int


proc codeListing*(c: TCtx; start = 0; last = -1): seq[DebugVmCodeEntry] =
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
      code.rb = c.code[i + 1].regA
      code.types = (c.rtti[c.code[i + 0].regBx-wordExcess].nimType,
                    c.rtti[c.code[i + 1].regBx-wordExcess].nimType)
      inc i, 1
    of opcLdConst:
      let cnst = c.constants[code.idx]
      code.ast =
        case cnst.kind
        of cnstInt:    newIntNode(nkIntLit, cnst.intVal)
        of cnstFloat:  newFloatNode(nkFloatLit, cnst.floatVal)
        of cnstNode:   cnst.node
        of cnstSliceListInt..cnstSliceListFloat:
          # XXX: translate into an `nkOfBranch`?
          newNode(nkEmpty)
    else:
      discard

    result.add code
    inc i

proc renderCodeListing*(config: ConfigRef, sym: PSym,
                        entries: seq[DebugVmCodeEntry]): string =
  ## Renders the code listing `entries` to text. `sym` is an optional symbol
  ## that, if provided, is used for providing additional context.
  if sym != nil:
    result = "Code Listing for '$1' $2" %
             [sym.name.s, config.toFileLineCol(sym.info)]
  else:
    result = "Code Listing for <unknown>"

  result.add "\n\n"

  var line: string # re-used for efficiency
  for e in entries:
    if e.isTarget:
      result.addf("L:$1\n", e.pc)

    func `$<`[T](arg: T): string =
      alignLeft($arg, 5)
    func `$<`(opc: TOpcode): string =
      # cut off the enum prefix
      alignLeft(substr($opc, 3), 12)

    line.setLen(0)

    case e.opc
    of opcIndCall, opcIndCallAsgn:
      line.addf("  $# r$# r$# #$#", $<e.opc, $<e.ra, $<e.rb, $<e.rc)
    of opcConv, opcCast:
      line.addf("  $# r$# r$# $# $#",
                $<e.opc, $<e.ra, $<e.rb,
                $<e.types[0].typeToString(),
                $<e.types[1].typeToString())
    of opcSetEh:
      line.addf("  $# $# $#", $<e.opc, $<e.ra, $e.rb)
    elif e.opc < firstABxInstr:
      line.addf("  $# r$# r$# r$#", $<e.opc, $<e.ra, $<e.rb, $<e.rc)
    elif e.opc in relativeJumps:
      line.addf("  $# r$# L$#", $<e.opc, $<e.ra, $<e.idx)
    elif e.opc in {opcLdConst}:
      line.addf("  $# r$# $# $#",
                $<e.opc, $<e.ra, $<e.ast.renderTree(), $<e.idx)
    else:
      line.addf("  $# r$# $#", $<e.opc, $<e.ra, $<e.idx)

    result.add alignLeft(line, 48)
    result.add toFileLineCol(config, e.info)
    result.add "\n"

  # one final line break
  result.add "\n"