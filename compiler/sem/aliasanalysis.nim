## This module implements a simple alias analysis based on MIR operation
## sequences, used to compute the relationship between two lvalues, i.e. if
## the underlying locations potentially overlap or are part sub- or
## super-locations of each other.
##
## Only lvalues with statically know roots (derived from named locals, global,
## etc.) are supported.
##
## The main procedure is ``compareLvalues``: it performs the comparison and
## returns the result as an opaque object ``CmpLocsResult``, which can then be
## queried for the relationship between the two inputs.
##
## Whether two lvalues refer to overlapping locations is not always statically
## known (e.g. when accessing an array with a value only known at run-time) --
## if it is not, the uncertainity is communicated via the ``maybe`` result.

import
  compiler/ast/[
    ast
  ],
  compiler/mir/[
    mirtrees
  ],
  compiler/utils/[
    idioms
  ]

type
  Ternary* = enum
    no, maybe, yes

  LvalueExpr* = tuple[root, last: NodePosition]
    # XXX: ``LvalueExpr`` fit well at one point, but it doesn't anymore. The
    #      name ``Handle`` might be a better fit now

  CmpLocsResult* = object
    endA, endB: bool ## whether the `last` operation of the provided sequences
                     ## was reached
    overlaps: Ternary

const
  Roots = SymbolLike + {mnkTemp, mnkCall, mnkMagic, mnkDeref, mnkDerefView}

func isSameRoot(an, bn: MirNode): bool =
  if an.kind != bn.kind:
    return false

  case an.kind
  of SymbolLike:
    result = an.sym.id == bn.sym.id
  of mnkTemp:
    result = an.temp == bn.temp
  of Roots - SymbolLike - {mnkTemp}:
    result = false
  else:
    unreachable(an.kind)

func sameIndex(a, b: MirNode): Ternary =
  if a.kind != b.kind or a.kind != mnkLiteral:
    maybe
  else:
    if a.lit.intVal == b.lit.intVal:
      yes
    else:
      no

proc compareLvalues(body: MirTree, a, b: NodePosition,
                    lastA, lastB: NodePosition): CmpLocsResult =
  ## Performs the comparision and computes the information to later derive the
  ## facts from (e.g. if A is a part B)
  if not isSameRoot(body[a], body[b]):
    return CmpLocsResult(overlaps: no)

  var
    a = a + 1
    b = b + 1

  const IgnoreSet = {mnkStdConv, mnkConv, mnkAddr, mnkView}
    ## we can skip all conversions, since we know that they must be lvalue
    ## conversions; ``compareLvalues`` is only used for lvalues, which
    ## can't result from non-lvalue conversions.
    ## Both the 'addr' and 'view' operatio create a first-class handle to
    ## the reference location, so we also skip them. This allows for ``a.x``
    ## to be treated as refering to same location as ``a.x.addr`` (which is
    ## correct)

  var overlaps = yes # until proven otherwise
  while overlaps != no and a <= lastA and b <= lastB:
    while body[a].kind in IgnoreSet:
      inc a

    while body[b].kind in IgnoreSet:
      inc b

    if a > lastA or b > lastB:
      break

    let
      an {.cursor.} = body[a]
      bn {.cursor.} = body[b]

    if an.kind != bn.kind:
      overlaps = no
      break

    case an.kind
    of mnkPathNamed, mnkPathVariant:
      if an.field.id != bn.field.id:
        overlaps = no
        break

    of mnkPathPos:
      if an.position != bn.position:
        overlaps = no
        break

    of mnkPathArray:
      # -1 = the arg-block end
      # -2 = the arg node
      # -3 = the operand
      overlaps = sameIndex(body[a - 3], body[b - 3])
      if overlaps == no:
        break

    of ArgumentNodes:
      # this happens when invoking a path operator with more than one
      # operand. Skip to the end of the arg-block
      a = parentEnd(body, a)
      b = parentEnd(body, b)
    else:
      unreachable(an.kind)

    inc a
    inc b

  result = CmpLocsResult(endA: a > lastA, endB: b > lastB, overlaps: overlaps)

func isAPartOfB*(r: CmpLocsResult): Ternary {.inline.} =
  result = r.overlaps
  if result != no and not r.endB and r.endA:
    # B either is or maybe is a part of A, but A is not a part of B
    result = no

func isBPartOfA*(r: CmpLocsResult): Ternary {.inline.} =
  result = r.overlaps
  if result != no and not r.endA and r.endB:
    result = no

func isSame*(r: CmpLocsResult): bool {.inline.} =
  ## Returns whether A and B refer to the exact same location
  ## (ignoring the type)
  result = r.overlaps == yes and r.endA and r.endB

template overlaps*(r: CmpLocsResult): Ternary =
  r.overlaps

func compareLvalues*(tree: MirTree, ea, eb: LvalueExpr): CmpLocsResult {.inline.} =
  result = compareLvalues(tree, ea.root, eb.root, ea.last, eb.last)

func overlaps*(tree: MirTree, ea, eb: LvalueExpr): Ternary {.inline.} =
  ## Convenience wrapper
  compareLvalues(tree, ea.root, eb.root, ea.last, eb.last).overlaps

func isPartOf*(tree: MirTree, ea, eb: LvalueExpr): Ternary {.inline.} =
  ## Computes if the location named by `ea` is part of `eb`. Also evaluates to
  ## 'yes' if both name the exact same location
  let cmp =  compareLvalues(tree, ea.root, eb.root, ea.last, eb.last)
  result = isAPartOfB(cmp)