## This module contains various functions for querying information about
## `VmType`s and working with them in general.

import
  compiler/utils/[
    idioms
  ],
  compiler/vm/[
    vmdef
  ]

from std/bitops import bitand

type VmTypeRel* = enum
  vtrUnrelated
  vtrSame
  vtrSub
  vtrSuper


func elemType*(typ: PVmType): PVmType =
  case typ.kind
  of akSeq:
    typ.seqElemType
  of akArray:
    typ.elementType
  else:
    unreachable(typ.kind)

func alignedSize*(t: PVmType): uint {.inline.} =
  let
    s = t.sizeInBytes
    a = 1'u shl t.alignment
  bitand(s + (a - 1), not (a - 1))

template decodeStart(t: PVmType): int =
  ## Decodes the first field's position from `relFieldStart`
  let r = t.relFieldStart.int
  assert r > 0
  r - 1

func totalFieldCount*(t: PVmType): int =
  ## The total number of 'real' fields (this excludes the base fields)
  result = t.objFields.len
  if t.relFieldStart > 0:
    result += t.decodeStart - 1 # -1 for the base field


func toFieldPos*(t: PVmType, i: FieldIndex): FieldPosition =
  if t.relFieldStart == 0:
    FieldPosition(i)
  else:
    FieldPosition(int(i) + t.decodeStart - 1) # -1 for the base field

func toFieldIndex*(t: PVmType, p: FieldPosition): FieldIndex =
  let (offset, start) =
    if t.relFieldStart == 0:
      (0, 0)
    else:
      (1, t.decodeStart)
  let r = p.int - start + offset
  assert r < t.objFields.len, "field with given position is not part of the type"
  FieldIndex(r)

func getFieldAndOwner*(t: PVmType, p: FieldPosition): (PVmType, FieldIndex) {.inline.} =
  var t = t
  while int(p) < int(t.relFieldStart) - 1:
    t = t.objFields[0].typ
  let adjusted =
    if t.relFieldStart == 0:
      FieldIndex(p)
    else:
      FieldIndex(int(p) - t.decodeStart + 1) # +1 due to the base field
  (t, adjusted)

func findDiscrBranchEntry*(t: PVmType, idx: FieldIndex): uint32 =
  ## Returns the branch-entry index for the discriminator with the given field
  ## index
  assert t.fieldAt(idx).typ.kind == akDiscriminator

  while result.int <% t.branches.len:
    let b = t.branches[result]
    if b.kind == blekStart and b.field == idx:
      break
    inc result

  assert result.int < t.branches.len

# XXX: how object inheritance information is stored it's likely going to change
func objTypeRel*(a, b: PVmType, swapped: bool): VmTypeRel =
  var a = a
  while a.relFieldStart >= b.relFieldStart:
    a = a.objFields[0].typ
    if a == b:
      return if swapped: vtrSuper else: vtrSub

  result = vtrUnrelated

func getTypeRel*(a, b: PVmType): VmTypeRel =
  ## Computes the relation between `a` and `b`, namely, whether `a` a super,
  ## sub, same or unrelated type to `b`. The rules from the language manual
  ## are used
  if a == b:
    result = vtrSame
  elif a.kind == b.kind:
    case a.kind
    of akObject:
      if a.relFieldStart != b.relFieldStart:
        let swapped = b.relFieldStart > a.relFieldStart
        var aa = a
        var bb = b
        {.cast(noSideEffect).}:
          # erroneously inferred side-effect
          if swapped: swap(aa, bb)
        # After the swap, `aa` is the object with the higher `relFieldStart`

        result = objTypeRel(aa, bb, swapped)

      elif a.relFieldStart == 0:
        # Both objects are base objects and since `a != b`, they're unrelated
        result = vtrUnrelated
      else:
        # Slow path
        result = objTypeRel(a, b, false)
        if result == vtrUnrelated:
          result = objTypeRel(b, a, true)

    of akRef, akPtr:
      result = getTypeRel(a.targetType, b.targetType)
    else:
      result = vtrUnrelated
  else:
    result = vtrUnrelated
