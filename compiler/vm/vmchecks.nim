## This module implements the handle validation logic.
##
## After making making sure that the handle references a valid cell (it's an
## error otherwise), the layout of cell is traversed in order to verify that
## there exists a valid location of the handle's type at the offset relative to
## the start of the cell.
##
## Special handling is required for variant objects, since whether a
## sub-location is valid depends on the run-time value of the respective
## disciminator(s) there.

import
  compiler/utils/[
    idioms
  ],
  compiler/vm/[
    vmdef,
    vmtypes
  ]

from compiler/vm/vmobjects import variantFieldIndices

iterator fields(p: VmMemoryRegion, t: PVmType): FieldIndex =
  ## Iterates and yields the indices of all active fields in the object at the
  ## given location
  if t.branches.len == 0:
    for i in 0..<t.objFields.len:
      yield FieldIndex(i)
  else:
    for i in variantFieldIndices(p, t, 0):
      yield i


func searchInObject(h: VmMemoryRegion, otyp, t: PVmType, o: int): AccessViolationReason
func testLocationType(locType: PVmType, t: PVmType): AccessViolationReason

func searchInArray(h: VmMemoryRegion, etyp: PVmType, t: PVmType, len, stride, o: int): AccessViolationReason =
  let i = o /% stride
  assert i < len

  if o == i * stride:
    testLocationType(etyp, t)
  else:
    case etyp.kind
    of realAtomKinds:
      avrNoLocation
    of akObject:
      searchInObject(h.subView(i*stride), etyp, t, o - (i*stride))
    of akArray:
      searchInArray(h.subView(i*stride), etyp.elementType, t, etyp.elementCount, etyp.elementStride, o - (i*stride))

func searchInObject(h: VmMemoryRegion, otyp, t: PVmType, o: int): AccessViolationReason =
  assert uint(o) < otyp.sizeInBytes
  assert otyp.kind == akObject
  for i in fields(h, otyp):
    let f = otyp.fieldAt(i)
    if f.offset == o:
      return testLocationType(f.typ, t)
    elif o < f.offset + int(f.typ.sizeInBytes):
      case f.typ.kind
      of akObject:
        return searchInObject(h.subView(f.offset), f.typ, t, o - f.offset)
      of akArray:
        return searchInArray(h.subView(f.offset), f.typ.elementType, t, f.typ.elementCount, f.typ.elementStride, o - f.offset)
      else:
        return avrNoLocation


func testLocationType(locType: PVmType, t: PVmType): AccessViolationReason =
  result = avrNoError
  if locType == t:
    return # common case

  var it {.cursor.} = locType
  while it != t:
    case it.kind
    of akObject:
      if it.objFields.len > 0:
        it = it.objFields[0].typ
      else:
        return avrTypeMismatch
    of akArray:
      if it.elementCount > 0:
        it = it.elementType
      else:
        return avrTypeMismatch
    of akRef, akPtr:
      # special rule: a location of type ``ref|ptr T`` can be accessed via a
      # handle with type ``ref|ptr X``, where `X` is either a super- or sub-type
      # of `T`
      if getTypeRel(t, it) != vtrUnrelated:
        break
      else:
        return avrTypeMismatch
    else:
      return avrTypeMismatch


func checkValid(p: VmMemPointer, typ: PVmType, cell: VmCell): AccessViolationReason =
  assert p.rawPointer >= cell.p, "memory not part of `cell`"

  let off = cast[int](p) - cast[int](cell.p)
  if off > 0:
    # `p` is an interior pointer
    if typ != nil:
      # the cell stores either a single- or a contiguous sequence of locations
      let stride = cell.typ.alignedSize.int
      result = searchInArray(toOpenArray(cell.p, 0, int(cell.sizeInBytes-1)), cell.typ, typ, cell.count, stride, off)
    else:
      # untyped memory; not yet supported
      unreachable()
  else:
    # `p` points to the start of the cell -- the most simple case. We only
    # need to perform a type comparision
    result = testLocationType(cell.typ, typ)

func checkValid*(al: VmAllocator, h: LocHandle): AccessViolationReason =
  ## Tests and returns whether the handle `h` is valid. That is, whether it
  ## points to a valid memory cell and location and whether the handle's type
  ## is compatible with that of the location.
  if h.cell != -1:
    checkValid(h.p, h.typ, al.cells[h.cell])
  else:
    avrOutOfBounds
