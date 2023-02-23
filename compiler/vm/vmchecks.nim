## This module implements the handle validation logic.
##
## Handles are validate by first making sure that they point to VM owned
## memory. After finding the allocation the handle points into, the
## allocation's layout is walked in order to verify that there exists a valid
## location of the handle's type at the offset relative to the start of
## the allocation.
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
    vmmemory
  ]

from compiler/vm/vmobjects import variantFieldIndices
from compiler/vm/vmtypes import alignedSize

iterator fields(p: MemRegionPtr, t: PVmType): FieldIndex =
  ## Iterates and yields the indices of all active fields in the object at the
  ## given location
  if t.branches.len == 0:
    for i in 0..<t.objFields.len:
      yield FieldIndex(i)
  else:
    for i in variantFieldIndices(p, t, 0):
      yield i


func searchInObject(h: MemRegionPtr, otyp, t: PVmType, o: int): AccessViolationReason
func testLocationType(locType: PVmType, t: PVmType): AccessViolationReason

func searchInArray(h: MemRegionPtr, etyp: PVmType, t: PVmType, len, stride, o: int): AccessViolationReason =
  let i = o /% stride
  assert i < len

  if o == i * stride:
    testLocationType(etyp, t)
  else:
    case etyp.kind
    of realAtomKinds:
      avrNoLocation
    of akObject:
      searchInObject(h.getSubHandle(i*stride), etyp, t, o - (i*stride))
    of akArray:
      searchInArray(h.getSubHandle(i*stride), etyp.elementType, t, etyp.elementCount, etyp.elementStride, o - (i*stride))


func searchInObject(h: MemRegionPtr, otyp, t: PVmType, o: int): AccessViolationReason =
  assert uint(o) < otyp.sizeInBytes
  assert otyp.kind == akObject
  for i in fields(h, otyp):
    let f = otyp.fieldAt(i)
    if f.offset == o:
      return testLocationType(f.typ, t)
    elif o < f.offset + int(f.typ.sizeInBytes):
      case f.typ.kind
      of akObject:
        return searchInObject(h.getSubHandle(f.offset), f.typ, t, o - f.offset)
      of akArray:
        return searchInArray(h.getSubHandle(f.offset), f.typ.elementType, t, f.typ.elementCount, f.typ.elementStride, o - f.offset)
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
    else:
      return avrTypeMismatch

func checkValid*(al: VmAllocator, a: MemRegionPtr, typ: PVmType): AccessViolationReason =
  ## Tests if the handle with address `a` and type `typ` refers to a valid
  ## location of type `typ`
  let rp = cast[int](a.rawPointer)

  result = avrOutOfBounds

  for x in al.regions.items:
    if rp in x.start..<(x.start+x.len):
      let rtyp = x.typ
      let off = rp - x.start
      if off > 0:
        if x.count == 0:
          let mp = makeMemPtr(cast[pointer](x.start), x.typ.sizeInBytes)
          case x.typ.kind
          of akObject:
            result = searchInObject(mp, x.typ, typ, off)
          of akArray:
            result = searchInArray(mp, x.typ.elementType, typ, x.typ.elementCount, x.typ.elementStride, off)
          else:
            result = avrTypeMismatch
        else:
          if typ != nil:
            let stride = rtyp.alignedSize.int
            result = searchInArray(makeMemPtr(cast[pointer](x.start), uint(stride * x.count)), rtyp, typ, x.count, stride, off)
          else:
            unreachable()
            #[
            # untyped memory
            if uint(off) + typ.sizeInBytes < uint(x.len):
              result = avrNoError
            else:
              discard "size is past the region"
            ]#
      else:
        result = testLocationType(rtyp, typ)

      break