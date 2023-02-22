## This module contains procedures for interacting with locations.

# TODO: merge seq and string implementation into one
# TODO: don't reallocate on every resize; use a capacity

import
  compiler/ast/[
    ast_types,
    ast
  ],
  compiler/front/[
    options
  ],
  compiler/utils/[
    idioms
  ],
  compiler/vm/[
    vmdef,
    vmmemory,
    vmtypes
  ]

import std/options as soptions

from std/bitops import bitand

func resetLocation*(mm: var VmMemoryManager, loc: var VmMemoryRegion, typ: PVmType)
  ## If the given location is a managed atomic location, cleans up and frees
  ## any auxiliary memory. If the location is a compound location, recursively
  ## cleans up all managed sub-locations
proc copyToLocation*(mm: var VmMemoryManager, dest: var VmMemoryRegion, src: MemRegionPtr, typ: PVmType, reset: static[bool] = true)
  ## Copies the data at `src` to `dest`. This is equivalent to an assignment
  ## in user-code. `reset` indicates whether is dest is in an initial zero
  ## state (`false`) or already contains valid data (`true`).

func reinterpretWrite[T](p: pointer, v: T) {.inline.} =
  copyMem(p, unsafeAddr v, sizeof T)


# TODO: think about endianness of the VM a bit. Right now it's the same
# as the host's, but maybe it should be the same as the target's instead?

func readUInt*(r: VmMemoryRegion): BiggestInt {.inline.} =
  ## Since uints are mostly stored as BiggestInt inside the compiler/VM,
  ## they're returned as BiggestInt here too.
  assert r.len in 1..8
  copyMem(addr result, unsafeAddr r[0], r.len)


func writeUInt*(r: var VmMemoryRegion, val: BiggestInt) {.inline.} =
  assert r.len in 1..8
  copyMem(addr r[0], unsafeAddr val, r.len)


func writeUInt*(h: LocHandle, val: BiggestInt) {.inline.} =
  assert h.isValid()
  copyMem(h.h.rawPointer, unsafeAddr val, h.typ.sizeInBytes)

# TODO: rename to writeIntBits
func writeInt*(r: var VmMemoryRegion, val: BiggestInt) {.inline.} =
  assert int8(r.len) in {1, 2, 4, 8}
  # TODO: use `reinterpretWrite` here
  copyMem(addr r[0], unsafeAddr val, r.len)


func readIntBits*(r: VmMemoryRegion): BiggestInt {.inline.} =
  let l = r.len
  assert int8(l) in {1, 2, 4, 8}
  result = 0
  copyMem(addr result, unsafeAddr r[0], r.len)


func readFloat32*(m: VmMemoryRegion): BiggestFloat {.inline.} =
  assert m.len == 4
  var f: float32
  copyMem(addr f, unsafeAddr m[0], 4)
  BiggestFloat(f)

func readFloat64*(m: VmMemoryRegion): BiggestFloat {.inline.} =
  assert m.len == 8
  var f: float64
  copyMem(addr f, unsafeAddr m[0], 8)
  BiggestFloat(f)

func readFloat*(m: VmMemoryRegion): BiggestFloat {.inline.} =
  case m.len
  of 4: readFloat32(m)
  of 8: readFloat64(m)
  else:
    unreachable("Float must be 4 or 8 byte in size")

func writeFloat64*(h: MemRegionPtr, f: float64) {.inline.} =
  assert h.isValid(8)
  reinterpretWrite(h.rawPointer, f)

func writeFloat64*(h: LocHandle, f: float64) {.inline.} =
  assert h.isValid()
  assert h.typ.kind == akFloat
  assert h.typ.sizeInBytes == 8
  reinterpretWrite(h.h.rawPointer, f)

func writeFloat32*(h: MemRegionPtr, f: float32) {.inline.} =
  assert h.isValid(4)
  reinterpretWrite(h.rawPointer, f)

func writeFloat32*(h: LocHandle, f: float32) {.inline.} =
  assert h.h.isValid(4)
  assert h.typ.kind == akFloat
  assert h.typ.sizeInBytes == 4
  reinterpretWrite(h.h.rawPointer, f)


func writeFloat*(h: LocHandle, f: BiggestFloat) {.inline.} =
  ## Write the float to the location. Converts the float to the
  ## locations size prior to writing
  assert h.isValid()
  assert h.typ.kind == akFloat
  case h.typ.sizeInBytes
  of 4:
    writeFloat32(h.h, float32(f))
  of 8:
    writeFloat64(h.h, float64(f))
  else:
    unreachable("Float must be 4 or 8 byte in size")

func unpackDiscr*(v: BiggestInt, numBits: Natural): tuple[value: int, index: int] {.inline.} =
  ## Extracts the discriminator value and branch index from the combined value
  let partMask = (1 shl numBits) - 1
  result.value = bitand(v, partMask).int
  result.index = bitand(v shr numBits, partMask).int

func packDiscr*(value, index, numBits: Natural): int =
  ## Packs the discriminator value and branch index into a combined value
  let L = 1 shl numBits
  assert value < L
  assert index < L
  result = (index shl numBits) or value
  assert unpackDiscr(result, numBits) == (value, index)


func readDiscriminant*(h: VmMemoryRegion, numBits: Natural): BiggestInt {.inline.} =
  ## Reads the discriminator's value from location `h` and extracts the
  ## user-facing value. `numBits` is the number of bits the value occupies
  readUInt(h).unpackDiscr(numBits).value

func readDiscriminant*(h: LocHandle): BiggestInt {.inline.} =
  assert h.typ.kind == akDiscriminator
  readUInt(h.byteView()).unpackDiscr(h.typ.numBits).value


func mapBranchIndex(index, default: int): int {.inline.} =
  ## Maps the branch index (`index`) to or from it's in-memory counterpart.
  ## See `vmtypegen.genRecordNode` for more information
  if index == 0: default
  elif index == default: 0
  else: index

func mapBranchIndex(bIdx: int, t: PVmType, idx: FieldIndex): int {.inline.} =
  mapBranchIndex(
    bIdx,
    t.branches[findDiscrBranchEntry(t, idx)].defaultBranch.int)

# TODO: the branch index should be a distinct int
func readDiscrBranch*(loc: LocHandle, owner: PVmType, idx: FieldIndex): int =
  ## Reads the discriminator value from `loc` and extracts/translates the
  ## branch index
  assert loc.typ.kind == akDiscriminator
  mapBranchIndex(
    readIntBits(loc.byteView()).unpackDiscr(loc.typ.numBits).index.int,
    owner, idx)

proc writeDiscrField*(loc: LocHandle, owner: PVmType, idx: FieldIndex, value, index: Natural) =
  ## Writes the discriminator to `loc`.
  ##
  ## `owner` refers to the type of the object the field is part of.
  ## `idx` is the field's index.
  assert loc.typ.kind == akDiscriminator
  let index = mapBranchIndex(index, owner, idx)
  writeUInt(loc, packDiscr(value, index, loc.typ.numBits))


# XXX: should be a function, but openArray is not a valid return type yet
template bitSet*(handle: LocHandle): untyped =
  ## Get a view to the `set` value stored in the location at `handle`
  let h = handle
  assert h.isValid()
  assert h.typ.kind == akSet
  toOpenArray(h.h.rawPointer, 0, int(h.typ.sizeInBytes - 1))


template mbitSet*(handle: LocHandle): untyped =
  ## Get a mutable view to the `set` value stored in the location at `handle`
  let h = handle
  assert h.isValid()
  assert h.typ.kind == akSet
  var p = h.h.rawPointer
  toOpenArray(p, 0, int(h.typ.sizeInBytes - 1))


func seekToBranch(bI: var uint32, target: int, typ: PVmType) =
  ## Moves the walk-list index `bI` to the branch with index `target`. `bI` is
  ## expected to point to the first branch
  assert typ.branches[bI].kind == blekBranch

  var i = 0
  while true:
    let b = typ.branches[bI]
    case b.kind
    of blekStart:
      bI += b.numItems
    of blekBranch:
      if i == target:
        break
      else:
        inc i
        inc bI
    of blekEnd:
      unreachable()

type VariantFieldIterCtx* = object
  ## The state for the variant field iterator. Before calling `get`
  ## or `next` for the first time, the iterator has to be set up with `setup`
  i, L: int
  last, nextDiscField: FieldIndex
  bI: uint32 ## index of the current walk-list entry

func setup*(ctx: var VariantFieldIterCtx, typ: PVmType, branch: uint32) =
  ## Initializes the iterator state. `branch` is the walk-list entry index of
  ## the branch that is to be iterated
  assert typ.branches[branch].kind == blekBranch
  ctx.bI = branch + 1

  let r = typ.branches[branch].fieldRange
  ctx.i = r.a.int
  ctx.last = r.b
  ctx.L = int(ctx.last) + 1

  ctx.nextDiscField =
    if typ.branches[ctx.bI].kind == blekStart:
      typ.branches[ctx.bI].field
    else:
      ctx.last

func get*(ctx: VariantFieldIterCtx): tuple[valid: bool, idx: FieldIndex] =
  ## Returns the current field index (or 0) and whether iteration is finished
  if ctx.i <% ctx.L:
    result = (true, FieldIndex(ctx.i))

func next*(ctx: var VariantFieldIterCtx, src: VmMemoryRegion, typ: PVmType) =
  ## Moves the iterator to the next active field, or does nothing if no more
  ## active fields are left. The fields are iterated in ascending index
  ## order but excludes fields of base types (if there exists a base type).
  ## `typ` must be the same match the type passed to `setup`
  let i = ctx.i
  var bI = ctx.bI

  if i <% ctx.L:
    var fI = FieldIndex(i)

    # XXX: maybe merge both branches into one? (they have some overlap)
    if fI == ctx.last:
      # Seek to the branch end
      while int(bI) <% typ.branches.len:
        let b = typ.branches[bI]
        case b.kind
        of blekStart:
          bI += b.numItems
        of blekBranch:
          bI += 1
        of blekEnd:
          let nr = typ.branches[bI].fieldRange
          if nr.a <= nr.b:
            fI = nr.a
            ctx.last = nr.b
            break

          # The record case is at the end of a branch (there are no fields
          # following the record case that are in the same scope as
          # the record case itself). Continue unwinding
          bI += 1

      if int(bI) <% typ.branches.len:
        # -1 since we increment at the end of the loop
        fI = fI - 1

        assert typ.branches[bI].kind == blekEnd
        bI += 1

        assert int(bI) <% typ.branches.len
        if typ.branches[bI].kind == blekStart:
          ctx.nextDiscField = typ.branches[bI].field

      else:
        fI = FieldIndex(typ.objFields.len)

    elif fI == ctx.nextDiscField:
      let dField = typ.fieldAt(fI)
      let dTyp = dField.typ
      assert dField.typ.kind == akDiscriminator
      assert typ.branches[bI].kind == blekStart

      let active =
        mapBranchIndex(
          unpackDiscr(
            readIntBits(src.subView(dField.offset, dTyp.sizeInBytes)),
            dTyp.numBits).index,
          typ.branches[bI].defaultBranch.int)

      # The VM protects discriminator fields from invalid values, so it's safe to
      # assume that `active` is valid
      assert active < typ.branches[bI].numBranches.int

      # Goto first branch entry
      inc bI

      # Seek to active branch
      seekToBranch(bI, active, typ)

      assert typ.branches[bI].fieldRange.a > fI
      # -1 since we increment at the end of the loop
      fI = typ.branches[bI].fieldRange.a - 1

      ctx.last = typ.branches[bI].fieldRange.b
      inc bI

      # A branch inside an discriminator sub-tree must always be followed by another list entry
      assert int(bI) <% typ.branches.len
      if typ.branches[bI].kind == blekStart:
        ctx.nextDiscField = typ.branches[bI].field

    ctx.i = int(fI) + 1
    ctx.bI = bI


iterator variantFieldIndices*(src: MemRegionPtr, typ: PVmType, branch: uint32 = 0'u32): FieldIndex =
  ## Iterates over the active fields (excluding base type fields) of the
  ## object at location `src`. The iterator supports changing active branches
  ## via discriminator updates on-the-fly, but only the one at the currently
  ## yielded field index.
  var ctx: VariantFieldIterCtx
  ctx.setup(typ, branch)

  while true:
    let r = ctx.get()
    if r[0]:
      yield r[1]
      ctx.next(byteView(src), typ)
    else:
      break

iterator variantFields*(src: MemRegionPtr, typ: PVmType, branch: uint32 = 0'u32): (int, PVmType) =
  ## Similar to `variantFieldIndices`, but yields the `(offset, type)` pair
  ## instead of the index
  for x in variantFieldIndices(src, typ, branch):
    yield typ.fieldAt(x)


func arrayLen*(loc: LocHandle): int =
  ## Returns the number of items of the array-like at `loc`
  case loc.typ.kind
  of akSeq:
    deref(loc).seqVal.length
  of akArray:
    loc.typ.elementCount
  of akString:
    deref(loc).strVal.len
  else:
    unreachable(loc.typ.kind)


template toOpenArray(a: VmString): untyped =
  assert a.len == 0 or a.data.isValid(uint a.len)
  toOpenArray(cast[ptr UncheckedArray[char]](a.data.rawPointer), 0, a.len - 1)

func `==`*(a, b: VmString): bool =
  toOpenArray(a) == toOpenArray(b)

template `==`*(a, b: VmFunctionPtr): bool =
  int(a) == int(b)


template subView(mem: VmMemoryRegion, off: int, typ: PVmType): untyped =
  ## Returns sub-view of `mem`, starting at `off` and having the length of the
  ## size-in-bytes of `typ`
  mem.subView(off, typ.sizeInBytes)


template signExtended*(val, size: BiggestInt): untyped =
  ## Calculates sign-extended `x` without branching. `size` is the number of
  ## bytes `val` occupies.
  let lsh = 8 * (sizeof(val) - size)
  ashr(val shl lsh, lsh)

# XXX: not sure if it's a good idea to overload the stringify operator here...
func `$`*(s: VmString): string =
  result = newString(s.len)

  if s.len > 0:
    safeCopyMem(result.toOpenArrayByte(0, result.high), s.data.subView(s.len), s.len)

func asCString*(s: VmString): cstring =
  ## Returns a `cstring` that is a direct view into the vm string's backing memory

  # While I doubt that the VM (compiler) will be able to run on the JS target
  # in the near future, treating the string's memory as a cstring won't work
  # there

  if not s.data.isNil:
    cast[cstring](s.data.rawPointer)
  else:
    nil

func allocVmString(a: var VmAllocator, len: Natural): MemRegionPtr {.inline.} =
  # +1 for the terminating '\0'
  let len = len + 1
  a.allocTypedLocations(a.byteType, len, len)

func asgnVmString*(dest: var VmString, src: VmString, a: var VmAllocator) =
  a.dealloc(dest.data)

  if src.len > 0:
    assert not src.data.isNil
    dest = newVmString(a.allocVmString(src.len), src.len)
    safeCopyMem(dest.data.subView(src.len), src.data.subView(src.len), src.len)
  else:
    dest = newVmString(nilMemPtr, 0)

func cmp*(a, b: VmString): int =
  let minLen = min(a.len, b.len)
  result = cmpMem(a.data.rawPointer, b.data.rawPointer, minLen)
  if result == 0:
    result = a.len - b.len

func `<=`*(a, b: VmString): bool =
  cmp(a, b) <= 0


func `<`*(a, b: VmString): bool =
  cmp(a, b) < 0

func `>`*(a, b: VmString): bool =
  cmp(a, b) > 0

func `>=`*(a, b: VmString): bool =
  cmp(a, b) >= 0

func `==`*(a: VmString, b: openArray[char]): bool =
  toOpenArray(a) == b

func `[]`*(s: VmString, i: Natural): char =
  ## Return the char at index `i`. Doesn't do any validation or index checking.
  ## The caller is expected to take care of that

  assert not s.data.isNil
  assert i < s.len

  char(s.data.rawPointer[i])


func `[]=`*(s: var VmString, i: Natural, c: char) =
  ## Set the char at index `i`. Doesn't do any validation or index checking.
  ## The caller is expected to take care of that

  assert not s.data.isNil
  assert i < s.len

  s.data.rawPointer[i] = byte(c)

func setLen*(s: var VmString, newLen: Natural, a: var VmAllocator) =
  ## Truncates or extends the length of `s` to `newLen`
  # TODO: improve
  var newData: MemRegionPtr
  if newLen > 0:
    newData = a.allocVmString(newLen)
    let numBytes = min(newLen, s.len)

    safeCopyMem(newData.subView(newLen), s.data.subView(s.len), numBytes)

  else:
    newData = nilMemPtr

  a.dealloc(s.data)
  s = newVmString(newData, newLen)



func add*(s: var VmString, c: char, mm: var VmAllocator) =
  let i = s.len
  s.setLen(i + 1, mm)
  s[i] = c

func add*(s: var VmString, chars: openArray[char], mm: var VmAllocator) =
  if chars.len == 0:
    return

  let i = s.len
  s.setLen(i + chars.len, mm)
  safeCopyMem(s.data.subView(i, chars.len), chars, chars.len)

# TODO: rename to `append`?
func add*(s: var VmString, str: VmString, mm: var VmAllocator) =
  add(s, toOpenArray(str), mm)

func add*(s: var string, str: VmString) =
  # XXX: should a `add(var string, openArray[char])` be added to the stdlib?
  #s.add toOpenArray(str)
  let i = s.len
  s.setLen(i + str.len)
  safeCopyMem(s.toOpenArray(i, s.high), str.data.subView(str.len), str.len)


func getItemHandle*(loc: LocHandle, index: Natural): LocHandle =
  ## Creates a handle to the item at `index` for the array-like at `loc`
  let typ = loc.typ
  case typ.kind
  of akSeq:
    makeLocHandle(getSubHandle(deref(loc).seqVal.data, typ.seqElemStride * index), typ.seqElemType)
  of akArray:
    makeLocHandle(getSubHandle(loc.h, typ.elementStride * index), typ.elementType)
  else:
    # Not an array like type
    unreachable(typ.kind)


proc arrayCopy*(mm: var VmMemoryManager, dest, src: MemRegionPtr, count: Natural, elemTyp: PVmType, reset: static[bool]) =
  ## Copies (via assignment) all items from `src[0..count-1]` to
  ## `dest[0..count-1]`
  let stride = elemTyp.alignedSize.int
  var off = 0
  # TODO: validate that the regions don't overlap. If they do, chaos will ensure

  # TODO: trivial copy optimization
  for i in 0..<count:
    mm.copyToLocation(dest.subView(off, elemTyp.sizeInBytes), src.getSubHandle(off), elemTyp, reset)
    off += stride

func destroyVmSeq(s: var VmSeq, typ: PVmType, mm: var VmMemoryManager) =
  ## Cleans up all items and frees the backing memory
  let elemTyp = typ.seqElemType
  # TODO: do nothing if the items are is trivially destructible
  for i in 0..<s.length:
    let offset = i * typ.seqElemStride
    mm.resetLocation(s.data.subView(offset, elemTyp.sizeInBytes), elemTyp)

  mm.allocator.dealloc(s.data)

func shrink(s: var VmSeq, typ: PVmType, newLen: Natural, mm: var VmMemoryManager) =
  ## Truncates `s` to `newLen`, cleaning up all items past `newLen`
  assert s.length > newLen

  let newSize = newLen * typ.seqElemStride

  let oldData = s.data

  if newLen > 0:
    s.data = mm.allocator.allocTypedLocations(typ.seqElemType, newLen, newSize)
    safeCopyMemSrc(s.data.subView(newSize), oldData.subView(newSize))
  else:
    s.data = nilMemPtr

  for i in newLen..<s.length:
    # Reset locations of remaining elements
    mm.resetLocation(oldData.subView(i * typ.seqElemStride, typ.seqElemType.sizeInBytes), typ.seqElemType)

  mm.allocator.dealloc(oldData)
  s.length = newLen

func grow(s: var VmSeq, typ: PVmType, newLen: Natural, mm: var VmMemoryManager) =
  ## Grows `s` to `newLen`. All items past the old length are zero initialized
  assert newLen > s.length

  let newSize = newLen * typ.seqElemStride

  let newData = mm.allocator.allocTypedLocations(typ.seqElemType, newLen, newSize)
  if s.length > 0:
    # Move data
    safeCopyMemSrc(newData.subView(newSize), s.data.subView(s.length * typ.seqElemStride))
    mm.allocator.dealloc(s.data)
  else:
    assert s.data.isNil

  s.length = newLen
  s.data = newData


func setLenSeq*(s: var VmSeq, typ: PVmType, len: int, mm: var VmMemoryManager) =
  if len > s.length:
    grow(s, typ, len, mm)
  elif len < s.length:
    shrink(s, typ, len, mm)
  else:
    discard # nothing to do

func growBy*(s: var VmSeq, typ: PVmType, num: Natural, mm: var VmMemoryManager) {.inline.} =
  s.grow(typ, s.length + num, mm)

func getItemHandle*(s: VmSeq, typ: PVmType, index: Natural): LocHandle {.inline.} =
  ## Creates a handle to the seq item at `index`
  makeLocHandle(getSubHandle(s.data, typ.seqElemStride * index), typ.seqElemType)

func newVmSeq*(s: var VmSeq, typ: PVmType, numItems: Natural, mm: var VmMemoryManager) =
  # Initializes `s` with new empty memory that is large enough for `numItems`.
  # The seq is destroyed
  if not s.data.isNil:
    destroyVmSeq(s, typ, mm)

  let newData =
    if numItems > 0:
      mm.allocator.allocTypedLocations(typ.seqElemType, numItems, numItems * typ.seqElemStride)
    else:
      nilMemPtr

  s = VmSeq(data: newData, length: numItems)

proc copyVmSeq*(dest: var VmSeq, src: VmSeq, typ: PVmType, mm: var VmMemoryManager) =
  if dest.length > 0:
    destroyVmSeq(dest, typ, mm)

  if src.length > 0:
    let lenInBytes = src.length * typ.seqElemStride
    dest = VmSeq(length: src.length, data: mm.allocator.allocTypedLocations(typ.seqElemType, src.length, lenInBytes))
    arrayCopy(mm, dest.data, src.data, src.length, typ.seqElemType, reset = false)
  else:
    dest = VmSeq()


func newVmString*(s: var VmString, numElements: Natural, a: var VmAllocator) =
  var newData: MemRegionPtr
  if numElements > 0:
    # +1 for the terminating '\0'
    newData = a.allocVmString(numElements)
  else:
    newData = nilMemPtr

  a.dealloc(s.data)
  s = newVmString(newData, numElements)

func newVmString*(s: var VmString, src: openArray[char], a: var VmAllocator) =
  a.dealloc(s.data)
  if src.len > 0:
    s = newVmString(a.allocVmString(src.len), src.len)
    safeCopyMem(s.data.subView(src.len), src, src.len)
  else:
    s = newVmString(nilMemPtr, 0)


template isVariant(x: PVmType): bool = x.branches.len > 0

func asRegionPtr(x: VmMemoryRegion): MemRegionPtr {.inline.} =
  let p = if x.len > 0: unsafeAddr x[0] else: nil
  makeMemPtr(p, uint(x.len))


func resetLocation*(mm: var VmMemoryManager, loc: var VmMemoryRegion, typ: PVmType) =
  assert typ.sizeInBytes <= uint(loc.len)
  let a = cast[ptr Atom](addr loc[0])

  case typ.kind
  of akInt, akFloat, akPtr, akSet, akCallable: discard
  of akString:
    if not a.strVal.data.isNil:
      mm.allocator.dealloc(a.strVal.data)
  of akSeq:
    if not a.seqVal.data.isNil:
      destroyVmSeq(a.seqVal, typ, mm)
  of akRef:
    if not a.refVal.isNil:
      mm.heap.heapDecRef(mm.allocator, a.refVal)
  of akClosure:
    let e = a.closureVal.env
    if not e.isNil:
      mm.heap.heapDecRef(mm.allocator, e)
  of akDiscriminator:
    # The caller has to make sure to never use `resetLocation` on
    # a discriminator outside the context of object resetting
    discard
  of akPNode:
    reset(a.nodeVal)
  of akObject:
    # TODO: track if types are trivial to destroy (no special handling
    #       required) and treat reset as a no-op if they are
    if not typ.isVariant:
      for f in typ.objFields.items:
        let (off, ty) = f
        resetLocation(mm, loc.toOpenArray(off, loc.high), ty)
    else:
      for f in variantFields(asRegionPtr(loc), typ):
        let (off, ty) = f
        resetLocation(mm, loc.toOpenArray(off, loc.high), ty)
  of akArray:
    # TODO: same as for akObject
    var off = 0
    for i in 0..<typ.elementCount:
      resetLocation(mm, loc.toOpenArray(off, loc.high), typ.elementType)
      off += typ.elementStride


func asgnRef*(dst: var HeapSlotHandle, src: HeapSlotHandle, mm: var VmMemoryManager, reset: static[bool]) =
  if not src.isNil: mm.heap.heapIncRef(src)
  when reset:
    if not dst.isNil: mm.heap.heapDecRef(mm.allocator, dst)

  dst = src

func asgnClosure*(dst: var VmClosure, src: VmClosure, mm: var VmMemoryManager, reset: static[bool]) {.inline.} =
  asgnRef(dst.env, src.env, mm, reset)
  dst.fnc = src.fnc


proc copyToLocation*(mm: var VmMemoryManager, dest: var VmMemoryRegion, src: MemRegionPtr, typ: PVmType, reset: static[bool] = true) =
  ## Deep-copy the value with type `typ` at location `src` to `dest`. The
  ## source and destination location must not overlap in memory
  assert typ.sizeInBytes <= uint(dest.len)
  let size = typ.sizeInBytes
  let srcAtom = cast[ptr Atom](addr src.rawPointer[0])
  let dstAtom = cast[ptr Atom](addr dest[0])
  assert dstAtom != srcAtom

  when not reset and defined(debug):
    for b in dest.items:
      if b != 0:
        # We expect zero'ed memory here
        doAssert false

  case typ.kind
  of akInt, akFloat, akPtr, akSet:
    safeCopyMem(dest, src.subView(size), size)
  of akString:
    asgnVmString(dstAtom.strVal, srcAtom.strVal, mm.allocator)
  of akSeq:
    copyVmSeq(dstAtom.seqVal, srcAtom.seqVal, typ, mm)
  of akRef:
    asgnRef(dstAtom.refVal, srcAtom.refVal, mm, reset)
  of akCallable:
    dstAtom.callableVal = srcAtom.callableVal
  of akClosure:
    asgnClosure(dstAtom.closureVal, srcAtom.closureVal, mm, reset)
  of akDiscriminator:
    safeCopyMem(dest, src.subView(size), size)
  of akPNode:
    dstAtom.nodeVal = srcAtom.nodeVal
  of akObject:
    # TODO: track if types are trivial (supports simple mem
    # copy for copying) and then use `copyMem` here when possible
    if not typ.isVariant:
      for f in typ.objFields.items:
        let (off, ty) = f
        copyToLocation(mm, dest.toOpenArray(off, dest.high), src.getSubHandle(off), ty, reset)
    else:
      when reset:
        # TODO: only reset locations that lie in mismatching branches
        resetLocation(mm, dest, typ)
        safeZeroMem(dest, dest.len)

      for f in variantFields(src, typ):
        let (off, ty) = f
        copyToLocation(mm, dest.toOpenArray(off, dest.high), src.getSubHandle(off), ty, reset)

  of akArray:
    # TODO: same as for akObject
    var off = 0
    for i in 0..<typ.elementCount:
      copyToLocation(mm, dest.toOpenArray(off, dest.high), src.getSubHandle(off), typ.elementType, reset)
      off += typ.elementStride


proc resetBranch*(mm: var VmMemoryManager, h: LocHandle, idx: FieldIndex, branch: int) =
  ## Cleanup the given branch of the discriminator at `fieldIndex`. `branch`
  ## must be equal to the currently active branch (not enforced, so watch out)
  var bI = findDiscrBranchEntry(h.typ, idx)

  inc bI
  seekToBranch(bI, branch, h.typ)

  for f in variantFields(h.h, h.typ, uint32(bI)):
    let (off, ty) = f
    resetLocation(mm, h.h.subView(off, ty.sizeInBytes), ty)

  # Zero old branch's memory region
  let branchRange = h.typ.branches[bI].fieldRange
  if branchRange.len > 0:
    let
      start = h.typ.fieldAt(branchRange.a).offset
      lastField = h.typ.fieldAt(branchRange.b)
      last = lastField.offset + lastField.typ.sizeInBytes.int - 1
    safeZeroMem(h.byteView().toOpenArray(start, last), last-start+1)