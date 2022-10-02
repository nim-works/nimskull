## This module implements the allocator and also contains functions for working
## with handles.

# XXX: module maybe needs a better name?
# XXX: lots of duplicated code here

import
  compiler/ast/[
    ast_types,
    report_enums
  ],
  compiler/vm/[
    vmdef,
    vmtypes
  ],
  std/[
    options
  ],
  experimental/[
    results
  ]

type DerefFailureCode* = enum
  dfcNil          ## nil-access
  dfcInvalid      ## not a valid ref
  dfcFreed        ## ref's target was already freed
  dfcTypeMismatch ## ref's type is not compatible with the target's type

const FailureCodeToReport* = [
  dfcNil:          rvmNilAccess,
  dfcInvalid:      rvmAccessOutOfBounds,
  dfcFreed:        rvmAccessOutOfBounds,
  dfcTypeMismatch: rvmAccessTypeMismatch
]

# TODO: rename to `allocUntypedMemory`
func allocLocationMemory*(a: var VmAllocator, len: Natural): MemRegionPtr =
  {.noSideEffect.}:
    let rounded = (len + sizeof(Atom) - 1) /% sizeof(Atom) * sizeof(Atom)
    let p = alloc0(rounded)
    result = makeMemPtr(p, rounded.uint)
  a.regions.add((nil, 0, cast[int](p), rounded))

func allocTypedLocations*(a: var VmAllocator, typ: PVmType; count, lenInBytes: Natural): MemRegionPtr =
  {.noSideEffect.}:
    let rounded = (lenInBytes + sizeof(Atom) - 1) /% sizeof(Atom) * sizeof(Atom)
    let p = alloc0(rounded)
    result = makeMemPtr(p, rounded.uint)
  a.regions.add((typ, count, cast[int](p), rounded))

func allocSingleLocation*(a: var VmAllocator, typ: PVmType): LocHandle =
  {.noSideEffect.}:
    let rounded = (int(typ.sizeInBytes) + sizeof(Atom) - 1) /% sizeof(Atom) * sizeof(Atom)
    let p = alloc0(rounded)
    result = makeLocHandle(p, typ)
  a.regions.add((typ, 1, cast[int](p), rounded))

func allocAtomLocation*(a: var VmAllocator, typ: PVmType): LocHandle =
  assert typ.kind in realAtomKinds
  {.noSideEffect.}:
    let p = create(Atom)
    result = makeLocHandle(p, typ)
  a.regions.add((typ, 1, cast[int](p), sizeof(Atom)))

func allocConstantLocation*(a: var VmAllocator, typ: PVmType): LocHandle =
  # XXX: currently the same as allocSingleLocation, but in the future,
  # constants should be allocated in special memory that is marked as
  # read-only
  allocSingleLocation(a, typ)


func dealloc*(a: var VmAllocator, p: MemRegionPtr) =
  {.noSideEffect.}:
    if p.rawPointer != nil:
      let rp = cast[int](p.rawPointer)
      var ri = -1
      for (i, x) in a.regions.pairs:
        if x.start == rp:
          ri = i
          break

      # This function is not available to guest code so it's
      # okay to use an assert here
      assert ri != -1
      a.regions.del(ri)

      dealloc(p.rawPointer)


func dealloc*(a: var VmAllocator, handle: LocHandle) {.inline.} =
  a.dealloc(handle.h)

# XXX: maybe use `[]` operator overload instead?
template subView*(p: MemRegionPtr, len: Natural): VmMemoryRegion =
  ## Shortcut for `p.byteView.subView(0, len-1)`
  assert len == 0 or p.isValid(uint(len))

  # Trick the compiler into giving us a mutable `openArray` by having `v` as
  # var (let wont work)
  var v = p.rawPointer
  toOpenArray(v, 0, int(len)-1)

template subView*(p: MemRegionPtr, offset, len: Natural): VmMemoryRegion =
  ## Shortcut for `p.byteView.subView(offset, len-1)`
  assert p.isValid(uint(int(offset) + int(len)))
  var v = p.rawPointer
  toOpenArray(v, offset, int(offset)+int(len)-1)

# TODO: rename to something more fitting
func getSubHandle*(h: MemRegionPtr, offset: Natural): MemRegionPtr {.inline.} =
  assert h.len > offset
  assert not(h.isNil)
  makeMemPtr(addr h.rawPointer[offset], uint(h.len - offset))

func getSubHandle*(h: LocHandle, offset: Natural, typ: PVmType): LocHandle {.inline.} =
  assert h.h.isValid(uint(offset) + typ.sizeInBytes)
  assert h.typ.kind in pseudoAtomKinds
  makeLocHandle(h.h.getSubHandle(offset), typ)

func getFieldHandle*(h: LocHandle, idx: FieldIndex): LocHandle {.inline.} =
  let f = h.typ.fieldAt(idx)
  h.getSubHandle(f.offset, f.typ)

# TODO: maybe rename to getHandleToField? makeHandleToField?
func getFieldHandle*(h: LocHandle, pos: FieldPosition): LocHandle =
  ## Creates a handle to the field with position `pos` of the object at
  ## location `h`
  let (typ, adjusted) =
    if h.typ.relFieldStart == 0: # common case
      (h.typ, FieldIndex(pos))
    else:
      getFieldAndOwner(h.typ, pos)

  let f = typ.fieldAt(adjusted)
  h.getSubHandle(f.offset, f.typ)

func deref*(handle: LocHandle): ptr Atom {.inline.} =
  # let x = handle
  assert handle.isValid()
  cast[ptr Atom](handle.h.rawPointer)

template byteView*(handle: LocHandle): untyped =
  handle.h.subView(handle.typ.sizeInBytes)

func heapNew*(heap: var VmHeap, a: var VmAllocator, typ: PVmType): HeapSlotHandle =
  ## Creates a new managed slot of type `typ` and sets the ref-count to 1
  assert heap.slots.len > 0
  result = heap.slots.len
  # XXX: slots are currently not reused
  heap.slots.add(HeapSlot(handle: a.allocSingleLocation(typ), refCount: 1))

func heapIncRef*(heap: var VmHeap, slot: HeapSlotHandle) =
  ## Increments the ref-counter for the given slot (expected to be valid)
  assert not slot.isNil
  assert not heap.slots[slot].handle.h.isNil
  assert heap.slots[slot].refCount > 0
  inc heap.slots[slot].refCount

func heapDecRef*(heap: var VmHeap, a: var VmAllocator, slot: HeapSlotHandle) =
  ## Decrements the ref-counter for the given slot (expected to be valid).
  ## If the counter reaches zero, the slot is added to the list of slots
  ## pending clean-up
  assert not slot.isNil
  assert not heap.slots[slot].handle.h.isNil
  assert heap.slots[slot].refCount > 0

  if heap.slots[slot].refCount > 1:
    dec heap.slots[slot].refCount
  else:
    heap.slots[slot].refCount = 0
    heap.pending.add(slot)

func isValid*(heap: VmHeap, slot: HeapSlotHandle): bool =
  ## Tests if `slot` is valid. A `HeapSlotHandle` is valid if it references an
  ## existing slot that is not empty or awaiting clean-up
  result =
    slot in 1..<heap.slots.len and
    heap.slots[slot].refCount > 0

func unsafeDeref*(heap: VmHeap, slot: HeapSlotHandle): LocHandle =
  ## Retrieves the managed location's handle without safety checks. Only use
  ## this if you're sure that `slot` is valid
  assert heap.isValid(slot)
  heap.slots[slot].handle

func tryDeref*(heap: VmHeap, slot: HeapSlotHandle): Option[LocHandle] =
  ## Tries to retrieve the managed location's handle. Returns the handle on
  ## success, or `none` if `slot` doesn't reference a valid slot
  if heap.isValid(slot):
    some(heap.slots[slot].handle)
  else:
    none(LocHandle)


func tryDeref*(heap: VmHeap, slot: HeapSlotHandle, typ: PVmType): Result[LocHandle, DerefFailureCode] =
  ## Tries to load the managed location's handle. If `typ` is not nil, the
  ## location's type must be either the same or a sub-type of `typ`. If the
  ## type check fails or if `slot` doesn't reference a valid slot, the failure
  ## code is returned. Otherwise, a handle to the location is returned

  # TODO: reorder the if/elif blocks so that the valid case is the topmost
  if slot == HeapSlotHandle(0):
    # nil access
    result.initFailure(dfcNil)
  elif slot >= HeapSlotHandle(heap.slots.len):
    # Invalid slot
    result.initFailure(dfcInvalid)
  elif heap.slots[slot].refCount == 0:
    # Freed slot
    result.initFailure(dfcFreed)
  else:
    let h = heap.slots[slot].handle

    # The slot's type must be the same or a sub-type of the given `typ`
    if typ == noneType or getTypeRel(h.typ, typ) in {vtrSame, vtrSub}:
      result.initSuccess(h)
    else:
      result.initFailure(dfcTypeMismatch)

func getUsedMem*(a: VmAllocator): uint =
  ## Calculates and returns the combined size-in-bytes of all current allocations
  for r in a.regions.items:
    result += r.len.uint
