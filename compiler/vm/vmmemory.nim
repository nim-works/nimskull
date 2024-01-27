## This module implements the allocator and also contains routines for working
## with handles.

# XXX: module maybe needs a better name?
# XXX: lots of duplicated code here

import
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

const FailureCodeToEvent* = [
  dfcNil:          vmEvtNilAccess,
  dfcInvalid:      vmEvtAccessOutOfBounds,
  dfcFreed:        vmEvtAccessOutOfBounds,
  dfcTypeMismatch: vmEvtAccessTypeMismatch
]

{.push stackTrace: off.}

template next(c: VmCell): untyped =
  c.count

template `next=`(c: var VmCell, id: CellId) =
  c.count = id

func addCell(a: var VmAllocator, p: pointer, c: sink VmCell): CellId =
  if a.freeHead == a.freeTail:
    # no more items in the linked list. There might still be more free slots in
    # the sequence, however.
    if a.freeTail >= a.cells.len:
      # no more free cell slots exists -> allocate more
      a.cells.setLen(max(16, a.cells.len * 3 div 2))

    inc a.freeTail # append a new free slot at the end
    a.cells[a.freeHead].next = a.freeTail

  # pop the first item from the free-cell list:
  result = a.freeHead
  a.freeHead = a.cells[result].next

  # update the cell slot:
  assert a.cells[result].p == nil, "cell not free"
  c.p = cast[ptr UncheckedArray[byte]](p)
  {.cast(noSideEffect).}: # erroneously inferred side-effect
    a.cells[result] = c

template sfAlloc(num: Natural): untyped =
  ## Side-effect free alloc that rounds the allocated size to the next multiple
  ## of the size of an atom. The side-effect (i.e. modifcation of the native
  ## allocator state) is handled by the ``var VmAllocator`` parameter (this is
  ## not entirely correct, but good enough)
  const AtomSize = uint sizeof(Atom)
  {.cast(noSideEffect).}:
    alloc0((uint(num) + AtomSize - 1) div AtomSize * AtomSize)

func allocLocationMemory*(a: var VmAllocator, len: Natural): CellId {.used.} =
  # TODO: rename to `allocUntypedMemory`
  # XXX: not used right now, but will be eventually
  a.addCell(sfAlloc(len), VmCell(sizeInBytes: len))

func allocTypedLocations*(a: var VmAllocator, typ: PVmType; count, lenInBytes: Natural): CellPtr =
  let p = sfAlloc(lenInBytes)
  discard a.addCell(p, VmCell(typ: typ, count: count, sizeInBytes: lenInBytes))

  result = cast[CellPtr](p)

func allocSingleLocation*(a: var VmAllocator, typ: PVmType): LocHandle =
  let
    numBytes = typ.sizeInBytes
    p = sfAlloc(numBytes)
    cell = a.addCell(p, VmCell(typ: typ, count: 1, sizeInBytes: int(numBytes)))

  result = LocHandle(cell: cell, typ: typ, p: cast[VmMemPointer](p))

func allocConstantLocation*(a: var VmAllocator, typ: PVmType): LocHandle =
  # XXX: currently the same as allocSingleLocation, but in the future,
  # constants should be allocated in special memory that is marked as
  # read-only
  allocSingleLocation(a, typ)

func mapPointerToCell*(a: VmAllocator, p: CellPtr): CellId =
  ## Maps a cell pointer to the corresponding cell id, or -1 if the pointer
  ## is not a valid cell pointer.
  if p.isNil:
    return -1

  for id in 0..<a.freeTail:
    if a.cells[id].p == pointer(p):
      return id

  result = -1

func mapInteriorPointerToCell(a: VmAllocator, p: pointer): CellId =
  if p.isNil:
    return -1

  let rp = cast[int](p)
  for id in 0..<a.freeTail:
    let
      cell = a.cells[id]
      start = cast[int](cell.p)
    if rp in start..<(start+cell.sizeInBytes):
      return id

  result = -1

func mapToCell*(a: VmAllocator, p: CellPtr): lent VmCell =
  let id = mapPointerToCell(a, p)
  assert id != -1, "pointer wasn't checked"
  result = a.cells[id]

func dealloc*(a: var VmAllocator, c: CellId) =
  ## Frees the cell's memory and marks the cell slot as empty. `c` is required
  ## to name a valid non-empty cell
  # this procedure is not available to guest code so it's okay to use
  # assertions here for now
  assert c != -1
  assert a.cells[c].p != nil, "cell is empty"

  {.noSideEffect.}:
    dealloc(a.cells[c].p)

  # mark the cell as free:
  reset(a.cells[c])

  # prepend the cell to the list of free cells:
  a.cells[c].next = a.freeHead
  a.freeHead = c

func dealloc*(a: var VmAllocator, p: CellPtr) =
  ## Deallocates the cell indicated by the valid cell pointer `p`. A ``nil``
  ## pointer is ignored.
  if pointer(p) != nil:
    let id = mapPointerToCell(a, p)
    assert id != -1, "pointer wasn't checked"

    dealloc(a, id)

func dealloc*(a: var VmAllocator, handle: LocHandle) {.inline.} =
  ## Deallocates the valid cell `handle` references.
  a.dealloc(handle.cell)

func makeLocHandle*(a: VmAllocator, p: pointer, typ: PVmType): LocHandle =
  ## Attempts to create a handle to the guest memory location that to host
  ## address `p` maps to. A handle signaling "invalid" is returned if no
  ## mapping exists.
  let id = mapInteriorPointerToCell(a, p)
  LocHandle(cell: id, p: cast[VmMemPointer](p), typ: typ)

func makeLocHandle*(a: VmAllocator, cp: CellPtr, offset: Natural, typ: PVmType
                   ): LocHandle =
  ## Attempts to create a handle to an interior location of the cell
  ## coressponding to `cp`. If `cp` is not a valid cell pointer, a handle that
  ## signals "invalid" is returned.
  let id = mapPointerToCell(a, cp)
  LocHandle(cell: id, p: applyOffset(cp, uint(offset)), typ: typ)

func loadFullSlice*(a: VmAllocator, cp: CellPtr, typ: PVmType): VmSlice =
  ## Attempts to create and returns a slice with item type `typ` covering all
  ## locations of the sequence cell corresponding to `cp`. Returns a slice
  ## signaling invalid if that's not possible.
  let id = mapPointerToCell(a, cp)
  # XXX: don't use assertions for ensuring that some expectations hold. While
  #      it would work now, it's not future proof
  VmSlice(cell: id, start: cast[VmMemPointer](cp), len: a.cells[id].count,
          typ: typ)

template internalSlice(p: VmMemPointer | CellPtr, l, h: Natural): untyped =
  var x = p # assign to a `var` first. This allows for using the resulting
            # `openArray` for mutations
  toOpenArray(x.rawPointer, l, h)

template `[]`*[T](p: VmMemPointer | CellPtr, s: Slice[T]): untyped =
  internalSlice(p, s.a, s.b)

template slice*(p: VmMemPointer | CellPtr, len: Natural): untyped =
  internalSlice(p, 0, int(len) - 1)

template slice*(p: VmMemPointer | CellPtr, offset, len: Natural): untyped =
  let o = int(offset) # warning: evaluation order differs from parameter order
  internalSlice(p, o, o + int(len) - 1)

func subLocation*(h: LocHandle, offset: Natural, typ: PVmType): LocHandle {.inline.} =
  ## Creates a handle to the interior location located at the relative
  ## `offset`, using `typ` for the handle's type.
  assert h.isValid
  assert h.typ.kind in pseudoAtomKinds
  LocHandle(cell: h.cell, p: applyOffset(h.p, uint(offset)), typ: typ)

func getFieldHandle*(h: LocHandle, idx: FieldIndex): LocHandle {.inline.} =
  let f = h.typ.fieldAt(idx)
  subLocation(h, f.offset, f.typ)

# TODO: maybe rename to getHandleToField? makeHandleToField?
func getFieldHandle*(loc: LocHandle, pos: FieldPosition): LocHandle =
  ## Creates a handle to the field with position `pos` of the object at
  ## location `h`
  let (typ, adjusted) =
    if loc.typ.relFieldStart == 0: # common case
      (loc.typ, FieldIndex(pos))
    else:
      getFieldAndOwner(loc.typ, pos)

  let f = typ.fieldAt(adjusted)
  subLocation(loc, f.offset, f.typ)

func deref*(handle: LocHandle): ptr Atom {.inline.} =
  assert handle.isValid()
  cast[ptr Atom](handle.p)

template byteView*(c: VmCell): untyped =
  let x = c
  toOpenArray(x.p, 0, x.sizeInBytes-1)

template byteView*(handle: LocHandle): untyped =
  var x = handle
  toOpenArray(x.rawPointer, 0, int(x.typ.sizeInBytes - 1))

template byteView*(slice: VmSlice): untyped =
  var x = slice
  toOpenArray(x.start.rawPointer, 0, int(uint(x.len) * alignedSize(x.typ)) - 1)

{.pop.}

func heapNew*(heap: var VmHeap, a: var VmAllocator, typ: PVmType): HeapSlotHandle =
  ## Creates a new managed slot of type `typ` and sets the ref-count to 1
  assert heap.slots.len > 0
  result = heap.slots.len
  # XXX: slots are currently not reused
  heap.slots.add(HeapSlot(handle: a.allocSingleLocation(typ), refCount: 1))

func heapIncRef*(heap: var VmHeap, slot: HeapSlotHandle) =
  ## Increments the ref-counter for the given slot (expected to be valid)
  assert not slot.isNil
  assert heap.slots[slot].handle.isValid
  assert heap.slots[slot].refCount > 0
  inc heap.slots[slot].refCount

func heapDecRef*(heap: var VmHeap, a: var VmAllocator, slot: HeapSlotHandle) =
  ## Decrements the ref-counter for the given slot (expected to be valid).
  ## If the counter reaches zero, the slot is added to the list of slots
  ## pending clean-up
  assert not slot.isNil
  assert heap.slots[slot].handle.isValid
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
  for r in a.cells.items:
    result += r.sizeInBytes.uint
