## The module implements helper functions for reading and writing values to VM
## locations. These are useful when writing VM callbacks.

import
  compiler/ast/[
    reports
  ],
  compiler/front/[
    options
  ],
  compiler/utils/[
    bitsets
  ],
  compiler/vm/[
    vmdef,
    vmerrors,
    vmobjects,
    vmmemory
  ]

# XXX: A better approach than `tryWrite` is needed if the plan is to wrap
#      large parts of the stdlib... (macros come to mind)
#      VM callback design needs to be revisited in general

proc tryWriteTo*[T: enum](v: T, dest: LocHandle, mm: var VmMemoryManager): bool =
  if dest.typ.kind == akInt:
    result = true
    # XXX: this will silently cut off bits beyond what dest can hold
    writeUInt(dest.byteView(), ord(v))


proc tryWriteTo*[T: SomeSignedInt](v: T, dest: LocHandle, mm: var VmMemoryManager): bool =
  if dest.typ.kind == akInt:
    result = true
    # XXX: this will silently cut off bits beyond what dest can hold
    writeInt(dest.byteView(), BiggestInt(v))


proc tryWriteTo*[T: SomeUnsignedInt](v: T, dest: LocHandle, mm: var VmMemoryManager): bool =
  if dest.typ.kind == akInt:
    result = true
    # XXX: this will silently cut off bits beyond what dest can hold
    writeUInt(dest, BiggestUInt(v))

proc tryWriteTo*[T: string](v: T, dest: LocHandle, mm: var VmMemoryManager): bool =
  if dest.typ.kind == akString:
    result = true
    deref(dest).strVal.newVmString(v, mm.allocator)
  else:
    result = false

proc tryWriteTo*[T: seq](v: T, dest: LocHandle, mm: var VmMemoryManager): bool =
  if dest.typ.kind == akSeq:
    deref(dest).seqVal.setLenSeq(dest.typ, v.len, mm)

    let s = deref(dest).seqVal # shallow copy
    for i in 0..<v.len:
      if tryWriteTo(v[i], s.getItemHandle(dest.typ, i), mm): discard
      else: return false

    result = true

proc tryWriteTo*[T: object|tuple](v: T, dest: LocHandle, mm: var VmMemoryManager): bool =
  var fI = 0
  # XXX: this might be bit fuzzy for more complex objects...
  for x in v.fields:
    if tryWriteTo(x, dest.getFieldHandle(FieldPosition(fI)), mm): discard
    else: return false
    inc fI

  result = true

proc writeTo*[T](v: T, dest: LocHandle, mm: var VmMemoryManager) =
  if tryWriteTo(v, dest, mm):
    return

  raiseVmError(VMReport(
    kind: rvmErrInternal,
    str: "Writing to location failed: " & $T))


func tryReadTo*[T](src: LocHandle, dst: var set[T]): bool =
  const ElemRange = low(T)..high(T)
  if src.typ.kind == akSet and src.typ.setLength <= len(ElemRange):
    # TODO: do something more efficient than testing for each possible element
    for i in ElemRange:
      if bitSetIn(src.byteView(), ord(i)):
        dst.incl i
    result = true

func readAs*[T](src: LocHandle, t: typedesc[T]): T =
  if tryReadTo(src, result):
    return

  raiseVmError(VMReport(
    kind: rvmErrInternal,
    str: "Reading from location failed"))
