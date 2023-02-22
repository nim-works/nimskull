#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Various helpers for interacting with registers from inside a VM callback
## implementations

import
  compiler/ast/[
    ast_types
  ],
  compiler/utils/[
    pathutils
  ],
  compiler/vm/[
    vmdef,
    vmmemory,
    vmobjects
  ]

# XXX: proper error handling is missing here. Since these functions are exposed
# via the compilerapi, `doAssert` is used for pre-condition checking

template setX(k, f) {.dirty.} =
  doAssert a.slots[a.ra].kind == k
  a.slots[a.ra].f = v

proc setResult*(a: VmArgs; v: BiggestInt) = setX(rkInt, intVal)
proc setResult*(a: VmArgs; v: BiggestFloat) = setX(rkFloat, floatVal)
proc setResult*(a: VmArgs; v: bool) =
  let v = v.ord
  setX(rkInt, intVal)

proc setResult*(a: VmArgs; v: openArray[char]) =
  assert a.slots[a.ra].handle.typ.kind == akString
  deref(a.slots[a.ra].handle).strVal.newVmString(v, a.mem.allocator)

proc setResult*(a: VmArgs; v: AbsoluteDir) = setResult(a, v.string)

proc setResult*(a: VmArgs, v: PNode) = setX(rkNimNode, nimNode)

func getResultHandle*(a: VmArgs): LocHandle =
  # XXX: doAssert is used since this function is exposed to non-compiler
  # code (i.e. compilerapi). Handling these kinds of errors as well as
  # VM-as-part-of-compilerapi need additional thought put into them
  doAssert a.slots[a.ra].kind == rkLocation, "result value is stored in a register"
  a.slots[a.ra].handle

template getReg(a, i): untyped =
  doAssert i < a.rc-1
  a.slots[i+a.rb+1].unsafeAddr

template getX(k, field): untyped {.dirty.} =
  let p = getReg(a, i)
  doAssert p.kind == k
  p.field

template getAtomX(k, field): untyped {.dirty.} =
  let p = getReg(a, i)
  doAssert p.kind in {rkHandle, rkLocation}
  doAssert p.handle.typ.kind == k
  deref(p.handle).field

proc numArgs*(a: VmArgs): int =
  result = a.rc-1


func getInt*(a: VmArgs; i: Natural): BiggestInt = getX(rkInt, intVal)
func getBool*(a: VmArgs; i: Natural): bool = getX(rkInt, intVal) != 0
func getFloat*(a: VmArgs; i: Natural): BiggestFloat = getX(rkFloat, floatVal)
func getNode*(a: VmArgs, i: Natural): PNode = getX(rkNimNode, nimNode)
# TODO: try to use openArray[char] wherever possible in `vmops.nim` and return a `lent VmString` here
func getString*(a: VmArgs; i: Natural): string = $getAtomX(akString, strVal)

func getHandle*(a: VmArgs; i: Natural): LocHandle =
  let p = getReg(a, i)
  doAssert p.kind in {rkHandle, rkLocation}
  p.handle


proc getVar*(a: VmArgs; i: Natural): LocHandle =
  # XXX: vmops don't have access to mutable int,float,ptr,NimNode paramenters right now.
  # Once rkRegAddr is gone and vmgen properly handles the aforementioned types, the issue
  # is no more
  let si = i+a.rb+1
  assert a.slots[si].kind == rkAddress

  # The address was validate by the VM prior the invoking the callback
  makeLocHandle(a.slots[si].addrVal, a.slots[si].addrTyp)
