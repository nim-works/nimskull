#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

const
  TableSize = when sizeof(int) <= 2: 0xff else: 0xff_ffff

type
  PtrTable = ptr object
    counter, max: int
    data: array[TableSize, (pointer, pointer)]

template hashPtr(key: pointer): int = cast[int](key) shr 8
template allocPtrTable: untyped =
  cast[PtrTable](alloc0(sizeof(int)*2 + sizeof(pointer)*2*cap))

proc rehash(t: PtrTable): PtrTable =
  let cap = (t.max+1) * 2
  result = allocPtrTable()
  result.counter = t.counter
  result.max = cap-1
  for i in 0..t.max:
    let k = t.data[i][0]
    if k != nil:
      var h = hashPtr(k)
      while result.data[h and result.max][0] != nil: inc h
      result.data[h and result.max] = t.data[i]
  dealloc t

proc initPtrTable(): PtrTable =
  const cap = 32
  result = allocPtrTable()
  result.counter = 0
  result.max = cap-1

template deinit(t: PtrTable) = dealloc(t)

proc get(t: PtrTable; key: pointer): pointer =
  var h = hashPtr(key)
  while true:
    let k = t.data[h and t.max][0]
    if k == nil: break
    if k == key:
      return t.data[h and t.max][1]
    inc h

proc put(t: var PtrTable; key, val: pointer) =
  if (t.max+1) * 2 < t.counter * 3: t = rehash(t)
  var h = hashPtr(key)
  while t.data[h and t.max][0] != nil: inc h
  t.data[h and t.max] = (key, val)
  inc t.counter

proc genericDeepCopyAux(dest, src: pointer, mt: PNimType;
                        tab: var PtrTable) {.benign.}
proc genericDeepCopyAux(dest, src: pointer, n: ptr TNimNode;
                        tab: var PtrTable) {.benign.} =
  var
    d = cast[ByteAddress](dest)
    s = cast[ByteAddress](src)
  case n.kind
  of nkSlot:
    genericDeepCopyAux(cast[pointer](d +% n.offset),
                       cast[pointer](s +% n.offset), n.typ, tab)
  of nkList:
    for i in 0..n.len-1:
      genericDeepCopyAux(dest, src, n.sons[i], tab)
  of nkCase:
    var dd = selectBranch(dest, n)
    var m = selectBranch(src, n)
    # reset if different branches are in use; note different branches also
    # imply that's not self-assignment (``x = x``)!
    if m != dd and dd != nil:
      genericResetAux(dest, dd)
    copyMem(cast[pointer](d +% n.offset), cast[pointer](s +% n.offset),
            n.typ.size)
    if m != nil:
      genericDeepCopyAux(dest, src, m, tab)
  of nkNone: sysAssert(false, "genericDeepCopyAux")

proc genericDeepCopyAux(dest, src: pointer, mt: PNimType; tab: var PtrTable) =
  var
    d = cast[ByteAddress](dest)
    s = cast[ByteAddress](src)
  sysAssert(mt != nil, "genericDeepCopyAux 2")
  case mt.kind
  of tyString:
      var x = cast[ptr NimStringV2](dest)
      var s2 = cast[ptr NimStringV2](s)[]
      nimAsgnStrV2(x[], s2)
  of tySequence:
      deepSeqAssignImpl(genericDeepCopyAux, tab)
  of tyObject:
    # we need to copy m_type field for tyObject, as it could be empty for
    # sequence reallocations:
    if mt.base != nil:
      genericDeepCopyAux(dest, src, mt.base, tab)
    else:
      var pint = cast[ptr PNimType](dest)
      pint[] = cast[ptr PNimType](src)[]
    genericDeepCopyAux(dest, src, mt.node, tab)
  of tyTuple:
    genericDeepCopyAux(dest, src, mt.node, tab)
  of tyArray, tyArrayConstr:
    for i in 0..(mt.size div mt.base.size)-1:
      genericDeepCopyAux(cast[pointer](d +% i *% mt.base.size),
                         cast[pointer](s +% i *% mt.base.size), mt.base, tab)
  of tyRef:
    let s2 = cast[PPointer](src)[]
    if s2 == nil:
      unsureAsgnRef(cast[PPointer](dest), s2)
    elif mt.base.deepcopy != nil:
      let z = mt.base.deepcopy(s2)
      cast[PPointer](dest)[] = z
    else:
      let z = tab.get(s2)
      if z == nil:
          let typ = if mt.base.kind == tyObject: cast[PNimType](cast[ptr PNimTypeV2](s2)[].typeInfoV1)
                    else: mt.base
          let z = nimNewObj(typ.size, typ.align)
          cast[PPointer](dest)[] = z

          tab.put(s2, z)
          genericDeepCopyAux(z, s2, typ, tab)
      else:
        unsureAsgnRef(cast[PPointer](dest), z)
  of tyPtr:
    # no cycle check here, but also not really required
    let s2 = cast[PPointer](src)[]
    if s2 != nil and mt.base.deepcopy != nil:
      cast[PPointer](dest)[] = mt.base.deepcopy(s2)
    else:
      cast[PPointer](dest)[] = s2
  else:
    copyMem(dest, src, mt.size)

proc genericDeepCopy(dest, src: pointer, mt: PNimType) {.compilerproc.} =
  var tab = initPtrTable()
  genericDeepCopyAux(dest, src, mt, tab)
  deinit tab

proc genericDeepCopyOpenArray(dest, src: pointer, len: int,
                            mt: PNimType) {.compilerproc.} =
  var
    d = cast[ByteAddress](dest)
    s = cast[ByteAddress](src)
  for i in 0..len-1:
    genericDeepCopy(cast[pointer](d +% i *% mt.base.size),
                    cast[pointer](s +% i *% mt.base.size), mt.base)
