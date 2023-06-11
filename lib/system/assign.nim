#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The run-time copy and reset implementation used by the `deepCopy`
## implementation and `typeinfo` module.

include seqs_v2_reimpl

proc genericResetAux(dest: pointer, n: ptr TNimNode) {.benign.}

proc genericAssignAux(dest, src: pointer, mt: PNimType, shallow: bool) {.benign.}
proc genericAssignAux(dest, src: pointer, n: ptr TNimNode,
                      shallow: bool) {.benign.} =
  var
    d = cast[ByteAddress](dest)
    s = cast[ByteAddress](src)
  case n.kind
  of nkSlot:
    genericAssignAux(cast[pointer](d +% n.offset),
                     cast[pointer](s +% n.offset), n.typ, shallow)
  of nkList:
    for i in 0..n.len-1:
      genericAssignAux(dest, src, n.sons[i], shallow)
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
      genericAssignAux(dest, src, m, shallow)
  of nkNone: sysAssert(false, "genericAssignAux")
  #else:
  #  echo "ugh memory corruption! ", n.kind
  #  quit 1

when true:
  template deepSeqAssignImpl(operation, additionalArg) {.dirty.} =
    var d = cast[ptr NimSeqV2Reimpl](dest)
    var s = cast[ptr NimSeqV2Reimpl](src)
    d.len = s.len
    let elem = mt.base
    d.p = cast[ptr NimSeqPayloadReimpl](newSeqPayload(s.len, elem.size, elem.align))

    let bs = elem.size
    let ba = elem.align
    let headerSize = align(sizeof(NimSeqPayloadBase), ba)

    for i in 0..d.len-1:
      operation(d.p +! (headerSize+i*bs), s.p +! (headerSize+i*bs), mt.base, additionalArg)

proc genericAssignAux(dest, src: pointer, mt: PNimType, shallow: bool) =
  var
    d = cast[ByteAddress](dest)
    s = cast[ByteAddress](src)
  sysAssert(mt != nil, "genericAssignAux 2")
  case mt.kind
  of tyString:
    var x = cast[ptr NimStringV2](dest)
    var s2 = cast[ptr NimStringV2](s)[]
    nimAsgnStrV2(x[], s2)
  of tySequence:
    deepSeqAssignImpl(genericAssignAux, shallow)
  of tyObject:
    var it = mt.base
    # don't use recursion here on the PNimType because the subtype
    # check should only be done at the very end:
    while it != nil:
      genericAssignAux(dest, src, it.node, shallow)
      it = it.base
    genericAssignAux(dest, src, mt.node, shallow)
    # we need to copy m_type field for tyObject, as it could be empty for
    # sequence reallocations:
    var pint = cast[ptr PNimTypeV2](dest)
    #chckObjAsgn(cast[ptr PNimTypeV2](src)[].typeInfoV2, mt)
    pint[] = cast[PNimTypeV2](mt.typeInfoV2)
  of tyTuple:
    genericAssignAux(dest, src, mt.node, shallow)
  of tyArray, tyArrayConstr:
    for i in 0..(mt.size div mt.base.size)-1:
      genericAssignAux(cast[pointer](d +% i *% mt.base.size),
                       cast[pointer](s +% i *% mt.base.size), mt.base, shallow)
  of tyRef:
    unsureAsgnRef(cast[PPointer](dest), cast[PPointer](s)[])
  else:
    copyMem(dest, src, mt.size) # copy raw bits

proc genericAssign(dest, src: pointer, mt: PNimType) {.compilerproc.} =
  # still needs to be a compilerproc for ``typeinfo`` to be able to import it
  genericAssignAux(dest, src, mt, false)

when false:
  proc debugNimType(t: PNimType) =
    if t.isNil:
      cprintf("nil!")
      return
    var k: cstring
    case t.kind
    of tyBool: k = "bool"
    of tyChar: k = "char"
    of tyEnum: k = "enum"
    of tyArray: k = "array"
    of tyObject: k = "object"
    of tyTuple: k = "tuple"
    of tyRange: k = "range"
    of tyPtr: k = "ptr"
    of tyRef: k = "ref"
    of tyVar: k = "var"
    of tySequence: k = "seq"
    of tyProc: k = "proc"
    of tyPointer: k = "range"
    of tyOpenArray: k = "openarray"
    of tyString: k = "string"
    of tyCstring: k = "cstring"
    of tyInt: k = "int"
    of tyInt32: k = "int32"
    else: k = "other"
    cprintf("%s %ld\n", k, t.size)
    debugNimType(t.base)

# ---------------------- assign zero -----------------------------------------

proc genericReset(dest: pointer, mt: PNimType) {.benign.}
proc genericResetAux(dest: pointer, n: ptr TNimNode) =
  var d = cast[ByteAddress](dest)
  case n.kind
  of nkNone: sysAssert(false, "genericResetAux")
  of nkSlot: genericReset(cast[pointer](d +% n.offset), n.typ)
  of nkList:
    for i in 0..n.len-1: genericResetAux(dest, n.sons[i])
  of nkCase:
    var m = selectBranch(dest, n)
    if m != nil: genericResetAux(dest, m)
    zeroMem(cast[pointer](d +% n.offset), n.typ.size)

proc genericReset(dest: pointer, mt: PNimType) =
  var d = cast[ByteAddress](dest)
  sysAssert(mt != nil, "genericReset 2")
  case mt.kind
  of tyRef:
    unsureAsgnRef(cast[PPointer](dest), nil)
  of tyString:
    var s = cast[ptr NimStringV2](dest)
    frees(s[])
    zeroMem(dest, mt.size)
  of tySequence:
    frees(cast[ptr NimSeqV2Reimpl](dest)[])
    zeroMem(dest, mt.size)
  of tyTuple:
    genericResetAux(dest, mt.node)
  of tyObject:
    genericResetAux(dest, mt.node)
    # also reset the type field for tyObject, for correct branch switching!
    var pint = cast[ptr PNimTypeV2](dest)
    pint[] = nil
  of tyArray, tyArrayConstr:
    for i in 0..(mt.size div mt.base.size)-1:
      genericReset(cast[pointer](d +% i *% mt.base.size), mt.base)
  else:
    zeroMem(dest, mt.size) # set raw bits to zero
