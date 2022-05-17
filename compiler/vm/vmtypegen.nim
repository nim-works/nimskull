## This module implements the `PType` to `VmType` translation. No duplicate
## types are allowed, making type comparison via ID (currently `PVmType`)
## possible.
##
## The `distinct` modifier is ignored (`distinct int` maps to the same type
## as just `int`) and named tuples are treated as unnamed tuples.

import
  compiler/ast/[
    ast_types,
    ast,
    reports,
    types
  ],
  compiler/front/[
    options
  ],
  compiler/vm/[
    vmdef
  ],

  std/[
    hashes,
    tables
  ]

import std/options as soptions

from std/bitops import bitand, fastLog2

const EmptySize = 4 ## the size of empty object/tuple/array types

template paddedSize(s, a: uint): untyped =
  ## Round the size up so that `s % a == 0`
  bitand(s + a - 1, not (a - 1))

template paddedSize(info: VmType): untyped =
  ## Round the size up so that `info.sizeInBytes % (1 shl info.alignment) == 0`
  paddedSize(info.sizeInBytes, 1'u shl info.alignment)


func getOrCreateFuncType*(c: var TypeInfoCache, typ: PType): FunctionTypeId

const skipTypeSet = abstractRange+{tyStatic}-{tyTypeDesc}

func getAtomicType(cache: TypeInfoCache, conf: ConfigRef, t: PType): Option[PVmType] =
  # XXX: if the type enums had a better ordering, the logic here could be a
  #      simplified by using enum-array lookup. See comment in `TypeInfoCache`
  #      definition
  let r =
    case t.kind
    of tyBool: cache.boolType
    of tyChar: cache.charType
    of tyCstring, tyString: cache.stringType
    of tyInt..tyInt64: cache.intTypes[t.kind]
    of tyUInt..tyUInt64: cache.uintTypes[t.kind]
    of tyFloat..tyFloat64: cache.floatTypes[t.kind]
    of tyPointer, tyNil: cache.pointerType
    of tyUntyped, tyTyped, tyTypeDesc: cache.nodeType # XXX: is this really
                        # needed? Is a field of one of these types possible?
    of tyRef:
      if t.sym != nil and t.sym.magic == mPNimrodNode: cache.nodeType
      else: nil
    of tyEnum:
      # This mirrors the logic for `tyEnum` in `types.getSize`
      {.cast(noSideEffect).}: # cast away the reporting related side effects
                              # of `firstOrd`/`lastOrd`
        if firstOrd(conf, t) < Zero:
          cache.intTypes[tyInt32]
        else:
          let lastOrd = toInt64(lastOrd(conf, t))   # BUGFIX: use lastOrd!
          let tk =
            if lastOrd < (1 shl 8): tyUInt8
            elif lastOrd < (1 shl 16): tyUInt16
            elif lastOrd < (BiggestInt(1) shl 32): tyUInt32
            else: tyUInt64
          cache.uintTypes[tk]
    else:
      nil

  result = option(r)

func genDiscriminator(conf: ConfigRef, typ: PType): VmType =
  ## Builds a `VmType` for a discriminator of type `typ`

  # Ignore the reporting related side-effects of `lastOrd`
  {.noSideEffect.}:
    let limit = toInt(lastOrd(conf, typ))

  # high(enum) might be zero so we have to guard against that case with `max`
  let numBits = fastLog2(max(limit, 1)) + 1

  # round the number of bytes to either 1 or the next power of two
  let exponent = fastLog2(((numBits * 2) + 7) /% 8)
  let sizeInBytes = 1 shl exponent

  VmType(kind: akDiscriminator, sizeInBytes: uint(sizeInBytes), alignment: uint8(exponent), numBits: numBits)


func addObjectType(c: var TypeInfoCache): PVmType =
  result = PVmType(kind: akObject)
  c.types.add(result)

func addTupleType(c: var TypeInfoCache, t: sink VmType): PVmType =
  result = PVmType()
  result[] = t
  # We don't depend on the order of object/tuple/atomic types, so in order get
  # around a costly seq insert, we simply add the tuple type, swap it with the
  # first object type and move the object start index
  # XXX: in theory, we could do this optimization at the AtomKind level. Would
  #      allow us to get rid of the `it.atomKind == ...` checks with
  #      `findAtomicType`. Not all atom kinds are produced by `genType`
  #      however.
  c.types.add(result)
  swap(c.types[c.startObjects], c.types[c.types.high])
  inc c.startObjects

func addAtomicType(c: var TypeInfoCache, t: sink VmType): PVmType =
  result = PVmType()
  result[] = t
  # See `addTupleType` for what the following does
  c.types.add(result)
  # XXX: swap where `addr a == addr b` works, but violates parameter aliasing
  #      rules
  swap(c.types[c.startObjects], c.types[c.types.high])
  swap(c.types[c.startTuples], c.types[c.startObjects])
  inc c.startObjects
  inc c.startTuples

type GenClosure* = object
  queue: seq[(PVmType, PType)]
  tmpFields: seq[PVmType]
  sizeQueue: seq[PVmType] ## newly created types that need to have their size
                          ## computed
  conf: ConfigRef

type GenResult = object
  existing: PVmType
  typ: VmType

template findAtomicType(c: TypeInfoCache, cmp: untyped): untyped =
  ## Searches for a non-object type that consists of only a single field.
  ## Use `it` for accessing the current type in the `cmp` expression
  var res: PVmType
  for i in c.startAtomic..<c.startTuples:
    let it {.inject.} = c.types[i]
    if cmp:
      res = it
      break
  res

func skipTypesGetInst(t: PType): tuple[inst: PType, concrete: PType] =
  ## Skips all types who's kind matches `skipTypeSet`. Returns the first
  ## non-skipped type together with the surrounding `tyGenericInst`, if one
  ## exists (nil otherwise)
  var t = t
  while t.kind in skipTypeSet:
    {.cast(noSideEffect).}:
      # erroneously inferred side-effect
      result.inst =
        if t.kind == tyGenericInst: t
        else: nil

    t = t.lastSon

  {.cast(noSideEffect).}:
    # erroneously inferred side-effect
    result.concrete = t

func getOrAddKnownInstType(c: var TypeInfoCache, inst: PType): (PVmType, bool) =
  let typeId = inst.lastSon.sym.itemId
  let insts = addr c.genericInsts.mgetOrPut(typeId, @[])
  for (lhs, typ) in insts[].items:
    assert lhs.kind == tyGenericInst
    assert lhs.len - 2 == inst.len - 2
    block inner:
      # Compare all arguments
      for i in 1..<lhs.len - 1:
        {.cast(noSideEffect).}:
          # XXX: sameType reads from the global variable `eqTypeFlags`. It's
          #      only mutated at the start of compilation, so casting away
          #      the side-effect should pose little risk
          if not sameType(lhs[i], inst[i]):
            break inner

      # found a match
      return (typ, true)

  # instantiation not yet known
  let r = c.addObjectType()
  insts[].add((inst, r))

  result = (r, false)


func genTuple(c: var TypeInfoCache, t: PType, cl: var GenClosure): GenResult

func genType(c: var TypeInfoCache, t: PType, cl: var GenClosure): tuple[typ: PVmType, existed: bool] =
  ## The heart of `PType` -> `VmType` translation. Looks up or creates types
  ## and adds mappings to `lut` if they don't exist already
  let (inst, t) = t.skipTypesGetInst()
  let typ = getAtomicType(c, cl.conf, t)
  if typ.isSome():
    return (typ.unsafeGet, true)

  if t.itemId in c.lut:
    return (c.lut[t.itemId], true)

  var res: GenResult

  # TODO: skip search for all ref, ptr, array and seq types if the element
  #       type was just created

  case t.kind
  of tyVar, tyLent, tyPtr, tyRef:
    let (et, _) = genType(c, t[0], cl)

    let k = case t.kind
      of tyVar, tyLent, tyPtr: akPtr
      of tyRef: akRef
      else: unreachable()

    res.typ = VmType(kind: k)
    res.typ.targetType = et

    res.existing = findAtomicType(c):
      it.kind == k and it.targetType == et

  of tySequence, tyOpenArray:
    let (et, _) = genType(c, t[0], cl)

    res.typ = VmType(kind: akSeq)
    res.typ.seqElemType = et

    res.existing = findAtomicType(c):
      it.kind == akSeq and it.seqElemType == et

  of tyProc:
    let kind =
      if t.callConv == ccClosure: akClosure
      else: akCallable

    let id = c.getOrCreateFuncType(t)

    res.typ = VmType(kind: kind)
    res.typ.funcTypeId = id

    res.existing = findAtomicType(c):
      it.kind == kind and it.funcTypeId == id

  of tyObject:
    if inst == nil:
      # if the `t` is not in the cache, we've got a new object type

      # object types are special. They're creation gets deferred in order to not
      # run into cyclic dependency issues. With some clever tricks and by using
      # some form of resumeable functions we could get around the deferred
      # creation in some cases, but the added complexity is likely not worth it

      result.typ = c.addObjectType()
    else:
      # Two `tyGenericInst` that refer to same type (e.g. `A[int]` and
      # `A[int]`) don't necessarily have the same item id, so we have to keep
      # track of all known instantiation and manually check if a VmType was
      # already generated for `inst`

      let (typ, exists) = getOrAddKnownInstType(c, inst)
      if exists: res.existing = typ
      else: result.typ = typ

  of tyTuple:
    res = genTuple(c, t, cl)
  of tyArray:
    # XXX: lengthOrd has side-effects due to error reporting right now.
    #      We shouldn't lie to the compiler like this. `lengthOrd` should
    #      probably be side-effect free
    {.noSideEffect.}:
      let L = toInt(lengthOrd(cl.conf, t))

    let (et, _) = genType(c, t[1], cl)

    res.existing = findAtomicType(c):
      it.kind == akArray and it.elementCount == L and it.elementType == et

    res.typ = VmType(kind: akArray,
      elementType: et,
      elementCount: L)
  of tySet:
    # XXX: for now `set`s are separate atoms, but they could be represented
    #      via `akInt` and `akArry` similar to how the C backend does it.
    #      This would first require some language specification regarding
    #      static-/dynamic-type compatibility however.

    {.noSideEffect.}: # Ignore the reporting related side effects
      let L =
        if t[0].kind != tyEmpty: toUInt32(lengthOrd(cl.conf, t[0]))
        else: 0

    res.existing = findAtomicType(c):
      # TODO: sets should be ordered by `setLength`
      it.kind == akSet and it.setLength == int L

    if res.existing == nil:
      res.typ = VmType(kind: akSet, setLength: int L)
      # Calculate the size and alignment for the underlying storage
      (res.typ.sizeInBytes, res.typ.alignment) =
        if L <= 8:    (1'u, 0'u8)
        elif L <= 16: (2'u, 1'u8)
        elif L <= 32: (4'u, 2'u8)
        elif L <= 64: (8'u, 3'u8)
        else: (bitand(uint(L) + 7, not 7'u) div 8, 3'u8)

  of tyUserTypeClass, tyUserTypeClassInst:
    # XXX: do these two even reach here? `cgen` has logic for them, but maybe
    #      that's a historic leftover?
    assert t.isResolvedUserTypeClass
    assert inst == nil

    # XXX: maybe it's not a good idea to add all user-type class instances to
    #      the cache?
    res.existing = c.genType(t.lastSon(), cl).typ
  of tyFloat128:
    # XXX: introducing an error path just for float128 seems excessive, even
    #      more so when float128 seems to be almost never used. sem should
    #      reject this instead, but for now we just raise an internal compiler
    #      error
    # XXX: could also use `globalReport`, but it's not side-effect free
    doAssert false, "float128 not supported by the VM"
  else:
    unreachable()

  if res.existing != nil:
    # The `VmType` the given `typ` maps to already exists, cache the mapping
    result.typ = res.existing
    result.existed = true
    c.lut[t.itemId] = res.existing
  else:
    # A new `VmType`
    if res.typ.sizeInBytes == 0:
      (res.typ.sizeInBytes, res.typ.alignment) = c.staticInfo[res.typ.kind]

    # XXX: as of this commit, there's a compiler bug (presumably with
    #      `injectdestructors`) that leads to `result.typ` being erroneously
    #      sunk into the tuple passed to `cl.queue.add` below, making the
    #      following if check crash due to a nil access, when bootstrapping
    #      with ORC. First assigning to a temporary like this makes the issue
    #      go away
    let temp = result.typ

    if t.kind == tyObject:
      assert result.typ != nil
      # Queue the object to be generated later. See the comment in the
      # `tyObject` of-branch above
      cl.queue.add((temp, t))
    else:
      result.typ =
        if res.typ.kind == akObject:
          c.addTupleType(res.typ)
        else:
          c.addAtomicType(res.typ)

    if result.typ.kind in {akObject, akArray, akSeq}:
      cl.sizeQueue.add(result.typ)

    result.existed = false

    c.lut[t.itemId] = result.typ


func genTuple(c: var TypeInfoCache, t: PType, cl: var GenClosure): GenResult =
  ## Searches all previously created tuple types for a type equivalent to the
  ## `VmType` `t` maps to. If a matching `VmType` is found, it's id (`PVmType`)
  ## is returned. Otherwise, the new `VmType` is returned.
  assert t.n == nil or t.n.len == t.sons.len
  let L = t.sons.len
  if L == 0:
    result.existing = c.emptyType
    return
  #[
  # XXX: breaks too much
  elif L == 1:
    discard genType(c, t[0], false, cl)
    # HACK: `genType` doesn't return an id so we have to use this work-around
    result.existing = c.lut[t[0].skipTypes(skipTypeSet).itemId]
    return
  ]#

  let fieldStart = cl.tmpFields.len
  var isNew = false

  for i in 0..<t.len:
    let (typ, e) = genType(c, t[i], cl)
    isNew = isNew or not e # if the field's type was just created, the tuple
                           # type also doesn't exist yet
    cl.tmpFields.add(typ)

  if not isNew:
    # Iterate over all tuple types and see if one exists that matches the
    # one we're trying to generate
    for i in c.startTuples..<c.startObjects:
      let typ = c.types[i]
      if typ.objFields.len == L:
        var n = 0
        while n < L:
          if typ.objFields[n].typ != cl.tmpFields[fieldStart + n]:
            break
          inc n

        if n == L:
          # a match!
          result.existing = typ
          break

  # set up the type if it doesn't exist yet
  if result.existing == nil:
    result.typ = VmType(kind: akObject)
    # Copy over the field types
    result.typ.objFields.setLen(L)
    for i in 0..<L:
      result.typ.objFields[i].typ = cl.tmpFields[fieldStart + i]

  cl.tmpFields.setLen(fieldStart)

func findDefaultBranch(n: PNode): int =
  ## Returns the 1-based index of the default branch. If no 'of'-branch
  ## contains 0, the else branch (highest index) must cover it
  for i in 1..<n.len:
    let b = n[i]
    for j in 0..<b.len-1: # the last item is the branch's content
      let v = b[j]
      let low =
        if v.kind == nkRange: getOrdValue(v[0])
        else: getOrdValue(v)

      if low == Zero:
        return i

    if b.kind == nkElse:
      return i

  unreachable()

func genRecordNode(c: var TypeInfoCache, dest: var VmType, n: PNode, cl: var GenClosure) =
  ## Recursively walks the given record node, populating `dest` with all
  ## record fields as well as branch walk-list information (if a record-case
  ## is present)

  case n.kind
  of nkSym:
    let (t, _) = genType(c, n.sym.typ, cl)
    dest.objFields.add((0, t))

  of nkRecList:
    for x in n.items:
      genRecordNode(c, dest, x, cl)
  of nkRecCase:
    if dest.branches.len == 0:
      # Add the super-branch
      dest.branches.add(BranchListEntry(kind: blekBranch))

    # discriminator
    block:
      let dTyp = genDiscriminator(cl.conf, n[0].sym.typ)
      var ty = findAtomicType(c):
        # XXX: just testing for `numBits` means that `a: range[0..2]` and
        #      `b: range[0..3]` are treated as the same type!
        it.kind == akDiscriminator and it.numBits == dTyp.numBits

      if ty == nil:
        ty = c.addAtomicType(dTyp)
        # XXX: In order to add a `PType` mapping for discriminators, we'd
        #      need some kind of flags as part of the PType-key.

      dest.objFields.add((0, ty))

    let discIndex = dest.branches.len
    block:
      # In order for a zero-initialized object variant to be valid, the
      # default branch would have to be at index 0. Since that's not
      # necessarily the case, whenever reading or writing the branch index to
      # a location, 0 is mapped to the index of the default branch and vice
      # versa.
      let defaultBranch = findDefaultBranch(n) - 1

      let entry = BranchListEntry(
        kind: blekStart,
        field: FieldIndex(dest.objFields.high),
        defaultBranch: defaultBranch.uint16,
        numBranches: uint16(n.len - 1))

      dest.branches.add(entry)

    for i in 1..<n.len:
      let bI = uint32(dest.branches.len)
      dest.branches.add(BranchListEntry(kind: blekBranch))

      let first = FieldIndex(dest.objFields.len)
      let son = lastSon(n[i])
      genRecordNode(c, dest, son, cl)
      let last = FieldIndex(dest.objFields.high)

      # `fieldRange` is allowed to be empty
      dest.branches[bI].fieldRange = first..last

    # The `b` value for `fieldRange` is filled later by `finishObjectDesc`
    let r = FieldIndex(dest.objFields.len)..FieldIndex(0)
    dest.branches.add(BranchListEntry(kind: blekEnd, fieldRange: r))
    dest.branches[discIndex].numItems = uint32(dest.branches.len - discIndex)

  else:
    unreachable()


func finishObjectDesc(desc: var VmType) =
  ## For variant objects, adds the end entry and fills in some missing branch
  ## entry information

  if desc.branches.len > 0:
    # Set field range for the super-branch and add end entry
    let fieldsHigh = FieldIndex(desc.objFields.high)
    desc.branches[0].fieldRange = FieldIndex(0)..fieldsHigh

    func adjustEnds(list: var openArray[BranchListEntry], endItem: int, i: var int) =
      ## Recursively walks the walk-list, filling in the `fieldRange.b` for
      ## 'end' entries.
      assert list[i].kind == blekBranch

      # XXX: Instead of using recursion, a flat loop with a seq as the
      #      stack could also be used. The downside would be that one or more
      #      heap allocations are required then
      let lastField = list[i].fieldRange.b

      inc i

      while i < endItem:
        case list[i].kind
        of blekBranch:
          return
        of blekStart:
          let it = list[i]
          let e = i + int(it.numItems) - 1
          inc i
          for _ in 0'u16..<it.numBranches:
            adjustEnds(list, e, i)

        of blekEnd:
          list[i].fieldRange.b = lastField

          inc i

    var i = 0
    adjustEnds(desc.branches, desc.branches.len, i)

    # An variant object always has at least one field (a discriminator) so
    # `endRange` is always an empty slice
    let endRange = FieldIndex(desc.objFields.len)..FieldIndex(0)
    desc.branches.add(BranchListEntry(kind: blekEnd, fieldRange: endRange))

func genObject(c: var TypeInfoCache, dst: var VmType, t: PType, cl: var GenClosure) =
  ## Populates `dst`'s field list and branch walk-list
  assert dst.objFields.len == 0 # type must be in an empty state
  var base: PVmType

  if t[0] != nil:
    let pt = t[0].skipTypes(skipPtrs + abstractInst)
    assert pt.kind == tyObject
    base = genType(c, pt, cl).typ
    dst.objFields.add((0, base))
    dst.relFieldStart = 1 # the base type might not be fully set-up yet, so
                          # we defer the `relFieldStart` computation to the
                          # size pass. `relFieldStart` is set to 1 here in
                          # order to signal that inheritance is used
    # XXX: it might be a better idea to merge size/offset computation with
    #      object type generation and use a DAG to store the dependencies
    #      between queued types. This would allow for size computation to
    #      become flat (instead of recursive, as it is now) and also
    #      make the `relFieldStart` computable during object type creation

  genRecordNode(c, dst, t.n, cl)

  finishObjectDesc(dst)

type SizeAlignTuple = tuple[size: uint, align: uint8]

func calcSizeAndAlign(t: var VmType): SizeAlignTuple
  ## Recursively calculates and fills in the `sizeInBytes` and `alignemnt`
  ## information for `t`

func getSizeAndAlign(t: var VmType): SizeAlignTuple {.inline.} =
  ## If `t` already has size information, retrieves it. Calculates and fills in
  ## the information otherwise
  if t.sizeInBytes > 0:
    # likely case
    (t.sizeInBytes, t.alignment)
  else:
    calcSizeAndAlign(t)

func calcFieldOff(fieldSA: SizeAlignTuple, o: var uint, a: var uint8): int {.inline.} =
  o = paddedSize(o, 1'u shl fieldSA.align)
  result = o.int
  o += fieldSA.size
  a = max(a, fieldSA.align)

func calcForRange(fields: var openArray[(int, PVmType)], offset: uint, align: uint8): SizeAlignTuple =
  result = (offset, align)
  for (off, typ) in fields.mitems:
    let fSa = getSizeAndAlign(typ[])
    off = calcFieldOff(fSa, result.size, result.align)

template slice[T, ST](x: seq[T], s: Slice[ST]): untyped =
  let ab = s # don't evaluate `s` multiple times
  toOpenArray(x, ab.a.int, ab.b.int)

func calcBranch(t: var VmType, i: var int, offset: uint, align: uint8): SizeAlignTuple

func calcCase(t: var VmType, i: var int, offset: uint, align: uint8): SizeAlignTuple =
  # The discriminator field is already processed by `calcBranch`
  let numBranches = t.branches[i].numBranches

  inc i # move to first branch

  var start: SizeAlignTuple = (offset, align)
  for b in 0'u32..<numBranches:
    # `calcBranch` updates `i`
    let tmp = calcBranch(t, i, start.size, start.align)
    result.size = max(result.size, tmp.size)
    result.align = max(result.align, tmp.align)

  # Don't skip the union's 'end' entry
  assert t.branches[i].kind == blekEnd

func calcBranch(t: var VmType, i: var int, offset: uint, align: uint8): SizeAlignTuple =
  result.size = offset
  result.align = align

  let b = t.branches[i]
  assert b.kind == blekBranch, $b.kind

  inc i

  var fields = b.fieldRange
  while fields.a <= fields.b:
    let next = addr t.branches[i] # the target of `next` is not mutated
    if next.kind == blekStart:
      # only process fields up to the sub rec-case
      fields.b = next.field

    # This includes the discriminator of the next rec-case (if there is one)
    result = calcForRange(t.objFields.slice(fields), result.size, result.align)

    if next.kind == blekStart:
      result = calcCase(t, i, result.size, result.align)

      let e = addr t.branches[i]
      assert e.kind == blekEnd
      fields = e.fieldRange

      inc i # skip the union's 'end'
    else:
      break


func calcSizeAndAlign(t: var VmType): SizeAlignTuple =
  case t.kind:
  of akObject:
    if t.branches.len == 0:
      result = calcForRange(t.objFields, 0, 0)
    else:
      var i = 0
      result = calcBranch(t, i, 0, 0)
      assert i == t.branches.high

    if result.size == 0:
      result.size = EmptySize

    if t.relFieldStart > 0:
      # The base type is stored as the first field and thus has had it's
      # size and field start already computed by the logic above
      let base = t.objFields[0].typ
      t.relFieldStart = base.relFieldStart + uint32(base.objFields.len)
      if base.relFieldStart > 0:
        # -1 for the base's base field
        t.relFieldStart -= 1
      else:
        # +1 so that all `relFieldStart` values for objects that have bases
        # start at 1. See the documentation for `VmType` for the reason
        # behind this
        t.relFieldStart += 1

  of akArray:
    let (s, a) = getSizeAndAlign(t.elementType[])

    let stride = paddedSize(s, 1'u shl a)
    let sizeInBytes = max(EmptySize.uint, stride * uint(t.elementCount))
      # To not break too much assumptions, arrays have a byte-size of atleast 1

    t.elementStride = stride.int
    result = (sizeInBytes, a)
  else:
    unreachable()

  t.sizeInBytes = result[0]
  t.alignment = result[1]

func genAllTypes(c: var TypeInfoCache, typ: PType, cl: var GenClosure): PVmType =
  result = genType(c, typ, cl).typ

  # generate all queued object types
  var i = 0
  while i < cl.queue.len:
    # new items might be added to the queue while we're iterating
    let (typ, ptyp) = cl.queue[i]
    assert typ.kind == akObject
    genObject(c, typ[], ptyp, cl)

    inc i

  # Calculate the size and alignment for all objects/tuples/arrays
  for ty in cl.sizeQueue:
    case ty.kind:
    of akObject, akArray:
      discard calcSizeAndAlign(ty[])
    of akSeq:
      # seqs are added after their element type, so the element type has it's
      # size calcualated by now
      ty.seqElemStride = int paddedSize(ty.seqElemType[])
    else: unreachable()

proc getOrCreate*(
  c: var TypeInfoCache,
  conf: ConfigRef,
  typ: PType,
  cl: var GenClosure): PVmType {.inline.} =
  ## Lookup or create the `VmType` corresponding to `typ`. If a new type is
  ## created, the `PType` -> `PVmType` mapping is cached
  cl.queue.setLen(0)
  cl.conf = conf

  genAllTypes(c, typ, cl)

# TODO: move this one to `vmgen`
proc getOrCreate*(c: var TCtx, typ: PType): PVmType {.inline.} =
  var cl: GenClosure
  getOrCreate(c.typeInfoCache, c.config, typ, cl)


func lookup*(c: TypeInfoCache, conf: ConfigRef, typ: PType): Option[PVmType] =
  ## Searches the cache for a `VmType` matching the given `typ`. If one exists,
  ## it's returned, otherwise, `none` is returned
  let t = typ.skipTypes(skipTypeSet)
  result = getAtomicType(c, conf, t)
  if result.isNone() and t.itemId in c.lut:
    # XXX: double lookup
    result = some(c.lut[t.itemId])

template hash(x: FuncTypeLutKey): untyped =
  # XXX: the (extremely) simple hash function is not worth the cost of the
  #      additional calls to `sameType`?
  hash(x.PType.len)

func `==`(x, y: FuncTypeLutKey): bool {.inline.} =
  let xt = x.PType
  let yt = y.PType
  if xt.len == yt.len:
    # we want to ignore calling convention differences for the proc type in
    # question but not for procedural parameter types. Since `types.sameType`
    # doesn't support this, we do the iteration ourself

    for i in 0..<xt.len:
      {.noSideEffect.}:
        # XXX: `compareTypes` reads from a global variable (`eqTypeFlags`)
        # that's only written to at compiler start-up, so it's relatively
        # safe to cast away the side-effects

        # TODO: using `dcEq` here leads to a function pointer with type
        #       `proc(x: distinct int)` being incompatible with one of
        #       type `proc(x: int)`. Specification for compatible function
        #       pointers is missing. Using `dcEqIgnoreDistinct` would break
        #       functions with `typeDesc` parameters however.
        if not compareTypes(xt[i], yt[i], dcEq, {IgnoreTupleFields}):
          return false

    result = true

func getOrCreateFuncType*(c: var TypeInfoCache, typ: PType): FunctionTypeId =
  assert typ.kind == tyProc

  let typ = typ.skipTypes(abstractRange)

  let key = FuncTypeLutKey(typ)
  # XXX: redundant table lookup
  if key in c.funcTypeLut:
    return c.funcTypeLut[key]
  else:
    result = c.nextFuncTypeId
    inc int(c.nextFuncTypeId)

    c.funcTypeLut[key] = result