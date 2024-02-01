## Implements the deserialization of VM data into `PNode`-trees.

import
  compiler/ast/[
    ast_types,
    ast,
    errorhandling,
    lineinfos,
    nimsets,
    types
  ],
  compiler/utils/[
    idioms
  ],
  compiler/vm/[
    vmaux,
    vmdef,
    vmmemory,
    vmobjects,
    vmtypes,
  ],
  experimental/[
    results
  ]

# XXX: the function signatures are a bit cumbersome here

from compiler/ast/trees import exprStructuralEquivalent, cyclicTree

const SkipSet = abstractRange + {tyStatic} - {tyTypeDesc}

# Functions for VM to PNode conversion

# XXX: the deserialize functions don't need the whole TCtx, just a few things
#      out of it (ConfigRef, heap, functions, etc.). Passing each of them as
#      separate parameters would make the function signature unreasonably
#      large however.

template wrongNode(t: PType): PNode =
  mixin info
  newNodeIT(nkEmpty, info, t)

proc deserialize(c: TCtx, m: VmMemoryRegion, vt: PVmType, formal, t: PType, info: TLineInfo): PNode

proc deserializeObject(c: TCtx, m: VmMemoryRegion, vt: PVmType, f, con: PType, info: TLineInfo): PNode

proc deserializeRef*(c: TCtx, slot: HeapSlotHandle, vt: PVmType; f, con: PType, info: TLineInfo): PNode =
  ## Produces the construction AST for the `ref` value represented by `slot`.
  ## If the heap slot is inaccessible or its type not compatible with `vt`,
  ## an error is returned. For the 'nil' slot, a nil literal is returned.
  ##
  ## When `vt` is not ``noneType``, the value is deserialized as if it were of
  ## type `vt`. Otherwise the full value is deserialized.
  assert con.kind == tyRef

  let r = c.heap.tryDeref(slot, vt)

  result =
    if r.isOk:
      let src = r.unsafeGet()

      let base = con.elemType()
      let conBase = base.skipTypes(abstractInst)

      if conBase.kind == tyObject:
        let t =
          if vt == noneType: src.typ
          else:              vt

        # pass the unskipped base type
        c.deserializeObject(src.byteView(), t, f, conBase, info)
      else:
        c.config.newError(
          wrongNode(base),
          PAstDiag(kind: adVmUnsupportedNonNil, unsupported: con))
    else:
      let e = r.takeErr()
      if e == dfcNil:
        newNodeIT(nkNilLit, info, f)
      else:
        c.config.newError(wrongNode(f), PAstDiag(
          kind: case e
                of dfcNil:               adVmDerefNilAccess
                of dfcInvalid, dfcFreed: adVmDerefAccessOutOfBounds
                of dfcTypeMismatch:      adVmDerefAccessTypeMismatch))

proc deserialize(c: TCtx, m: VmMemoryRegion, vt: PVmType, formal: PType, info: TLineInfo): PNode {.inline.} =
  deserialize(c, m, vt, formal, formal.skipTypes(SkipSet), info)


# TODO: record walking is implemented multiple times in multiple different
#       places across the compiler. An `iterator` encapsulating the logic would
#       probably make sense...
func findField(n: PNode, pos: int): PSym =
  case n.kind
  of nkSym:
    if n.sym.position == pos:
      return n.sym
  of nkRecList:
    for x in n.items:
      result = findField(x, pos)
      if result != nil:
        return
  of nkRecCase:
    if n[0].sym.position == pos:
      return n[0].sym

    for i in 1..<n.len:
      result = findField(lastSon(n[i]), pos)
      if result != nil:
        return
  else:
    unreachable()

proc deserializeTuple(c: TCtx, m: VmMemoryRegion, vt: PVmType; formal, ty: PType, info: TLineInfo): PNode =
  assert vt.kind == akObject

  result = newNodeIT(nkTupleConstr, info, formal)
  result.sons.newSeq(vt.objFields.len)

  var hasError = false

  template unmarshalField(o, t, nt): untyped =
    let n = c.deserialize(m.subView(o, t.sizeInBytes), t, nt, info)
    hasError = hasError or n.isError
    n

  if ty.n != nil:
    # named tuple
    for i, (o, t) in vt.objFields.pairs:
      let sym = ty.n[i].sym
      assert sym.position == i
      result[i] = newTree(nkExprColonExpr, newSymNode(sym), unmarshalField(o, t, sym.typ))
  else:
    # unnamed tuple
    for i, (o, t) in vt.objFields.pairs:
      result[i] = unmarshalField(o, t, ty[i])

  if hasError:
    result = c.config.wrapError(result)


proc deserializeObjectPart(c: TCtx,
  m: VmMemoryRegion,
  vt: PVmType,
  ty: PType, info: TLineInfo,
  dest: var TNode): tuple[cIdx: int, hasError: bool] =
  var start = 0
  if vt.relFieldStart == 0:
    discard "nothing to do"
    result.cIdx = 1 # the child at index 0 is the constructor symbol node
  else:
    let p = vt.objFields[0].typ
    result = deserializeObjectPart(c, m.subView(0, p.sizeInBytes), p, ty[0].skipTypes(skipPtrs), info, dest)
    start = 1

  template constrField(f, sym): untyped =
    let n = c.deserialize(m.subView(f.offset, f.typ.sizeInBytes), f.typ, sym.typ, info)
    result.hasError = result.hasError or n.isError
    nkExprColonExpr.newTreeI(info, newSymNode(sym), n)

  if vt.branches.len == 0:
    # no variant object
    for i in start..<vt.objFields.len:
      let f = vt.objFields[i]
      let sym = findField(ty.n, vt.toFieldPos(FieldIndex i).int)
      dest.sons[result.cIdx] = constrField(f, sym)
      inc result.cIdx

  else:
    # variant object
    var iter: VariantFieldIterCtx
    iter.setup(vt, 0)

    if vt.relFieldStart > 0:
      # skip base object field
      iter.next(m, vt)

    # TODO: should recursively walk the type's record instead
    while true:
      let r = iter.get()
      if r.valid:
        let f = vt.fieldAt(r.idx)
        let sym = findField(ty.n, vt.toFieldPos(r.idx).int)
        assert sym.typ != nil
        dest.sons[result.cIdx] = constrField(f, sym)
        inc result.cIdx
      else:
        break

      iter.next(m, vt)


proc deserializeObject(c: TCtx, m: VmMemoryRegion, vt: PVmType; f, con: PType; info: TLineInfo): PNode =
  assert con.kind == tyObject
  result = newNodeI(nkObjConstr, info)
  result.typ = f
  result.sons.newSeq(1 + vt.totalFieldCount()) # This over-allocates in
                                                # the case of variant objects

  # XXX: If wanted, the constructor expr could be filled in here. Just
  #      `newSymNode(ty.sym)` won't work however, as `ref` types and
  #      generic instances need special handling. Not using `nkEmpty`
  #      also changes what `opcRepr` prints with `mm:refc`
  result.sons[0] = newNode(nkEmpty)
  result.sons[0].typ = f

  let (len, hasError) = deserializeObjectPart(c, m, vt, con, info, result[])
  result.sons.setLen(len) # XXX: this should ideally also shrink the capacity

  if hasError:
    result = c.config.wrapError(result)

proc deserializeArray*(
  c: TCtx,
  m: VmMemoryRegion,
  count: int, stride: int, eTyp: PVmType,
  f: PType, info: TLineInfo): PNode =
  ## Doesn't set the resulting node's type. `f` is the formal array
  ## element type
  result = newNodeI(nkBracket, info)
  result.sons.newSeq(count)

  var hasError = false

  # XXX: `deserialize` dispatches to a concrete deserialize proc based on
  #      `f.kind`. In our case, the kind stays the same accross the loop,
  #      so we're doing lots of unnecessary dispatching
  var off = 0
  for i in 0..<count:
    result.sons[i] =
      c.deserialize(m.subView(off, eTyp.sizeInBytes), eTyp, f, info)
    hasError = hasError or result.sons[i].isError
    off += stride

  if hasError:
    result = c.config.wrapError(result)


proc deserialize(c: TCtx, m: VmMemoryRegion, vt: PVmType, formal, t: PType, info: TLineInfo): PNode =
  let
    s = vt.sizeInBytes
    atom = cast[ptr Atom](unsafeAddr(m[0]))

  template setResult(k: TNodeKind, f, v) =
    result = newNodeIT(k, info, formal)
    result.f = v

  template wrongNode(): PNode =
    ## For `newError`. There doesn't exist a "wrong node" here, so we're
    ## simply using an empty node instead
    newNodeIT(nkEmpty, info, formal)

  case t.kind
  of tyChar, tyUInt..tyUInt64:
    let i =
      case vt.kind
      of akInt: readUInt(m)
      of akDiscriminator: readDiscriminant(m, vt.numBits)
      else: unreachable()
    result = newIntTypeNode(i, formal)
    result.info = info
  of tyBool, tyInt..tyInt64:
    let i =
      case vt.kind
      of akInt: signExtended(readIntBits(m), BiggestInt(s))
      of akDiscriminator: readDiscriminant(m, vt.numBits)
      else: unreachable()
    result = newIntTypeNode(i, formal)
    result.info = info
  of tyEnum:
    # the value is stored as the enum's underlying type
    result = deserialize(c, m, vt, formal, t.lastSon, info)
  of tyFloat32:
    assert vt.kind == akFloat
    setResult(nkFloat32Lit, floatVal, readFloat32(m))
  of tyFloat64, tyFloat:
    assert vt.kind == akFloat
    setResult(nkFloat64Lit, floatVal, readFloat64(m))
  of tyCstring, tyString:
    assert vt.kind == akString
    setResult(nkStrLit, strVal, $atom.strVal)
  of tyPtr, tyPointer, tyNil:
    assert vt.kind == akPtr
    if atom.ptrVal == nil:
      result = newNodeIT(nkNilLit, info, formal)
    else:
      result = c.config.newError(
        wrongNode(),
        PAstDiag(kind: adVmUnsupportedNonNil, unsupported: t))
  of tyRef:
    if t.sym == nil or t.sym.magic != mPNimrodNode:
      # FIXME: cyclic references will lead to infinite recursion
      result = deserializeRef(c, atom.refVal, vt.targetType, formal, t, info)
    else:
      assert vt.kind == akPNode

      if unlikely(cyclicTree(atom.nodeVal)):
        result = c.config.newError(
          wrongNode(),
          PAstDiag(kind: adCyclicTree, cyclic: atom.nodeVal))
      else:
        # XXX: not doing a full tree-copy here might lead to issues
        result = newTreeIT(nkNimNodeLit, info, formal): atom.nodeVal

  of tyProc:
    case t.callConv
    of ccClosure:
      # closures are stored as a ``(prc, env)`` tuple
      assert vt.kind == akObject
      let
        fieldA = cast[ptr Atom](addr m[vt.objFields[0].offset])
        fieldB = cast[ptr Atom](addr m[vt.objFields[1].offset])

      if fieldA.callableVal.isNil:
        result = newNode(nkNilLit)
      else:
        let prc = c.functions[int toFuncIndex(fieldA.callableVal)].sym
        result = newTree(nkClosure, [newSymNode(prc), nil])
        result[1] =
          if fieldB.refVal.isNil:
            newNode(nkNilLit)
          else:
            let t = getEnvParam(prc).typ
            c.deserializeRef(fieldB.refVal, noneType, t, t, info)
    else:
      if not atom.callableVal.isNil:
        let entry = c.functions[int toFuncIndex(atom.callableVal)]
        # XXX: the effects list of the prc.typ and `formal` can be different.
        #      What problems does this entail?
        result = newSymNode(entry.sym)
      else:
        result = newNode(nkNilLit)

    result.typ = formal
    result.info = info

    if result.safeLen == 2 and result[1].isError:
      # The env can be an nkError
      result = c.config.wrapError(result)

  of tyObject:
    result = deserializeObject(c, m, vt, formal, t, info)
  of tyTuple:
    result = deserializeTuple(c, m, vt, formal, t, info)
  of tySequence, tyOpenArray, tyArray:
    case vt.kind
    of akArray:
      assert t.kind in {tyArray, tyOpenArray}
      result = deserializeArray(c, m,
        vt.elementCount,
        vt.elementStride,
        vt.elementType,
        t.elemType(),
        info)
    of akString, akSeq:
      assert t.kind in {tySequence, tyOpenArray}
      result = deserializeArray(c,
        byteView(toSlice(atom.seqVal, vt.seqElemType, c.allocator)),
        atom.seqVal.length,
        vt.seqElemStride,
        vt.seqElemType,
        t.elemType(),
        info)
    else:
      unreachable($vt.kind)

    result.typ = formal
  of tySet:
    assert vt.kind == akSet
    result = toTreeSet(nil, m, formal, info)
  of tyVar, tyLent, tyUntyped, tyTyped, tyTypeDesc:
    # HACK: these should be rejected during sem-check instead
    # XXX: skConst is not necessarily true here. deserialization also happens
    #      for `static(expr)`

    let n = wrongNode()
    # values of this type can't cross the VM/compiler border
    result = c.config.newError(n, PAstDiag(
      kind: adSemTypeNotAllowed,
      allowedType: (
        allowed: formal,
        actual: formal,
        kind: skConst,
        allowedFlags: {})))
  else:
    unreachable()

  assert result.typ != nil, $t.kind

# XXX: can't be a func because of internal error reporting
proc deserialize*(c: TCtx, handle: LocHandle, asType: PType, info: TLineInfo): PNode =
  deserialize(c, handle.byteView(), handle.typ, asType, info)
