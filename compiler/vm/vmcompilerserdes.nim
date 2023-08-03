## This module implements the translation of `PNode`-tree values into
## VM data and vice-versa.
##
## ser = serialize; des = deserialize
##
## `serialize` means `PNode` -> VM-data
## `deserialize` means VM-data -> `PNode`

import
  compiler/ast/[
    ast_types,
    ast,
    errorhandling,
    lineinfos,
    nimsets,
    types
  ],
  compiler/front/[
    msgs
  ],
  compiler/utils/[
    idioms
  ],
  compiler/vm/[
    vmaux,
    vmdef,
    vmmemory,
    vmobjects,
    vmtypegen,
    vmtypes,
  ],
  std/[
    options
  ],
  experimental/[
    results
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import SemReport,
  reportAst
from compiler/ast/report_enums import ReportKind

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
  ## In case that `con` is not none, deserialize the object as `con` and not
  ## as the type it was originally created with
  assert vt.kind == akRef
  assert con.kind == tyRef

  let r = c.heap.tryDeref(slot, vt.targetType)

  result =
    if r.isOk:
      let src = r.unsafeGet()

      let base = con.elemType()
      let conBase = base.skipTypes(abstractInst)

      if conBase.kind == tyObject:
        # Don't deserialize the target as the type it was created with, but
        # rather with the type of the handle

        # pass the unskipped base type
        c.deserializeObject(src.byteView(), vt.targetType, f, conBase, info)
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
    setResult(nkUIntLit, intVal, i)
  of tyBool, tyEnum, tyInt..tyInt64:
    let i =
      case vt.kind
      of akInt: signExtended(readIntBits(m), BiggestInt(s))
      of akDiscriminator: readDiscriminant(m, vt.numBits)
      else: unreachable()
    setResult(nkIntLit, intVal, i)
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
      result = deserializeRef(c, atom.refVal, vt, formal, t, info)
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
    case vt.kind
    of akCallable:
      if not atom.callableVal.isNil:
        let entry = c.functions[int toFuncIndex(atom.callableVal)]
        # XXX: the effects list of the prc.typ and `formal` can be different.
        #      What problems does this entail?
        result = newSymNode(entry.sym)
      else:
        result = newNode(nkNilLit)

    of akClosure:
      if atom.closureVal.fnc.isNil:
        result = newNode(nkNilLit)
      else:
        let entry = c.functions[int toFuncIndex(atom.closureVal.fnc)]
        result = newNode(nkClosure)
        result.add(newSymNode(entry.sym))

        let env =
          if atom.closureVal.env.isNil:
            newNode(nkNilLit)
          else:
            let t = entry.sym.getEnvParam().typ
            c.deserializeRef(atom.closureVal.env, entry.envParamType, t, t, info)

        result.sons.add(env)
    else:
      unreachable()

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

proc serialize*(c: var TCtx, n: PNode, dest: LocHandle, t: PType = nil)

proc marshalFields*(c: var TCtx, nodes: openArray[PNode], dest: LocHandle) =
  for (i, n) in nodes.pairs:
    let (p, sub) =
      if n.kind == nkExprColonExpr:
        assert i == n[0].sym.position
        (n[0].sym.position, n[1])
      else:
        (i, n)

    c.serialize(sub, dest.getFieldHandle(FieldPosition(p)))

template fidx(x: SomeInteger): untyped =
  rangeCheck(x >= 0)
  FieldIndex(x)

proc serializeObject*(c: var TCtx, dest: LocHandle, constr: PNode, ty: PType): int =
  ## Serializes one `object`-type layer to `dest`, processing base types via
  ## recursion. `constr` is the `nkObjConst` expression tree where each
  ## successive symbol has a higher `position` value than the previous one
  assert ty.kind == tyObject
  let vt = dest.typ
  result =
    if vt.relFieldStart > 0:
    let pt = ty[0].skipTypes(skipPtrs)
    serializeObject(c, dest.getFieldHandle(fidx(0)), constr, pt)
  else:
    1 # skip the child at index 0 (constructor type expr)

  if vt.branches.len == 0:
    let start = fidx(if vt.relFieldStart > 0: 1 else: 0)
    for i in start..<fidx(vt.objFields.len):
      let n = constr[result]
      assert n[0].kind == nkSym
      assert n[0].sym.position == toFieldPos(vt, i).int
      serialize(c, n[1], dest.getFieldHandle(i))
      inc result
  else:
    # `deserializeObjectPart` made sure to only include active fields, so no
    # extra logic is required here
    let posHigh = totalFieldCount(vt) - 1
    # Iterate the items in `constr` until either the end is reached or a field
    # that's not part of this object is found
    while result < constr.len and (let n = constr[result]; n[0].sym.position <= posHigh):
      let
        s = n[0].sym
        rhs = n[1]
        p = s.position.fpos
        idx = toFieldIndex(vt, p)
        fLoc = dest.getFieldHandle(idx)

      if sfDiscriminant notin s.flags:
        c.serialize(rhs, fLoc)
      else:
        let recCase = findRecCase(ty, s)
        assert recCase != nil
        let b = findMatchingBranch(recCase, rhs)
        assert b != -1

        fLoc.writeDiscrField(dest.typ, idx, rhs.intVal.int, b)

      inc result

proc serialize*(c: var TCtx, n: PNode, dest: LocHandle, t: PType = nil) =
  ## Translates the given `PNode`-tree based data (`n`) into VM data, storing
  ## the result in the location referenced by `dest`. Required extra memory
  ## (`string`, `seq`, closures, etc.) is automatically created and populated
  let origTyp = if t != nil: t else: n.typ
  let t = origTyp.skipTypes(abstractRange+{tyStatic}-{tyTypeDesc})

  case t.kind
  of tyBool, tyChar, tyEnum, tyInt..tyInt64, tyUInt..tyUInt64:
    assert n.kind in nkCharLit..nkUInt64Lit
    dest.writeUInt(n.intVal)
  of tyCstring, tyString:
    assert n.kind in nkStrLit..nkTripleStrLit
    assert dest.typ.kind == akString
    deref(dest).strVal.newVmString(n.strVal, c.allocator)
  of tyFloat32:
    assert n.kind == nkFloat32Lit
    dest.writeFloat32(float32(n.floatVal))
  of tyFloat64:
    assert n.kind == nkFloat64Lit
    dest.writeFloat64(float64(n.floatVal))
  of tyFloat:
    assert n.kind in {nkFloatLit, nkFloat64Lit}
    dest.writeFloat(n.floatVal)
  of tyPointer, tyPtr, tyNil:
    assert n.kind == nkNilLit
    # Only allow nil values for pointers
    deref(dest).ptrVal = nil
  of tyUntyped, tyTyped, tyTypeDesc:
    assert dest.typ.kind == akPNode
    deref(dest).nodeVal = n
  of tyRef:
    if t.sym == nil or t.sym.magic != mPNimrodNode:
      assert dest.typ.kind == akRef
      case n.kind:
      of nkNilLit: discard "nothing to do"
      of nkObjConstr:
        let typ = c.getOrCreate(t)
        let r = c.heap.heapNew(c.allocator, typ.targetType)
        c.serialize(n, c.heap.unsafeDeref(r), t[0])
      else: unreachable()
    else:
      # a ``NimNode``
      assert dest.typ.kind == akPNode
      assert n.kind == nkNimNodeLit
      deref(dest).nodeVal = n[0]
  of tyProc:
    case t.callConv
    of ccClosure:
      assert dest.typ.kind == akClosure
      case n.kind
      of nkNilLit: discard "nothing to do"
      of nkClosure:
        assert n[0].kind == nkSym
        let fnc = toFuncPtr(c.lookupProc(n[0].sym))

        let env =
          if n[1].kind == nkNilLit:
            # TODO: use a constant instead
            HeapSlotHandle(0)
          else:
            let nEnvTyp = n[1].typ # note: this is the `ref env` type
            # The closure's env type was already created, so we can just look
            # it up here
            let envTyp = c.typeInfoCache.lookup(c.config, nEnvTyp).unsafeGet
            let e = c.heap.heapNew(c.allocator, envTyp.targetType)
            c.serialize(n[1], c.heap.unsafeDeref(e), nEnvTyp[0])
              # we wan't to fill the object, so pass the object type (not the
              # ref-type, i.e. `nEnvTyp`)
            e

        deref(dest).closureVal = VmClosure(fnc: fnc, env: env)
      else: unreachable(n.kind)
    else:
      assert n.kind == nkSym
      assert dest.typ.kind == akCallable
      deref(dest).callableVal = toFuncPtr(c.lookupProc(n.sym))
  of tyObject:
    assert n.kind == nkObjConstr
    assert dest.typ.kind == akObject

    let L = serializeObject(c, dest, n, t)
    assert L == n.sons.len

  of tyArray, tySequence, tyOpenArray:
    assert n.kind == nkBracket
    let slice =
      if t.kind == tyArray:
        assert dest.typ.kind == akArray
        toSlice(dest)
      else:
        # `openArray` is currently the same as `seq` inside the VM
        assert dest.typ.kind == akSeq
        newVmSeq(deref(dest).seqVal, dest.typ, n.len, c.memory)
        loadFullSlice(c.allocator, deref(dest).seqVal.data, dest.typ.seqElemType)

    for (i, x) in n.pairs:
      c.serialize(x, slice[i])

  of tyTuple:
    assert n.kind == nkTupleConstr
    assert dest.typ.kind == akObject
    marshalFields(c, n.sons, dest)

  of tySet:
    assert n.kind == nkCurly
    if t[0].kind != tyEmpty: # Prevent `set[Empty]` from reaching `lengthOrd`
      inclTreeSet(mbitSet(dest), c.config, n)

  of tyVar, tyLent:
    # XXX: var and lent should either be rejected during sem check or supported
    c.config.globalReport(n.info, SemReport(
      kind: rsemTypeNotAllowed,
      allowedType: (
        allowed: origTyp,
        actual: origTyp,
        kind: skConst,
        allowedFlags: {})))

  of tyError: unreachable("error nodes must not reach here")
  else: unreachable(t.kind)
