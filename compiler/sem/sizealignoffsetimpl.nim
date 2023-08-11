#
#
#           The Nim Compiler
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#
## code owner: Arne Döring
## e-mail: arne.doering@gmx.net
## included from types.nim

proc align(address, alignment: BiggestInt): BiggestInt =
  result = (address + (alignment - 1)) and not (alignment - 1)

proc align(address, alignment: int): int =
  result = (address + (alignment - 1)) and not (alignment - 1)

const
  ## a size is considered "unknown" when it is an imported type from C
  szUnknownSize* = -3
  szIllegalRecursion* = -2
  szUncomputedSize* = -1
  szTooBigSize* = -4

type IllegalTypeRecursionError = object of ValueError

proc raiseIllegalTypeRecursion() =
  raise newException(IllegalTypeRecursionError, "illegal type recursion")

type
  OffsetAccum = object
    maxAlign: int
    offset: int

proc inc(arg: var OffsetAccum; value: int) =
  if unlikely(value == szIllegalRecursion): raiseIllegalTypeRecursion()
  if value == szUnknownSize or arg.offset == szUnknownSize:
    arg.offset = szUnknownSize
  else:
    arg.offset += value

proc alignmentMax(a,b: int): int =
  if unlikely(a == szIllegalRecursion or b == szIllegalRecursion): raiseIllegalTypeRecursion()
  if a == szUnknownSize or b == szUnknownSize:
    szUnknownSize
  else:
    max(a,b)

proc align(arg: var OffsetAccum; value: int) =
  if unlikely(value == szIllegalRecursion):  raiseIllegalTypeRecursion()
  if value == szUnknownSize or arg.maxAlign == szUnknownSize or arg.offset == szUnknownSize:
    arg.maxAlign = szUnknownSize
    arg.offset = szUnknownSize
  else:
    arg.maxAlign = max(value, arg.maxAlign)
    arg.offset = align(arg.offset, value)

proc mergeBranch(arg: var OffsetAccum; value: OffsetAccum) =
  if value.maxAlign == szUnknownSize or arg.maxAlign == szUnknownSize or
     value.offset == szUnknownSize or arg.offset == szUnknownSize:
    arg.maxAlign = szUnknownSize
    arg.offset = szUnknownSize
  else:
    arg.offset = max(arg.offset, value.offset)
    arg.maxAlign = max(arg.maxAlign, value.maxAlign)

proc finish(arg: var OffsetAccum): int =
  if arg.maxAlign == szUnknownSize or arg.offset == szUnknownSize:
    result = szUnknownSize
    arg.offset = szUnknownSize
  else:
    result = align(arg.offset, arg.maxAlign) - arg.offset
    arg.offset += result

proc computeSizeAlign(conf: ConfigRef; typ: PType)

proc computeSubObjectAlign(conf: ConfigRef; n: PNode): BiggestInt =
  ## returns object alignment
  case n.kind
  of nkRecCase:
    assert(n[0].kind == nkSym)
    result = computeSubObjectAlign(conf, n[0])
    for i in 1..<n.len:
      let child = n[i]
      case child.kind
      of nkOfBranch, nkElse:
        let align = computeSubObjectAlign(conf, child.lastSon)
        if align < 0:
          return align
        result = max(result, align)
      else:
        unreachable("computeSubObjectAlign")
  of nkRecList:
    result = 1
    for i, child in n.sons:
      let align = computeSubObjectAlign(conf, n[i])
      if align < 0:
        return align
      result = max(result, align)
  of nkSym:
    computeSizeAlign(conf, n.sym.typ)
    result = n.sym.typ.align
  else:
    result = 1


proc setOffsetsToUnknown(n: PNode) =
  if n.kind == nkSym and n.sym.kind == skField:
    n.sym.offset = szUnknownSize
  else:
    for i in 0..<n.safeLen:
      setOffsetsToUnknown(n[i])

proc computeObjectOffsetsFoldFunction(conf: ConfigRef; n: PNode, packed: bool, accum: var OffsetAccum) =
  ## ``offset`` is the offset within the object, after the node has been written, no padding bytes added
  ## ``align`` maximum alignment from all sub nodes
  assert n != nil
  if n.typ != nil and n.typ.size == szIllegalRecursion:
    raiseIllegalTypeRecursion()
  case n.kind
  of nkRecCase:
    assert(n[0].kind == nkSym)
    computeObjectOffsetsFoldFunction(conf, n[0], packed, accum)
    var maxChildAlign: int = if accum.offset == szUnknownSize: szUnknownSize else: 1
    if not packed:
      for i in 1..<n.len:
        let child = n[i]
        case child.kind
        of nkOfBranch, nkElse:
          # offset parameter cannot be known yet, it needs to know the alignment first
          let align = int(computeSubObjectAlign(conf, n[i].lastSon))
          maxChildAlign = alignmentMax(maxChildAlign, align)
        else:
          unreachable("computeObjectOffsetsFoldFunction(record case branch)")

    if maxChildAlign == szUnknownSize:
      setOffsetsToUnknown(n)
      accum.offset  = szUnknownSize
      accum.maxAlign = szUnknownSize
    else:
      # the union needs to be aligned first, before the offsets can be assigned
      accum.align(maxChildAlign)
      let accumRoot = accum # copy, because each branch should start af the same offset
      for i in 1..<n.len:
        var branchAccum = OffsetAccum(offset: accumRoot.offset, maxAlign: 1)
        computeObjectOffsetsFoldFunction(conf, n[i].lastSon, packed, branchAccum)
        discard finish(branchAccum)
        accum.mergeBranch(branchAccum)
  of nkRecList:
    for i, child in n.sons:
      computeObjectOffsetsFoldFunction(conf, child, packed, accum)
  of nkSym:
    var size = szUnknownSize
    var align = szUnknownSize
    if n.sym.bitsize == 0: # 0 represents bitsize not set
      computeSizeAlign(conf, n.sym.typ)
      size = n.sym.typ.size.int
      align = if packed: 1 else: n.sym.typ.align.int
    accum.align(align)
    if n.sym.alignment > 0:
      accum.align(n.sym.alignment)
    n.sym.offset = accum.offset
    accum.inc(size)
  else:
    accum.maxAlign = szUnknownSize
    accum.offset = szUnknownSize

proc computeUnionObjectOffsetsFoldFunction(conf: ConfigRef; n: PNode; packed: bool; accum: var OffsetAccum) =
  ## ``accum.offset`` will be the offset from the largest member of the union.
  case n.kind
  of nkRecCase:
    accum.offset = szUnknownSize
    accum.maxAlign = szUnknownSize
    conf.localReport(n, reportSem(rsemCaseInUnion))
  of nkRecList:
    let accumRoot = accum # copy, because each branch should start af the same offset
    for child in n.sons:
      var branchAccum = OffsetAccum(offset: accumRoot.offset, maxAlign: 1)
      computeUnionObjectOffsetsFoldFunction(conf, child, packed, branchAccum)
      discard finish(branchAccum)
      accum.mergeBranch(branchAccum)
  of nkSym:
    var size = szUnknownSize
    var align = szUnknownSize
    if n.sym.bitsize == 0: # 0 represents bitsize not set
      computeSizeAlign(conf, n.sym.typ)
      size = n.sym.typ.size.int
      align = if packed: 1 else: n.sym.typ.align.int
    accum.align(align)
    if n.sym.alignment > 0:
      accum.align(n.sym.alignment)
    n.sym.offset = accum.offset
    accum.inc(size)
  else:
    accum.maxAlign = szUnknownSize
    accum.offset = szUnknownSize

proc computeSizeAlign(conf: ConfigRef; typ: PType) =
  ## computes and sets ``size`` and ``align`` members of ``typ``
  assert typ != nil
  let hasSize = typ.size != szUncomputedSize
  let hasAlign = typ.align != szUncomputedSize

  if hasSize and hasAlign:
    # nothing to do, size and align already computed
    return

  # This function can only calculate both, size and align at the same time.
  # If one of them is already set this value is stored here and reapplied
  let revertSize = typ.size
  let revertAlign = typ.align
  defer:
    if hasSize:
      typ.size = revertSize
    if hasAlign:
      typ.align = revertAlign

  if typ.size == szIllegalRecursion or typ.align == szIllegalRecursion:
    # we are already computing the size of the type
    # --> illegal recursion in type
    return

  # mark computation in progress
  typ.size = szIllegalRecursion
  typ.align = szIllegalRecursion
  typ.paddingAtEnd = 0

  var tk = typ.kind
  case tk
  of tyProc:
    if typ.callConv == ccClosure:
      typ.size = 2 * conf.target.ptrSize
    else:
      typ.size = conf.target.ptrSize
    typ.align = int16(conf.target.ptrSize)
  of tyNil:
    typ.size = conf.target.ptrSize
    typ.align = int16(conf.target.ptrSize)
  of tyString:
    if optSeqDestructors in conf.globalOptions:
      typ.size = conf.target.ptrSize * 2
    else:
      typ.size = conf.target.ptrSize
    typ.align = int16(conf.target.ptrSize)
  of tyCstring, tySequence, tyPtr, tyRef, tyVar, tyLent:
    let base = typ.lastSon
    if base == typ:
      # this is not the correct location to detect ``type A = ptr A``
      typ.size = szIllegalRecursion
      typ.align = szIllegalRecursion
      typ.paddingAtEnd = szIllegalRecursion
      return
    typ.align = int16(conf.target.ptrSize)
    if typ.kind == tySequence and optSeqDestructors in conf.globalOptions:
      typ.size = conf.target.ptrSize * 2
    else:
      typ.size = conf.target.ptrSize

  of tyArray:
    computeSizeAlign(conf, typ[1])
    let elemSize = typ[1].size
    let len = lengthOrd(conf, typ[0])
    if elemSize < 0:
      typ.size = elemSize
      typ.align = int16(elemSize)
    elif len < 0:
      typ.size = szUnknownSize
      typ.align = szUnknownSize
    else:
      typ.size = toInt64Checked(len * int32(elemSize), szTooBigSize)
      typ.align = typ[1].align

  of tyUncheckedArray:
    let base = typ.lastSon
    computeSizeAlign(conf, base)
    typ.size = 0
    typ.align = base.align

  of tyEnum:
    let base = typ.lastSon
    computeSizeAlign(conf, base)
    # use the size and alignment of the underlying type:
    typ.size = base.size
    typ.align = base.align
  of tySet:
    if typ[0].kind == tyGenericParam:
      typ.size = szUncomputedSize
      typ.align = szUncomputedSize
    else:
      let length = toInt64(lengthOrd(conf, typ[0]))
      if length <= 8:
        typ.size = 1
        typ.align = 1
      elif length <= 16:
        typ.size = 2
        typ.align = 2
      elif length <= 32:
        typ.size = 4
        typ.align = 4
      elif length <= 64:
        typ.size = 8
        typ.align = int16(conf.floatInt64Align)
      elif align(length, 8) mod 8 == 0:
        typ.size = align(length, 8) div 8
        typ.align = int16(conf.floatInt64Align)
      else:
        typ.size = align(length, 8) div 8 + 1
        typ.align = int16(conf.floatInt64Align)
  of tyRange:
    computeSizeAlign(conf, typ[0])
    typ.size = typ[0].size
    typ.align = typ[0].align
    typ.paddingAtEnd = typ[0].paddingAtEnd

  of tyTuple:
    try:
      var accum = OffsetAccum(maxAlign: 1)
      for i in 0..<typ.len:
        let child = typ[i]
        computeSizeAlign(conf, child)
        accum.align(child.align)
        if typ.n != nil: # is named tuple (has field symbols)?
          let sym = typ.n[i].sym
          sym.offset = accum.offset
        accum.inc(int(child.size))
      typ.paddingAtEnd = int16(accum.finish())
      typ.size = if accum.offset == 0: 1 else: accum.offset
      typ.align = int16(accum.maxAlign)
    except IllegalTypeRecursionError:
      typ.paddingAtEnd = szIllegalRecursion
      typ.size = szIllegalRecursion
      typ.align = szIllegalRecursion

  of tyObject:
    try:
      var accum =
        if typ[0] != nil:
          # compute header size
          var st = typ[0]
          while st.kind in skipPtrs:
            st = st[^1]
          computeSizeAlign(conf, st)
          OffsetAccum(
            offset: int(st.size),
            maxAlign: st.align
          )
        elif isObjectWithTypeFieldPredicate(typ):
          # this branch is taken for RootObj
          OffsetAccum(
            offset: conf.target.intSize,
            maxAlign: conf.target.intSize
          )
        else:
          OffsetAccum(maxAlign: 1)
      if tfUnion in typ.flags:
        if accum.offset != 0:
          let info = if typ.sym != nil: typ.sym.info else: unknownLineInfo
          conf.localReport(info, reportSem(rsemOffsetInUnion))
          accum = OffsetAccum(offset: szUnknownSize, maxAlign: szUnknownSize)
        elif tfPacked in typ.flags:
          computeUnionObjectOffsetsFoldFunction(conf, typ.n, true, accum)
        else:
          computeUnionObjectOffsetsFoldFunction(conf, typ.n, false, accum)
      elif tfPacked in typ.flags:
        accum.maxAlign = 1
        computeObjectOffsetsFoldFunction(conf, typ.n, true, accum)
      else:
        computeObjectOffsetsFoldFunction(conf, typ.n, false, accum)
      let paddingAtEnd = int16(accum.finish())
      if typ.sym != nil and
         typ.sym.flags * {sfCompilerProc, sfImportc} == {sfImportc} and
         tfCompleteStruct notin typ.flags:
        typ.size = szUnknownSize
        typ.align = szUnknownSize
        typ.paddingAtEnd = szUnknownSize
      else:
        typ.size = if accum.offset == 0: 1 else: accum.offset
        typ.align = int16(accum.maxAlign)
        typ.paddingAtEnd = paddingAtEnd
    except IllegalTypeRecursionError:
      typ.size = szIllegalRecursion
      typ.align = szIllegalRecursion
      typ.paddingAtEnd = szIllegalRecursion
  of tyInferred:
    if typ.len > 1:
      computeSizeAlign(conf, typ.lastSon)
      typ.size = typ.lastSon.size
      typ.align = typ.lastSon.align
      typ.paddingAtEnd = typ.lastSon.paddingAtEnd

  of tyGenericInst, tyDistinct, tyGenericBody, tyAlias, tySink:
    computeSizeAlign(conf, typ.lastSon)
    typ.size = typ.lastSon.size
    typ.align = typ.lastSon.align
    typ.paddingAtEnd = typ.lastSon.paddingAtEnd

  of tyTypeClasses:
    if typ.isResolvedUserTypeClass:
      computeSizeAlign(conf, typ.lastSon)
      typ.size = typ.lastSon.size
      typ.align = typ.lastSon.align
      typ.paddingAtEnd = typ.lastSon.paddingAtEnd
    else:
      typ.size = szUnknownSize
      typ.align = szUnknownSize
      typ.paddingAtEnd = szUnknownSize

  of tyTypeDesc:
    computeSizeAlign(conf, typ.base)
    typ.size = typ.base.size
    typ.align = typ.base.align
    typ.paddingAtEnd = typ.base.paddingAtEnd

  of tyForward:
    # is this really illegal recursion, or maybe just unknown?
    typ.size = szIllegalRecursion
    typ.align = szIllegalRecursion
    typ.paddingAtEnd = szIllegalRecursion

  of tyStatic:
    if typ.n != nil:
      computeSizeAlign(conf, typ.lastSon)
      typ.size = typ.lastSon.size
      typ.align = typ.lastSon.align
      typ.paddingAtEnd = typ.lastSon.paddingAtEnd
    else:
      typ.size = szUnknownSize
      typ.align = szUnknownSize
      typ.paddingAtEnd = szUnknownSize
  else:
    typ.size = szUnknownSize
    typ.align = szUnknownSize
    typ.paddingAtEnd = szUnknownSize

template foldSizeOf*(conf: ConfigRef; n: PNode; fallback: PNode): PNode =
  let config = conf
  let node = n
  let typ = node[1].typ
  computeSizeAlign(config, typ)
  let size = typ.size
  if size >= 0:
    let res = newIntNode(nkIntLit, size)
    res.info = node.info
    res.typ = node.typ
    res
  else:
    fallback

template foldAlignOf*(conf: ConfigRef; n: PNode; fallback: PNode): PNode =
  let config = conf
  let node = n
  let typ = node[1].typ
  computeSizeAlign(config, typ)
  let align = typ.align
  if align >= 0:
    let res = newIntNode(nkIntLit, align)
    res.info = node.info
    res.typ = node.typ
    res
  else:
    fallback

template foldOffsetOf*(conf: ConfigRef; n: PNode; fallback: PNode): PNode =
  ## Returns an int literal node of the given offsetof expression in `n`.
  ## Falls back to `fallback`, if the `offsetof` expression can't be processed.
  let
    config = conf
    node : PNode = n
    dotExpr =
      if node[1].kind == nkDotExpr:
        node[1]
      elif node[1].kind == nkCheckedFieldExpr:
        node[1][0]
      else:
        config.newError(n, PAstDiag(kind: adSemFoldCannotComputeOffset))

  assert dotExpr != nil
  let value = dotExpr[0]
  let member = dotExpr[1]
  computeSizeAlign(config, value.typ)
  let offset = member.sym.offset
  if offset >= 0:
    let tmp = newIntNode(nkIntLit, offset)
    tmp.info = node.info
    tmp.typ = node.typ
    tmp
  else:
    fallback
