# Implements the RTTI datum creation, both the legacy (v1) and new (v2) one.
# Included from ``mir2cg.nim``.

# TODO: move RTTI creation out of the MIR->CGIR stage and firmly into the MIR
#       stage. In detail, this means:
#       1. creating the RTTI data during to-MIR translation
#       2. emitting the type header initialization for objects in mirgen

import compiler/sem/sighashes
import compiler/mir/rtti_helper
from compiler/ast/typesrenderer import typeToString, addTypeHeader
from std/strutils import toLowerAscii

# field positions are hardcoded because looking up the fields to retrieve
# the position is too cumbersome
const
  # ``TNimTypeV2`` fields
  MemberV2Destructor = 0
  MemberV2Size = 1
  MemberV2Align = 2
  MemberV2Name = 3
  MemberV2Trace = 4
  MemberV2TypeInfo = 5
  MemberV2Flags = 6

  # ``TNimType`` fields
  MemberV1Size = 0
  MemberV1Align = 1
  MemberV1Kind = 2
  MemberV1Flags = 3
  MemberV1Base = 4
  MemberV1Node = 5
  MemberV1DeepCopy = 6
  MemberV1TypeInfo = 7
  MemberV1Name = 8

  # ``TNimNode`` fields
  MemberV1NodeKind = 0
  MemberV1Offset = 1
  MemberV1Typ = 2
  MemberV1NodeName = 3
  MemberV1Len = 4
  MemberV1Sons = 5

proc getTypeInfoV1(c; env; typ: PType, bu): Expr
proc getTypeInfoV2(c; env; typ: PType, bu): Expr

proc genTypeInfoV1Prefix(t: PType): string =
  ## Generates a non-unique name to use as a prefix in RTTI global names.
  let s = typeToString(t)
  result = newStringOfCap(s.len)
  for i in 0..<s.len:
    let c = s[i]
    case c
    of 'a'..'z':
      result.add c
    of 'A'..'Z':
      result.add toLowerAscii(c)
    of ' ':
      discard
    of ',':
      result.add '_'
    of '.':
      result.add 'O'
    of '[', '(', '{':
      result.add 'L'
    of ']', ')', '}':
      result.add 'T'
    else:
      # we mangle upper letters and digits too so that there cannot
      # be clashes with our special meanings
      result.addInt ord(c)

proc genTypeInfo2Name(t: PType): string =
  ## Computes the type path for the record type `t`. This is the name uniquely
  ## identifying a nominal type in the program (in theory).
  var res = "|"
  var it = t
  while it != nil:
    it = it.skipTypes(skipPtrs)
    if it.sym != nil:
      var m = it.sym.owner
      while m != nil and m.kind != skModule: m = m.owner
      if m == nil or sfSystemModule in m.flags:
        # produce short names for system types:
        res.add it.sym.name.s
      else:
        var p = m.owner
        if p != nil and p.kind == skPackage:
          res.add p.name.s & "."
        res.add m.name.s & "."
        res.add it.sym.name.s
    else:
      res.add $hashType(it)
    res.add "|"
    it = it[0]
  result = res

template addField(fields: var seq[NodeRef], bu: var Builder, pos: int64,
                  body: untyped) =
  fields.add bu.build(FieldInit(^pos, body))

template field(bu: var Builder, index: int64, e: NodeRef): NodeRef =
  mixin c
  bu.build FieldInit(^index, ^e)

proc hashTypeForRttiV1(typ: PType): (SigHash, PType) =
  let typ = typ.skipTypes(
    {tyAlias, tyGenericInst, tySink} + tyUserTypeClasses)
  (hashType(typ, {CoType, CoDistinct}), typ)

proc hashTypeForRttiV2(typ: PType): (SigHash, PType) =
  let typ = typ.skipTypes(
    {tyAlias, tyGenericInst, tySink, tyRange} + tyUserTypeClasses)
  (hashType(typ, {CoType, CoDistinct}), typ)

# ------------------ RTTIv2 generation -------------------------------------

proc genTypeInfoV2(c; env; typ: PType, bu): NodeRef =
  ## Generates the ``TNimTypeV2`` construction for the given type `typ`.
  let t = typ
  var fields: seq[NodeRef]
  if t.kind in {tyObject, tyDistinct}:
    fields.addField bu, MemberV2Name:
      Value(CstringType, ^genTypeInfo2Name(t))

  let destroy = c.graph.getAttachedOp(t, attachedDestructor)
  if destroy != nil and c.graph.getBody(destroy).len > 0:
    # FIXME: at run-time a proc with signature `proc(p: pointer)` is expected,
    #        but one with signature `proc(p: var T)` is stored here. Create a
    #        thunk procedure with the expected signature and pass that to
    #        the RTTI.
    #        Tracked by https://github.com/nim-works/nimskull/issues/1424
    fields.addField bu, MemberV2Destructor:
      PtrCast(PointerType,
        Addr(^env.types.add(destroy.typ),
          ^access(c, env, env.procedures.add(destroy), bu)))

  let trace = c.graph.getAttachedOp(t, attachedTrace)
  if trace != nil and c.graph.getBody(trace).len > 0:
    # FIXME: same as with destructors, the dynamic and static types are
    #        not compatible
    fields.addField bu, MemberV2Trace:
      PtrCast(PointerType,
        Addr(^env.types.add(trace.typ),
          ^access(c, env, env.procedures.add(trace), bu)))

  if not isCyclePossible(t, c.graph):
    fields.addField bu, MemberV2Flags:
      ^c.genInt(env, 1, env.types.sizeType, bu)

  fields.addField bu, MemberV2Size:
    Sizeof(^env.types.sizeType, ^env.types.add(t))

  fields.addField bu, MemberV2Align:
    Alignof(^env.types.sizeType, ^env.types.add(t))

  if optEnableDeepCopy in c.graph.config.globalOptions:
    # deepCopy being enabled implies v1 RTTI being enabled too, in which case
    # the v2 RTTI needs to link back to the v1 RTTI
    let info = getTypeInfoV1(c, env, t, bu)
    fields.addField bu, MemberV2TypeInfo, ^bu.use(info)

  bu.build RecConstr(^c.rttiV2Type, fields)

# ------------------ RTTIv1 generation -------------------------------------

proc genTypeInfo(c; env; typ, orig: PType;
                 base, sons: NodeRef, bu): NodeRef =
  ## Generates a ``TNimType`` construction for `typ`, with `base` as the
  ## base field initializer and `sons` as the sons field initializer.
  var nimtypeKind: int
  if isObjLackingTypeField(typ):
    nimtypeKind = ord(tyPureObject)
  else:
    nimtypeKind = ord(typ.kind)

  let real =
    if tfIncompleteStruct in typ.flags:
      PointerType
    else:
      env.types.add(typ)

  var fields: seq[NodeRef]
  fields.addField bu, MemberV1Size:
    Sizeof(^env.types.sizeType, real)
  fields.addField bu, MemberV1Align:
    Alignof(^env.types.sizeType, real)
  fields.addField bu, MemberV1Base, base
  fields.addField bu, MemberV1Kind, ^c.genInt(env, nimtypeKind, UInt8Type, bu)

  # compute type flags for GC optimization
  # TODO: these were meant for the long-removed legacy GCs. Investigate whether
  #       anyone still depends on the flags, and if not, remove them
  var flags = 0
  if not containsGarbageCollectedRef(typ):
    flags = flags or 1
  if not canFormAcycle(typ):
    flags = flags or 2
  if tfEnumHasHoles in typ.flags or
     (typ.kind == tyEnum and c.graph.config.firstOrd(typ) != Zero):
    # the flag is set for all enum types where the ordinal value cannot be
    # used as an index into the node array, not just for enum types with holes
    flags = flags or 4

  if flags != 0:
    fields.addField bu, MemberV1Flags:
      ^c.genInt(env, flags, UInt8Type, bu)

  if isDefined(c.graph.config, "nimTypeNames"):
    var typename =
      typeToString((if orig.typeInst.isNil: typ else: orig.typeInst),
                   preferName)
    if typename == "ref object" and orig.skipTypes(skipPtrs).sym != nil:
      # TODO: use the line info again
      typename = "anon ref object from " &
                 c.graph.config$orig.skipTypes(skipPtrs).sym.info

    fields.addField bu, MemberV1Name:
      Value(CstringType, typename)

  fields.addField bu, MemberV1Node, sons

  var op = c.graph.getAttachedOp(orig, attachedDeepCopy)
  if op.isNil and typ != orig:
    # the inner type might have an attached deepcopy
    op = c.graph.getAttachedOp(typ, attachedDeepCopy)
  if op != nil and op.ast[bodyPos].len > 0:
    fields.addField bu, MemberV1DeepCopy:
      PtrCast(PointerType,
        ^access(c, env, env.procedures.add(op), bu))

  if typ.kind == tyObject and sfImportc notin typ.sym.flags:
    let info = getTypeInfoV2(c, env, typ, bu)
    fields.addField bu, MemberV1TypeInfo:
      PtrCast(PointerType, ^bu.use(info))

  bu.build RecConstr(^c.rttiV1Type, fields)

proc genTypeInfoGeneric(c; env; typ, orig: PType, bu): NodeRef =
  ## Generates the ``TNimType`` construction for `typ`, using
  let base =
    if typ.len > 0:
      bu.use(getTypeInfoV1(c, env, typ.lastSon, bu))
    else:
      bu.build NilLit()

  genTypeInfo(c, env, typ, orig, base, bu.build(NilLit()), bu)

proc genSonsArrayAddr(c; env; arr: TypeId, d: Datum, bu): NodeRef =
  let elem = env.types.headerFor(arr, Lowered).elem
  let pt = env.types.newPtrToArray(elem)
  bu.build Addr(pt, Path(elem, Use(arr, ^datumRef(d)), 0))

proc genObjectFields(c; env; typ: PType, n: PNode): Datum =
  case n.kind
  of nkRecList:
    if n.len == 1:
      genObjectFields(c, env, typ, n[0])
    elif n.len > 0:
      let pt = env.types.newPtr(c.rttiV1NodeType)
      let arrayTyp = env.types.newArray(n.len, pt)
      let tmp = c.buildDatum Constr(arrayTyp,
        ^collect(
          for i in 0..<n.len:
            bu.build Addr(pt,
              ^datumRef(c.genObjectFields(env, typ, n[i])))))

      c.buildDatum RecConstr(^c.rttiV1NodeType,
        *field(MemberV1NodeKind, ^c.genInt(env, 2, UInt8Type, bu)),
        *field(MemberV1Len, ^c.genInt(env, n.len, env.types.sizeType, bu)),
        *field(MemberV1Sons, ^c.genSonsArrayAddr(env, arrayTyp, tmp, bu)))
    else:
      c.buildDatum RecConstr(^c.rttiV1NodeType,
        *field(MemberV1NodeKind, ^c.genInt(env, 2, UInt8Type, bu)),
        *field(MemberV1Len, ^c.genInt(env, 0, env.types.sizeType, bu)))
  of nkRecCase:
    let discr = n[0].sym
    let L = lengthOrd(c.graph.config, discr.typ)
    let pt = env.types.newPtr(c.rttiV1NodeType)
    # for record cases, the node (a list node) represents a table from
    # discriminator value to the associated branch's node. This allows for fast
    # lookup at run-time, but it can lead to enormous (in terms of space) RTTI
    # data for discriminators with a large range
    var fields: seq[NodeRef]
    var bu = initBuilder()
    for i in 1..<n.len:
      let b = n[i] # branch
      let tmp2 = genObjectFields(c, env, typ, lastSon(b))
      let elem = bu.build Addr(pt, ^datumRef(tmp2))

      case b.kind
      of nkOfBranch:
        for j in 0..<b.len - 1:
          if b[j].kind == nkRange:
            var x = toInt64(getOrdValue(b[j][0]))
            let y = toInt64(getOrdValue(b[j][1]))
            while x <= y:
              fields.addField bu, x, elem
              inc x
          else:
            fields.addField bu, toInt64(getOrdValue(b[j])), elem
      of nkElse:
        fields.addField bu, toInt64(L)-1, elem
      else:
        unreachable()

    let arrayTyp = env.types.newArray(toInt64(L), pt)
    let cons = bu.build RecConstr(arrayTyp, fields)
    let tmp = c.addDatum(bu, cons)
    let id = env.types.add(typ)
    c.buildDatum RecConstr(^c.rttiV1NodeType,
      *field(MemberV1NodeKind, ^c.genInt(env, 3, UInt8Type, bu)),
      *field(MemberV1Offset, Offsetof(^env.types.sizeType, id,
        ^(block:
          var path: seq[NodeRef]
          discard rawFieldAccess(c, env, id, discr.position.int32, path, bu)
          path))),
      *field(MemberV1Typ, ^bu.use(getTypeInfoV1(c, env, discr.typ, bu))),
      *field(MemberV1Len, ^c.genInt(env, toInt64(L), env.types.sizeType, bu)),
      *field(MemberV1NodeName, Value(CstringType, ^discr.name.s)),
      *field(MemberV1Sons, ^c.genSonsArrayAddr(env, arrayTyp, tmp, bu)))
  of nkSym:
    let s = n.sym
    let id = env.types.add(typ)
    # bitfields are ignored
    if s.bitsize == 0:
      c.buildDatum RecConstr(^c.rttiV1NodeType,
        *field(MemberV1NodeKind, ^c.genInt(env, 1, UInt8Type, bu)),
        *field(MemberV1Offset,
          Offsetof(^env.types.sizeType, id,
            ^(block:
              var path: seq[NodeRef]
              discard rawFieldAccess(c, env, id, s.position.int32, path, bu)
              path))),
        *field(MemberV1Typ, ^bu.use(getTypeInfoV1(c, env, s.typ, bu))),
        *field(MemberV1NodeName, Value(CstringType, ^s.name.s)))
    else:
      c.buildDatum RecConstr(^c.rttiV1NodeType,
        *field(MemberV1NodeKind, ^c.genInt(env, 0, UInt8Type, bu)))

  else:
    unreachable()

proc genObjectInfo(c; env; typ, orig: PType, bu): NodeRef =
  let
    base =
      if typ[0] != nil:
        bu.use(getTypeInfoV1(c, env, typ[0].skipTypes(skipPtrs), bu))
      else:
        bu.build NilLit()
    node =
      if sfImportc in typ.sym.flags:
        bu.build NilLit()
      else:
        let pt = env.types.newPtr(c.rttiV1NodeType)
        let d = genObjectFields(c, env, typ, typ.n)
        bu.build Addr(pt, ^datumRef(d))

  genTypeInfo(c, env, typ, orig, base, node, bu)

proc genTupleInfo(c; env; typ, orig: PType, bu): NodeRef =
  let pt = env.types.newPtr(c.rttiV1NodeType)
  let node =
    if typ.len > 0:
      let arrType = env.types.newArray(typ.len, pt)
      let id = env.types.add(typ)
      let val = c.buildDatum Constr(^arrType,
        ^collect(
          for i in 0..<typ.len:
            let f = c.buildDatum RecConstr(^c.rttiV1NodeType,
              *field(MemberV1NodeKind, ^c.genInt(env, 1, UInt8Type, bu)),
              *field(MemberV1Offset, Offsetof(^env.types.sizeType, id, i)),
              *field(MemberV1Typ, ^bu.use(getTypeInfoV1(c, env, typ[i], bu))),
              *field(MemberV1NodeName, Value(CstringType, ^("Field" & $i))))
            bu.build Addr(pt, ^datumRef(f))))
      c.buildDatum RecConstr(^c.rttiV1NodeType,
        *field(MemberV1NodeKind, ^c.genInt(env, 2, UInt8Type, bu)),
        *field(MemberV1Len, ^c.genInt(env, typ.len, env.types.sizeType, bu)),
        *field(MemberV1Sons, ^c.genSonsArrayAddr(env, arrType, val, bu)))
    else:
      c.buildDatum RecConstr(^c.rttiV1NodeType,
        *field(MemberV1NodeKind, ^c.genInt(env, 2, UInt8Type, bu)))

  genTypeInfo(c, env, typ, typ,
    bu.build(NilLit()),
    bu.build(Addr(pt, ^datumRef(node))),
    bu)

proc genEnumInfo(c; env; typ, origType: PType, bu): NodeRef =
  let pt = env.types.newPtr(c.rttiV1NodeType)
  let arrayTyp = env.types.newArray(typ.n.len, pt)
  var elems: Datum
  block:
    var bu = initBuilder()
    var enumFields: seq[NodeRef]
    for i in 0..<typ.n.len:
      var field = typ.n[i].sym
      let name =
        if field.ast == nil:
          # no explicit string value for the enum field, so use the name:
          c.module.put(field.name.s)
        else:
          c.module.put(field.ast.strVal)

      let sub = c.buildDatum RecConstr(^c.rttiV1NodeType,
        *field(MemberV1NodeKind, ^c.genInt(env, 0, UInt8Type, bu)),
        *field(MemberV1Offset,
          ^c.genInt(env, field.position, env.types.sizeType, bu)),
        *field(MemberV1NodeName, Value(CstringType, name)))

      enumFields.add bu.build(Addr(pt, ^datumRef(sub)))

    let r = bu.build Constr(arrayTyp, enumFields)
    elems = c.addDatum(bu, r)

  let top = c.buildDatum RecConstr(^c.rttiV1NodeType,
    *field(MemberV1NodeKind, ^c.genInt(env, 2, UInt8Type, bu)),
    *field(MemberV1Len, ^c.genInt(env, typ.n.len, env.types.sizeType, bu)),
    *field(MemberV1Sons, ^c.genSonsArrayAddr(env, arrayTyp, elems, bu)))

  genTypeInfo(c, env, typ, origType,
    bu.build(NilLit()),
    bu.build(Addr(pt, ^datumRef(top))),
    bu)

proc genSetInfo(c; env; typ, origType: PType, bu): NodeRef =
  let pt = env.types.newPtr(c.rttiV1NodeType)
  let L = toInt64(firstOrd(c.graph.config, typ))
  let tmp = c.buildDatum RecConstr(^c.rttiV1NodeType,
    *field(MemberV1Len, ^c.genInt(env, L, env.types.sizeType, bu)),
    *field(MemberV1NodeKind, ^c.genInt(env, 0, UInt8Type, bu)))

  genTypeInfo(c, env, typ, origType,
    bu.build(NilLit()),
    bu.build(Addr(pt, ^datumRef(tmp))),
    bu)

proc fakeClosureType(g: ModuleGraph, owner: PSym): PType =
  ## Creates the type to base the RTTI for closures on (i.e.,
  ## ``tuple[pointer, ref RootObj]``).
  result = newType(tyTuple, nextTypeId g.idgen, owner)
  result.rawAddSon(newType(tyPointer, nextTypeId g.idgen, owner))
  var r = newType(tyRef, nextTypeId g.idgen, owner)
  r.rawAddSon(g.getCompilerProc("RootObj").typ)
  result.rawAddSon(r)

proc openArrayToTuple(g: ModuleGraph; t: PType): PType =
  ## Creates a tuple PType representing the internal layout of an openArray.
  ## For RTTI creation purposes only.
  result = newType(tyTuple, nextTypeId g.idgen, t.owner)
  let p = newType(tyPtr, nextTypeId g.idgen, t.owner)
  let a = newType(tyUncheckedArray, nextTypeId g.idgen, t.owner)
  a.add t.lastSon
  p.add a
  result.add p
  result.add getSysType(g, t.owner.info, tyInt)

proc genTypeInfoV1(c; env; t: PType, bu): NodeRef =
  ## Generates the construction expression for the RTTIv1 object for `t`.
  let origType = t
  let t = t.skipTypes({tyAlias, tyGenericInst, tySink, tyDistinct} +
                       tyUserTypeClasses)
  # note: distinct types are not part of the RTTI, but they are considered when
  # creating the RTTI, in order to use the correct deepcopy hook and names
  case t.kind
  of tyPointer, tyBool, tyChar, tyCstring, tyString, tyInt..tyUInt64, tyVar,
     tyLent:
    genTypeInfo(c, env, t, origType,
      bu.build(NilLit()),
      bu.build(NilLit()),
      bu)
  of tyProc:
    if t.callConv != ccClosure:
      genTypeInfo(c, env, t, origType,
        bu.build(NilLit()),
        bu.build(NilLit()),
        bu)
    else:
      let x = fakeClosureType(c.graph, t.owner)
      genTupleInfo(c, env, x, x, bu)
  of tyPtr, tyRef, tyRange, tyUncheckedArray, tySequence, tyArray:
    genTypeInfoGeneric(c, env, t, origType, bu)
  of tySet:
    genSetInfo(c, env, t, origType, bu)
  of tyEnum:
    genEnumInfo(c, env, t, origType, bu)
  of tyObject:
    genObjectInfo(c, env, t, origType, bu)
  of tyTuple:
    genTupleInfo(c, env, t, origType, bu)
  of tyOpenArray:
    let x = openArrayToTuple(c.graph, t)
    genTupleInfo(c, env, x, origType, bu)
  else:
    unreachable(t.kind)
