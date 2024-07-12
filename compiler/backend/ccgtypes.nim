#
#
#           The Nim Compiler
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from cgen.nim

# ------------------------- Name Mangling --------------------------------

import compiler/sem/sighashes

proc isKeyword(w: PIdent): bool =
  # Nim and C++ share some keywords
  # it's more efficient to test the whole Nim keywords range
  case w.id
  of ccgKeywordsLow..ccgKeywordsHigh,
     nimKeywordsLow..nimKeywordsHigh,
     ord(wInline): return true
  else: return false

proc mangleField(m: BModule; name: string): string =
  result = mangle(name)
  if isKeyword(m.g.graph.cache.getIdent(name)):
    result.add "_0"

proc mangleField(m: BModule; name: PIdent): string =
  result = mangle(name.s)
  # fields are tricky to get right and thanks to generic types producing
  # duplicates we can end up mangling the same field multiple times. However
  # if we do so, the 'cppDefines' table might be modified in the meantime
  # meaning we produce inconsistent field names (see bug #5404).
  # Hence we do not check for ``m.g.config.cppDefines.contains(result)`` here
  # anymore:
  if isKeyword(name):
    result.add "_0"

proc mangleName(g: ModuleGraph; s: PSym): Rope =
  result = s.extname
  if result == "":
    result = s.name.s.mangle.rope
    result.add "__"
    result.add g.ifaces[s.itemId.module].uniqueName
    result.add "_"
    result.add rope s.itemId.item

proc mangleParamName(c: ConfigRef; s: PSym): Rope =
  ## we cannot use 'sigConflicts' here since we don't have access to a BProc.
  ## Fortunately C's scoping rules are sane enough so that that doesn't
  ## cause any trouble.
  if true:
    var res = s.name.s.mangle
    if isKeyword(s.name) or c.cppDefines.contains(res):
      res.add "_0"

    result = res.rope

proc mangleLocalName(p: BProc; name: PIdent, id: LocalId): Rope =
  if name.isNil:
    # use the ID prefixed by an underscore, which is guaranteed to be a unique
    # name within the procedure
    result = "_" & $id.int
  else:
    # mangle the user-defiend name, also accounting for shadowed names
    var key = name.s.mangle
    let counter = p.sigConflicts.getOrDefault(key)
    result = key
    if counter != 0 or isKeyword(name) or p.module.g.config.cppDefines.contains(key):
      result.add "_" & rope(counter+1)
    p.sigConflicts.inc(key)

proc scopeMangledParam(p: BProc; name: PIdent) =
  ## parameter generation doesn't have access to a ``BProc``, so we have to
  ## remember these parameter names are already in scope to be able to
  ## generate unique identifiers reliably (consider that ``var a = a`` is
  ## even an idiom in Nim).
  var key = name.s.mangle
  p.sigConflicts.inc(key)

const
  irrelevantForBackend = {tyGenericBody, tyGenericInst, tyGenericInvocation,
                          tyDistinct, tyRange, tyStatic, tyAlias, tySink,
                          tyInferred}

proc mapSetType(conf: ConfigRef; typ: PType): TCTypeKind =
  ## Legacy procedure.
  case int(getSize(conf, typ))
  of 1: result = ctInt8
  of 2: result = ctInt16
  of 4: result = ctInt32
  of 8: result = ctInt64
  else: result = ctArray

proc mapType(types: TypeEnv, typ: TypeId): TCTypeKind

proc mapType(types: TypeEnv; desc: TypeHeader): TCTypeKind =
  ## Maps a NimSkull type to the corresponding C type.
  case desc.kind
  of tkIndirect, tkImported:
    mapType(types, desc.elem)
  of tkVoid: ctVoid
  of tkBool: ctBool
  of tkChar: ctChar
  of tkInt:
    case desc.size(types)
    of 1: ctInt8
    of 2: ctInt16
    of 4: ctInt32
    of 8: ctInt64
    else: unreachable()
  of tkUInt:
    case desc.size(types)
    of 1: ctUInt8
    of 2: ctUInt16
    of 4: ctUInt32
    of 8: ctUInt64
    else: unreachable()
  of tkFloat:
    case desc.size(types)
    of 4: ctFloat32
    of 8: ctFloat64
    else: unreachable()
  of tkArray, tkUncheckedArray:
    ctArray
  of tkRecord, tkUnion:
    ctStruct
  of tkPtr, tkRef, tkLent, tkVar:
    case mapType(types, desc.elem)
    of ctArray:
      ctPtrToArray
    else:
      ctPtr
  of tkPointer: ctPtr
  of tkProc: ctProc
  of tkCstring: ctCString
  # handling of the non-lowered types follows:
  # XXX: this needs to eventually be removed; the whole code generator needs
  #      to only operate on the lowered types
  of tkSet:
    case desc.size(types)
    of 1: ctInt8
    of 2: ctInt16
    of 4: ctInt32
    of 8: ctInt64
    else: ctArray
  of tkOpenArray: ctNimOpenArray
  of tkSeq:       ctNimSeq
  of tkString:    ctNimStr
  of tkClosure:   ctStruct
  else:
    unreachable(desc.kind)

proc mapType(types: TypeEnv, typ: TypeId): TCTypeKind =
  mapType(types, types.headerFor(typ, Original))

proc mapType(m: BModule; typ: PType): TCTypeKind =
  ## Legacy procedure. Bridges the old to the new types.
  let id = m.addLate(typ)
  mapType(m.types, id)

proc mapReturnType(m: BModule; typ: PType): TCTypeKind =
  #if skipTypes(typ, typedescInst).kind == tyArray: result = ctPtr
  #else:
  result = mapType(m, typ)

proc getTypeDesc(m: BModule, typ: PType): Rope
proc useType(m: BModule, typ: TypeId, desc: TypeHeader; onlyName = false): Rope

proc containsGarbageCollectedRef(env: TypeEnv, typ: TypeId): bool =
  ## Computes whether `typ` is or contains a garbage-collected type.
  let n = env.get(typ).desc[Canonical]
  case env[n].kind
  of tkRef, tkClosure:
    result = true
  of tkArray:
    result = containsGarbageCollectedRef(env, env[n].elem)
  of tkRecord:
    var rec = typ
    # traverse the object hierarchy:
    while rec != VoidType:
      for (_, f) in env.fields(env.headerFor(rec, Canonical)):
        # is the field a garbabe-collected reference?
        if containsGarbageCollectedRef(env, f.typ):
          return true

      rec = env.headerFor(rec, Canonical).base(env)

    result = false
  else:
    # neither an aggregate type nor a garbage-collected ref
    result = false

proc usesRvo(types: TypeEnv, typ: TypeId): bool =
  ## Computes for the record type `typ` whether it uses the return-value
  ## optimization.
  # seek to the root type in the inheritance hierarchy:
  var base = typ
  while (let next = types.headerFor(base, Lowered).base(types);
         next != VoidType):
    base = next

  # does the type have a RTTI header? does the record have a garbage-collected
  # field?
  # XXX: these rules originate from the refc days, where they were important
  #      for efficiency. This is no longer the case, and it'd make sense to
  #      make usage of RVO dependent on the *size* of the type instead
  result = types.headerFor(base, Lowered).fieldOffset(types) == -1 or
           containsGarbageCollectedRef(types, typ)

proc isInvalidReturnType(types: TypeEnv; typ: TypeId): bool =
  # Arrays cannot be returned by a C procedure, because C is
  # such a poor programming language.
  # We exclude records with refs too. This enhances efficiency.
  # keep synchronized with ``mirpasses.eligibleForRvo``
  let typ = types.canonical(typ)
  case types.headerFor(typ, Lowered).kind
  of tkArray:  true
  of tkRecord: usesRvo(types, typ)
  else:        false

proc isInvalidReturnType(m: BModule; rettype: PType): bool =
  ## Legacy procedure; only exists for bridging the old to the new code.
  if rettype.isNil:
    false # nil stands for void, which is a valid return type
  else:
    let id = m.addLate(rettype)
    isInvalidReturnType(m.types, id)

const
  CallingConvToStr: array[TCallingConvention, string] = ["N_NIMCALL",
    "N_STDCALL", "N_CDECL", "N_SAFECALL",
    "N_SYSCALL", # this is probably not correct for all platforms,
                 # but one can #define it to what one wants
    "N_INLINE", "N_NOINLINE", "N_FASTCALL", "N_CLOSURE", "N_NOCONV"]

proc addAbiCheck(m: BModule, t: PType, name: Rope) =
  if isDefined(m.config, "checkAbi") and (let size = getSize(m.config, t); size != szUnknownSize):
    var msg = "backend & Nim disagree on size for: "
    msg.addTypeHeader(m.config, t)
    var msg2 = ""
    msg2.addQuoted msg # not a hostspot so extra allocation doesn't matter
    m.s[cfsTypeInfo].addf("NIM_STATIC_ASSERT(sizeof($1) == $2, $3);$n", [name, rope(size), msg2.rope])
    # see `testCodegenABICheck` for example error message it generates

proc ccgIntroducedPtr(conf: ConfigRef; s: PSym, retType: PType): bool =
  var pt = skipTypes(s.typ, typedescInst)
  assert skResult != s.kind

  if tfByRef in pt.flags: return true
  elif tfByCopy in pt.flags: return false
  case pt.kind
  of tyObject:
    if s.typ.sym != nil and sfForward in s.typ.sym.flags:
      # forwarded objects are *always* passed by pointers for consistency!
      result = true
    elif (optByRef in s.options) or (getSize(conf, pt) > conf.target.floatSize * 3):
      result = true           # requested anyway
    elif (tfFinal in pt.flags) and (pt[0] == nil):
      result = false          # no need, because no subtyping possible
    else:
      result = true           # ordinary objects are always passed by reference,
                              # otherwise casting doesn't work
  of tyTuple:
    result = (getSize(conf, pt) > conf.target.floatSize*3) or (optByRef in s.options)
  else:
    result = false
  # first parameter and return type is 'lent T'? --> use pass by pointer
  if s.position == 0 and retType != nil and retType.kind == tyLent:
    result = not (pt.kind in {tyVar, tyArray, tyOpenArray, tyVarargs, tyRef, tyPtr, tyPointer} or
      pt.kind == tySet and mapSetType(conf, pt) == ctArray)

proc initResultParamLoc(m: BModule; param: CgNode): TLoc =
  result = initLoc(locParam, param, "Result", OnStack)
  let t = param.typ
  if mapReturnType(m, t) != ctArray and isInvalidReturnType(m, t):
    incl(result.flags, lfIndirect)
    result.storage = OnUnknown

proc useType(m: BModule, typ: TypeId; onlyName = false): Rope =
  useType(m, typ, m.types.headerFor(typ, Lowered), onlyName)

proc getSimpleTypeDesc(m: BModule, typ: PType): Rope =
  useType(m, m.addLate(typ))

proc structOrUnion(kind: TypeKind): Rope =
  case kind
  of tkRecord: "struct"
  of tkUnion:  "union"
  else:        unreachable(kind)

proc addForwardStructFormat(m: BModule, structOrUnion: Rope, typename: Rope) =
  m.s[cfsForwardTypes].addf "typedef $1 $2 $2;$n", [structOrUnion, typename]

proc getTypeForward(m: BModule, typ: TypeId, desc: TypeHeader): Rope =
  result = m.forwTypeCache.getOrDefault(typ)
  if result != "": return
  result = m.typeCache.getOrDefault(typ)
  if result != "": return

  case desc.kind
  of tkRecord, tkUnion:
    result = computeTypeName(m.g.graph, m.types, typ)
    addForwardStructFormat(m, structOrUnion(desc.kind), result)
    m.forwTypeCache[typ] = result
  of tkImported:
    result = useType(m, typ, desc)
  else:
    unreachable(desc.kind)

proc payloadType(m: BModule, typ: TypeId): TypeId =
  # the second field is a pointer to the payload type
  m.types.headerFor(m.types[lookupField(m.types, typ, 1)].typ, Lowered).elem

proc getSeqPayloadType(m: BModule; t: PType): Rope =
  let typ = m.addLate(t)
  result = useType(m, payloadType(m, typ))

proc prepareParameters(m: BModule, t: PType): seq[TLoc] =
  ## Sets up and returns the locs of the parameter symbols for procedure
  ## type `t`. The loc for the first parameter is stored at index 1.
  ##
  ## Neither the loc for the 'result' nor for the hidden environment parameter
  ## are filled-in here. 'result' might not be a parameter, and the hidden
  ## environment parameter is currently always treated as a local variable.
  assert t.kind == tyProc
  let params = t.n
  result.newSeq(params.len)

  for i in 1..<params.len:
    let param = params[i].sym
    if isCompileTimeOnly(param.typ):
      # ignore parameters only relevant for overloading (e.g., ``static[T]``,
      # etc.); already filled-in parameters are also skipped
      continue

    let storage =
      if mapType(m, param.typ.skipTypes({tyVar, tyLent})) == ctArray:
        # something that's represented as a C array. Since an indirection is
        # involved, we don't know where the location resides
        OnUnknown
      else:
        OnStack

    result[i] = initLoc(locParam, newLocalRef(LocalId(i), param.info, param.typ),
                        mangleParamName(m.config, param), storage)

    if ccgIntroducedPtr(m.config, param, t[0]):
      # the parameter is passed by address; mark it as indirect
      incl(result[i].flags, lfIndirect)
      result[i].storage = OnUnknown

proc prepareParameters(m: BModule, desc: TypeHeader): seq[TLoc] =
  assert desc.kind == tkProc
  result.newSeq(desc.numParams + 1)

  for i, it, flags in params(m.types, desc):
    let i = i + 1
    if it == VoidType:
      # ignore compile-time only parameters
      # XXX: the parameter needs to be eliminated during to-MIR translation
      #      already
      continue

    # the loc sequence is only used for procedure type emission, so using nil
    # as the node is fine
    result[i] = initLoc(locParam, nil, "_" & $i, OnUnknown)

    if pfByRef in flags and mapType(m.types, it) != ctArray:
      # the parameter is passed by address; mark it as indirect
      incl(result[i].flags, lfIndirect)
      result[i].storage = OnUnknown

proc genParamDecl(m: BModule, id: TypeId, params: var Rope, name: string,
                  indirect, noAlias, weakDep: bool) =
  ## Emits the declaration for a parameter.
  case mapType(m.types, id)
  of ctNimOpenArray:
    # an openArray tuple is "unpacked" into the parameter list (it takes up
    # two parameters)
    params.add useType(m, m.types[m.types.lookupField(id, 0)].typ)
    params.add " "
    if noAlias:
      params.add("NIM_NOALIAS ")
    params.add(name)
    params.addf(", NI $1Len_0", [name])
  elif indirect:
    params.add useType(m, id, onlyName=true)
    params.add("* ")
    if noAlias:
      params.add("NIM_NOALIAS ")
    params.add(name)
  else:
    params.add useType(m, id, onlyName=weakDep)
    if noAlias:
      params.add(" NIM_NOALIAS")
    params.add(" ")
    params.add(name)

proc finishProcType(m: BModule, ret: TypeId, withEnv, withVarargs: bool,
                    params: var string) =
  ## Appends the return type as a parameter, and wraps `params` in parenthesis.
  if isInvalidReturnType(m.types, ret):
    if params != "":
      params.add ", "
    if mapType(m.g.env.types, ret) == ctArray:
      params.add useType(m, ret)
    else:
      params.add useType(m, ret, onlyName=true)
      params.add "*"
    params.add " Result"

  if withEnv:
    if params != "":
      params.add ", "
    params.add "void* ClE_0"

  if withVarargs:
    if params != "":
      params.add ", "
    params.add "..."

  if params == "": params.add("void)")
  else: params.add(")")
  params = "(" & params

proc genProcParams(m: BModule, desc: TypeHeader, ret, params: var Rope,
                   locs: openArray[TLoc], weakDep: bool) =
  assert desc.kind == tkProc
  params = ""
  let rt = desc.retType(m.types)
  if isInvalidReturnType(m.types, rt):
    ret = ~"void"
  else:
    ret = useType(m, rt, weakDep)

  for i, typ, flags in params(m.types, desc):
    let i = i + 1
    if locs[i].k == locNone: continue
    if params != "": params.add(~", ")
    genParamDecl(m, typ, params, locs[i].r, lfIndirect in locs[i].flags,
                 noAlias=false, weakDep)

  finishProcType(m, rt, desc.callConv(m.types) == ccClosure,
                 desc.hasVarargs(m.types), params)

proc genProcParams(m: BModule, t: PType, rettype, params: var string,
                   locs: openArray[TLoc]) =
  ## Legacy procedure for contexts where type IDs aren't yet used.
  params = ""
  let rty = m.addLate(t[0])
  if isInvalidReturnType(m.g.env.types, rty):
    rettype = ~"void"
  else:
    rettype = useType(m, rty)

  for i in 1..<t.len:
    if locs[i].k == locNone: continue
    if params != "": params.add(~", ")
    let ty = m.addLate(t[i])
    genParamDecl(m, ty, params, locs[i].r, lfIndirect in locs[i].flags,
                 sfNoalias in t.n[i].sym.flags, weakDep=false)

  finishProcType(m, rty, t.callConv == ccClosure, tfVarargs in t.flags, params)

proc mangleRecFieldName(m: BModule; field: PSym): Rope =
  if {sfImportc, sfExportc} * field.flags != {}:
    result = field.extname
  else:
    result = rope(mangleField(m, field.name))
  m.config.internalAssert(result != "", field.info, "mangleRecFieldName")

proc mangleDynLibProc(sym: PSym): Rope

proc genDecl(m: BModule, result: var Rope, typ: TypeId, name: Rope,
             align, bitsize: int, noAlias: bool) =
  let desc = m.types.headerFor(typ, Lowered)
  if align != 0 and align != desc.align:
    result.addf("NIM_ALIGN($1) ", [$align])

  case desc.kind
  of tkVoid:
    # XXX: void fields need to be eliminated during PType->MIR translation
    #      instead
    discard "drop"
  of tkUncheckedArray:
    result.add useType(m, desc.elem)
    result.addf(" $1[SEQ_DECL_SIZE]", [name])
  of tkArray:
    result.add useType(m, desc.elem)
    result.addf(" $1[$2]", [name, $desc.arrayLen(m.types)])
  else:
    result.add useType(m, typ)
    if noAlias:
      result.add " NIM_NOALIAS"
    result.add " " & name
    if bitsize != 0:
      result.add ":" & $bitsize

proc genFieldDesc(m: BModule, id: FieldId, field: RecField, pos: int,
                  result: var Rope, accessor: string) =
  var mangled: string
  if not field.isNamed:
    # use a name derived from the position for anonymous fields. The name can
    # easily be reconstructed from anywhere, so it's not cached
    mangled = "Field" & $pos
  elif field.isNoMangle:
    mangled = m.types.name(field)
    m.g.fields[id] = accessor & mangled
  else:
    mangled = mangleField(m, m.types.name(field))
    # for efficiency, cache the name combined with the accessor
    m.g.fields[id] = accessor & mangled

  genDecl(m, result, field.typ, mangled, field.align, field.bitsize,
          field.isNoAlias)
  result.add ";\n"

proc getTaggedUnionDesc(m: BModule, desc: TypeHeader, result: var Rope,
                        accessor: string)

proc genRecordDesc(m: BModule, desc: TypeHeader, result: var Rope,
                   accessor: string) =
  if desc.isPacked(m.types) and hasAttribute in CC[m.config.cCompiler].props:
    # if only push/pop are supported, the outer struct is already wrapped in a
    # pair of those
    result.add "struct __attribute__((__packed__)) {\n"
  else:
    result.add "struct {\n"

  var pos = 0
  for (id, it) in m.types.fields(desc):
    if m.types.isEmbedded(it.typ):
      # embedded tagged union
      getTaggedUnionDesc(m, m.types.headerFor(it.typ, Lowered), result,
                         accessor)
    else:
      genFieldDesc(m, id, it, pos, result, accessor)
    inc pos

  result.add "}"

proc getTaggedUnionDesc(m: BModule, desc: TypeHeader, result: var Rope,
                        accessor: string) =
  # the discriminator is directly embedded into the surrounding struct
  let
    id    = desc.discr(m.types)
    discr = m.types[id]
  genFieldDesc(m, id, discr, 0, result, accessor)

  let
    name = m.types.name(discr)
    # all ``struct`` union fields use the mangled discriminator field name
    # as the prefix
    unionPrefix =
      if discr.isNoMangle:
        "_" & name & "_"
      else:
        "_" & mangleField(m, name) & "_"

  result.add "union {\n"
  var i = 1
  for (id, it) in m.types.fields(desc, 1):
    if m.types.isEmbedded(it.typ):
      # embedded record description. The accessor combined with the union
      # field name is passed along
      genRecordDesc(m, m.types.headerFor(it.typ, Lowered), result,
                    accessor & unionPrefix & $i & ".")
      result.addf(" $1$2;$n", [unionPrefix, $i])
    else:
      genFieldDesc(m, id, it, i, result, accessor)
      result.add ";\n"

    inc i

  result.add "};\n"

proc genUnionDesc(m: BModule, desc: TypeHeader, name: Rope, result: var Rope) =
  result.add "union "
  result.add name
  result.add " {\n"
  for (id, it) in m.types.fields(desc):
    genFieldDesc(m, id, it, 0, result, "")
  result.add "}"

proc genRecordDesc(m: BModule, desc: TypeHeader, name: Rope, result: var Rope) =
  let isPacked = desc.isPacked(m.types)

  if isPacked:
    if hasAttribute in CC[m.config.cCompiler].props:
      result.add "struct __attribute__((__packed__)) "
    else:
      result.add "#pragma pack(push, 1)\nstruct "
  else:
    result.add "struct "
  result.add name

  result.add " {\n"
  # the base type (if any) is added as the first field:
  if desc.base(m.types) != VoidType:
    result.add useType(m, desc.base(m.types))
    result.add " Sup;\n"

  var pos = 0
  for (id, it) in m.g.env.types.fields(desc):
    if m.types.isEmbedded(it.typ):
      # embedded tagged union
      getTaggedUnionDesc(m, m.types.headerFor(it.typ, Lowered), result, "")
    else:
      genFieldDesc(m, id, it, pos, result, "")
    inc pos

  result.add "}"
  # pop the packed pragma again:
  if isPacked and
     hasAttribute notin CC[m.config.cCompiler].props:
    result.add "#pragma pack(pop)\n"

proc isImportedType(t: PType): bool =
  t.sym != nil and sfImportc in t.sym.flags

proc emitTypeDef(m: BModule, id: TypeId, desc: TypeHeader) =
  ## Emits the full definition for the type.
  if id in m.typeCache:
    return

  let name = computeTypeName(m.g.graph, m.types, id)
  assert name != ""
  # remember that the type was emitted. Doing it before producing the type
  # body ensures that cyclic types don't result in infinite recursion:
  m.typeCache[id] = name

  case desc.kind
  of tkArray:
    let elem = useType(m, desc.elem)
    m.s[cfsTypes].addf("typedef $1 $2[$3];$n",
                       [elem, name, $desc.arrayLen(m.types)])
  of tkUncheckedArray:
    let elem = useType(m, desc.elem)
    m.s[cfsTypes].addf("typedef $1 $2[1];$n",
                       [elem, name])
  of tkProc:
    let locs = prepareParameters(m, desc)
    var rettype, params: string
    genProcParams(m, desc, rettype, params, locs, weakDep=true)
    m.s[cfsTypes].addf("typedef $1_PTR($2, $3)$4;$n",
                       [rope(CallingConvToStr[desc.callConv(m.types)]),
                        rettype, name, params])
  of tkRecord, tkUnion:
    if id notin m.forwTypeCache:
      m.forwTypeCache[id] = name
      addForwardStructFormat(m, structOrUnion(desc.kind), name)

    var recdesc: Rope
    if desc.kind == tkRecord:
      genRecordDesc(m, desc, name, recdesc)
    else:
      genUnionDesc(m, desc, name, recdesc)
    m.s[cfsTypes].addf("$1;$n", [recdesc])
  of tkImported:
    let t = m.types[id]
    useHeader(m, t.sym)
    addAbiCheck(m, t, name)

    # fill in the field names for records:
    let h = m.types.headerFor(desc.elem, Lowered)
    case h.kind
    of tkRecord:
      var tmp: Rope; genRecordDesc(m, h, "", tmp)
    of tkUnion:
      var tmp: Rope; genUnionDesc(m, h, "", tmp)
    else:
      discard "nothing to do"
  else:
    unreachable("doesn't need a full definition")

proc useType(m: BModule, typ: TypeId, desc: TypeHeader; onlyName = false): Rope =
  case desc.kind
  of tkVoid:
    result = "void"
  of tkPointer:
    result = "void*"
  of tkCstring:
    result = "NCSTRING"
  of tkBool:
    result = "NIM_BOOL"
  of tkChar:
    result = "NIM_CHAR"
  of tkInt:
    result = "NI" & $(desc.size(m.types) * 8)
  of tkUInt:
    result = "NU" & $(desc.size(m.types) * 8)
  of tkFloat:
    result = "NF" & $(desc.size(m.types) * 8)
  of tkRef, tkPtr, tkVar, tkLent:
    if mapType(m.types, desc) == ctPtrToArray:
      # use ``T*``, where `T` is the array's element type
      result = useType(m, m.types.headerFor(desc.elem, Lowered).elem,
                       onlyName=true)
    else:
      # only the pointee's name is required, not its full definition
      result = useType(m, desc.elem, onlyName=true)
    result.add "*"
  of tkImported, tkProc, tkArray, tkUncheckedArray:
    # definition is the same as only a forward declaration
    emitTypeDef(m, typ, desc)
    result = m.typeCache[typ]
  of tkRecord, tkUnion:
    if onlyName:
      # a forward declaration suffices
      result = getTypeForward(m, typ, desc)
    else:
      emitTypeDef(m, typ, desc)
      result = m.typeCache[typ]
  else:
    unreachable(desc.kind)

proc getTypeDesc(m: BModule, typ: PType): Rope =
  result = useType(m, m.addLate(typ))

type
  TClosureTypeKind = enum ## In C closures are mapped to 3 different things.
    clHalf,           ## fn(args) type without the trailing 'void* env' parameter
    clHalfWithEnv,    ## fn(args, void* env) type with trailing 'void* env' parameter
    clFull            ## struct {fn(args, void* env), env}

proc getClosureType(m: BModule, t: PType, kind: TClosureTypeKind): Rope =
  case kind
  of clHalf:
    # create a proc type with all of `t`'s parameters, except for the
    # environment pointer
    let canon {.cursor.} =
      m.types.headerFor(m.types.canonical(m.types[t]), Canonical)

    let pt = m.types.buildProc(tkProc, ccNimCall, canon.retType(m.types), bu):
      for (_, typ, flags) in params(m.types, canon):
        bu.addParam(flags, typ)

    result = useType(m, pt)
  of clHalfWithEnv:
    let c = m.types.canonical(m.types[t])
    result = useType(m, m.types[m.types.lookupField(c, 0)].typ)
  of clFull:
    result = getTypeDesc(m, t)

proc genProcHeader(m: BModule, prc: PSym, locs: openArray[TLoc]): Rope =
  ## Generates the C function header for `prc`, with `locs` being the locs
  ## of the formal parameters.
  var
    rettype, params: Rope
  # using static is needed for inline procs
  if exfExportLib in prc.extFlags:
    if isHeaderFile in m.flags:
      result.add "N_LIB_IMPORT "
    else:
      result.add "N_LIB_EXPORT "
  elif prc.typ.callConv == ccInline:
    result.add "static "
  elif sfImportc notin prc.flags:
    result.add "N_LIB_PRIVATE "
  genProcParams(m, prc.typ, rettype, params, locs)

  # careful here! don't access ``prc.ast`` as that could reload large parts of
  # the object graph!
  result.addf("$1($2, $3)$4",
        [rope(CallingConvToStr[prc.typ.callConv]), rettype,
         m.procs[m.g.env.procedures[prc]].name, params])

# ------------------ type info generation -------------------------------------

proc genTypeInfoV1(m: BModule, t: PType; info: TLineInfo): Rope
proc getNimNode(m: BModule): Rope =
  result = "$1[$2]" % [m.typeNodesName, rope(m.typeNodes)]
  inc(m.typeNodes)

proc genTypeInfoAuxBase(m: BModule; typ, origType: PType;
                        name, base: Rope; info: TLineInfo) =
  var nimtypeKind: int
  #allocMemTI(m, typ, name)
  if isObjLackingTypeField(typ):
    nimtypeKind = ord(tyPureObject)
  else:
    nimtypeKind = ord(typ.kind)

  var size: Rope
  if tfIncompleteStruct in typ.flags:
    size = rope"void*"
  else:
    size = getTypeDesc(m, origType)
  m.s[cfsTypeInit3].addf(
    "$1.size = sizeof($2);$n$1.align = NIM_ALIGNOF($2);$n$1.kind = $3;$n$1.base = $4;$n",
    [name, size, rope(nimtypeKind), base]
  )
  # compute type flags for GC optimization
  var flags = 0
  if not containsGarbageCollectedRef(typ): flags = flags or 1
  if not canFormAcycle(typ): flags = flags or 2
  #else echo("can contain a cycle: " & typeToString(typ))
  if flags != 0:
    m.s[cfsTypeInit3].addf("$1.flags = $2;$n", [name, rope(flags)])
  discard cgsym(m, "TNimType")
  if isDefined(m.config, "nimTypeNames"):
    var typename = typeToString(if origType.typeInst != nil: origType.typeInst
                                else: origType, preferName)
    if typename == "ref object" and origType.skipTypes(skipPtrs).sym != nil:
      typename = "anon ref object from " & m.config$origType.skipTypes(skipPtrs).sym.info
    m.s[cfsTypeInit3].addf("$1.name = $2;$n",
        [name, makeCString typename])
    discard cgsym(m, "nimTypeRoot")
    m.s[cfsTypeInit3].addf("$1.nextType = nimTypeRoot; nimTypeRoot=&$1;$n",
         [name])

  m.s[cfsData].addf("N_LIB_PRIVATE TNimType $1;$n", [name])

proc genTypeInfoAux(m: BModule, typ, origType: PType, name: Rope;
                    info: TLineInfo) =
  var base: Rope
  if typ.len > 0 and typ.lastSon != nil:
    var x = typ.lastSon
    if typ.kind == tyObject: x = x.skipTypes(skipPtrs)
    if typ.kind == tyPtr and x.kind == tyObject and incompleteType(x):
      base = rope("0")
    else:
      base = genTypeInfoV1(m, x, info)
  else:
    base = rope("0")
  genTypeInfoAuxBase(m, typ, origType, name, base, info)

proc discriminatorTableName(m: BModule, objtype: PType, d: PSym): Rope =
  # bugfix: we need to search the type that contains the discriminator:
  var objtype = objtype.skipTypes(abstractPtrs)
  while lookupInRecord(objtype.n, d.name) == nil:
    objtype = objtype[0].skipTypes(abstractPtrs)
  m.config.internalAssert(objtype.sym != nil, d.info, "anonymous obj with discriminator")
  result = "NimDT_$1_$2" % [rope($hashType(objtype)), rope(d.name.s.mangle)]

proc rope(arg: Int128): Rope = rope($arg)

proc genTNimNodeArray(m: BModule, name: Rope, size: Rope) =
  m.s[cfsTypeInit1].addf("static TNimNode* $1[$2];$n", [name, size])

proc genObjectFields(m: BModule, typ, origType: PType, n: PNode, expr: Rope;
                     info: TLineInfo) =
  case n.kind
  of nkRecList:
    if n.len == 1:
      genObjectFields(m, typ, origType, n[0], expr, info)
    elif n.len > 0:
      var tmp = getTempName(m) & "_" & $n.len
      genTNimNodeArray(m, tmp, rope(n.len))
      for i in 0..<n.len:
        var tmp2 = getNimNode(m)
        m.s[cfsTypeInit3].addf("$1[$2] = &$3;$n", [tmp, rope(i), tmp2])
        genObjectFields(m, typ, origType, n[i], tmp2, info)
      m.s[cfsTypeInit3].addf("$1.len = $2; $1.kind = 2; $1.sons = &$3[0];$n",
           [expr, rope(n.len), tmp])
    else:
      m.s[cfsTypeInit3].addf("$1.len = $2; $1.kind = 2;$n", [expr, rope(n.len)])
  of nkRecCase:
    assert(n[0].kind == nkSym)
    var field = n[0].sym
    var tmp = discriminatorTableName(m, typ, field)
    var L = lengthOrd(m.config, field.typ)
    assert L > 0
    let id = m.g.env.types.lookupField(m.types[origType], field.position.int32)
    m.s[cfsTypeInit3].addf("$1.kind = 3;$n" &
        "$1.offset = offsetof($2, $3);$n" & "$1.typ = $4;$n" &
        "$1.name = $5;$n" & "$1.sons = &$6[0];$n" &
        "$1.len = $7;$n", [expr, getTypeDesc(m, origType),
                           m.fields[id],
                           genTypeInfoV1(m, field.typ, info),
                           makeCString(field.name.s),
                           tmp, rope(L)])
    m.s[cfsData].addf("TNimNode* $1[$2];$n", [tmp, rope(L+1)])
    for i in 1..<n.len:
      var b = n[i]           # branch
      var tmp2 = getNimNode(m)
      genObjectFields(m, typ, origType, lastSon(b), tmp2, info)
      case b.kind
      of nkOfBranch:
        m.config.internalAssert(b.len >= 2, b.info, "genObjectFields; nkOfBranch broken")
        for j in 0..<b.len - 1:
          if b[j].kind == nkRange:
            var x = toInt(getOrdValue(b[j][0]))
            var y = toInt(getOrdValue(b[j][1]))
            while x <= y:
              m.s[cfsTypeInit3].addf("$1[$2] = &$3;$n", [tmp, rope(x), tmp2])
              inc(x)
          else:
            m.s[cfsTypeInit3].addf("$1[$2] = &$3;$n",
                 [tmp, rope(getOrdValue(b[j])), tmp2])
      of nkElse:
        m.s[cfsTypeInit3].addf("$1[$2] = &$3;$n",
             [tmp, rope(L), tmp2])
      else: internalError(m.config, n.info, "genObjectFields(nkRecCase)")
  of nkSym:
    var field = n.sym
    # Do not produce code for void types
    if isEmptyType(field.typ): return
    if field.bitsize == 0:
      let id = m.g.env.types.lookupField(m.types[origType], field.position.int32)
      m.s[cfsTypeInit3].addf("$1.kind = 1;$n" &
          "$1.offset = offsetof($2, $3);$n" & "$1.typ = $4;$n" &
          "$1.name = $5;$n", [expr, getTypeDesc(m, origType),
          m.fields[id], genTypeInfoV1(m, field.typ, info),
          makeCString(field.name.s)])
  else: internalError(m.config, n.info, "genObjectFields")

proc genObjectInfo(m: BModule, typ, origType: PType, name: Rope; info: TLineInfo) =
  if typ.kind == tyObject:
    if incompleteType(typ):
      localReport(m.config, info, reportTyp(
        rsemRttiRequestForIncompleteObject, typ))

    genTypeInfoAux(m, typ, origType, name, info)
  else:
    genTypeInfoAuxBase(m, typ, origType, name, rope("0"), info)
  var tmp = getNimNode(m)
  if not isImportedType(typ):
    genObjectFields(m, typ, origType, typ.n, tmp, info)
  m.s[cfsTypeInit3].addf("$1.node = &$2;$n", [name, tmp])
  var t = typ[0]
  while t != nil:
    t = t.skipTypes(skipPtrs)
    t.flags.incl tfObjHasKids
    t = t[0]

proc genTupleInfo(m: BModule, typ, origType: PType, name: Rope; info: TLineInfo) =
  genTypeInfoAuxBase(m, typ, typ, name, rope("0"), info)
  var expr = getNimNode(m)
  if typ.len > 0:
    var tmp = getTempName(m) & "_" & $typ.len
    genTNimNodeArray(m, tmp, rope(typ.len))
    for i in 0..<typ.len:
      var a = typ[i]
      var tmp2 = getNimNode(m)
      m.s[cfsTypeInit3].addf("$1[$2] = &$3;$n", [tmp, rope(i), tmp2])
      m.s[cfsTypeInit3].addf("$1.kind = 1;$n" &
          "$1.offset = offsetof($2, Field$3);$n" &
          "$1.typ = $4;$n" &
          "$1.name = \"Field$3\";$n",
           [tmp2, getTypeDesc(m, origType), rope(i), genTypeInfoV1(m, a, info)])
    m.s[cfsTypeInit3].addf("$1.len = $2; $1.kind = 2; $1.sons = &$3[0];$n",
         [expr, rope(typ.len), tmp])
  else:
    m.s[cfsTypeInit3].addf("$1.len = $2; $1.kind = 2;$n",
         [expr, rope(typ.len)])
  m.s[cfsTypeInit3].addf("$1.node = &$2;$n", [name, expr])

proc genEnumInfo(m: BModule, typ: PType, name: Rope; info: TLineInfo) =
  # Type information for enumerations is quite heavy, so we do some
  # optimizations here: The ``typ`` field is never set, as it is redundant
  # anyway. We generate a cstring array and a loop over it. Exceptional
  # positions will be reset after the loop.
  genTypeInfoAux(m, typ, typ, name, info)
  var nodePtrs = getTempName(m) & "_" & $typ.n.len
  genTNimNodeArray(m, nodePtrs, rope(typ.n.len))
  var enumNames, specialCases: Rope
  var firstNimNode = m.typeNodes
  var hasHoles = false
  for i in 0..<typ.n.len:
    assert(typ.n[i].kind == nkSym)
    var field = typ.n[i].sym
    var elemNode = getNimNode(m)
    if field.ast == nil:
      # no explicit string literal for the enum field, so use field.name:
      enumNames.add(makeCString(field.name.s))
    else:
      enumNames.add(makeCString(field.ast.strVal))
    if i < typ.n.len - 1: enumNames.add(", \L")
    if field.position != i or tfEnumHasHoles in typ.flags:
      specialCases.addf("$1.offset = $2;$n", [elemNode, rope(field.position)])
      hasHoles = true
  var enumArray = getTempName(m)
  var counter = getTempName(m)
  m.s[cfsTypeInit1].addf("NI $1;$n", [counter])
  m.s[cfsTypeInit1].addf("static char* NIM_CONST $1[$2] = {$n$3};$n",
       [enumArray, rope(typ.n.len), enumNames])
  m.s[cfsTypeInit3].addf("for ($1 = 0; $1 < $2; $1++) {$n" &
      "$3[$1+$4].kind = 1;$n" & "$3[$1+$4].offset = $1;$n" &
      "$3[$1+$4].name = $5[$1];$n" & "$6[$1] = &$3[$1+$4];$n" & "}$n", [counter,
      rope(typ.n.len), m.typeNodesName, rope(firstNimNode), enumArray, nodePtrs])
  m.s[cfsTypeInit3].add(specialCases)
  m.s[cfsTypeInit3].addf(
       "$1.len = $2; $1.kind = 2; $1.sons = &$3[0];$n$4.node = &$1;$n",
       [getNimNode(m), rope(typ.n.len), nodePtrs, name])
  if hasHoles:
    # 1 << 2 is {ntfEnumHole}
    m.s[cfsTypeInit3].addf("$1.flags = 1<<2;$n", [name])

proc genSetInfo(m: BModule, typ: PType, name: Rope; info: TLineInfo) =
  assert(typ[0] != nil)
  genTypeInfoAux(m, typ, typ, name, info)
  var tmp = getNimNode(m)
  m.s[cfsTypeInit3].addf("$1.len = $2; $1.kind = 0;$n" & "$3.node = &$1;$n",
       [tmp, rope(firstOrd(m.config, typ)), name])

proc genArrayInfo(m: BModule, typ: PType, name: Rope; info: TLineInfo) =
  genTypeInfoAuxBase(m, typ, typ, name, genTypeInfoV1(m, typ[1], info), info)

proc fakeClosureType(m: BModule; owner: PSym): PType =
  # we generate the same RTTI as for a tuple[pointer, ref tuple[]]
  result = newType(tyTuple, nextTypeId m.idgen, owner)
  result.rawAddSon(newType(tyPointer, nextTypeId m.idgen, owner))
  var r = newType(tyRef, nextTypeId m.idgen, owner)
  let obj = createObj(m.g.graph, m.idgen, owner, owner.info, final=false)
  r.rawAddSon(obj)
  result.rawAddSon(r)

proc useHook(m: BModule, s: PSym): ProcedureId =
  if s in m.g.env.procedures:
    result = m.g.env.procedures[s]
  else:
    result = m.g.env.procedures.add(s)
    # adding a module override is only necessary when it's a new procedure
    m.g.hooks.add (m, result)

  useProc(m, result)

proc genDeepCopyProc(m: BModule; s: PSym; result: Rope) =
  let id = useHook(m, s)
  m.s[cfsTypeInit3].addf("$1.deepcopy =(void* (N_RAW_NIMCALL*)(void*))$2;$n",
     [result, m.procs[id].name])

proc declareNimType(m: BModule, name: string; str: Rope, module: int) =
  m.s[cfsData].addf("extern $2 $1;$n", [str, rope(name)])

proc genTypeInfo2Name(m: BModule; t: PType): Rope =
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
  result = makeCString(res)

proc isTrivialProc(g: ModuleGraph; s: PSym): bool {.inline.} = getBody(g, s).len == 0

proc genHook(m: BModule; t: PType; info: TLineInfo; op: TTypeAttachedOp): Rope =
  let theProc = getAttachedOp(m.g.graph, t, op)
  if theProc != nil and not isTrivialProc(m.g.graph, theProc):
    # the prototype of a destructor is ``=destroy(x: var T)`` and that of a
    # finalizer is: ``proc (x: ref T) {.nimcall.}``. We need to check the calling
    # convention at least:
    if theProc.typ == nil or theProc.typ.callConv != ccNimCall:
      localReport(m.config, info, reportSym(
        rsemExpectedNimcallProc, theProc))

    let id = useHook(m, theProc)
    result = m.procs[id].name

    when false:
      if not canFormAcycle(t) and op == attachedTrace:
        echo "ayclic but has this =trace ", t, " ", theProc.ast
  else:
    when false:
      if op == attachedTrace and m.config.selectedGC == gcOrc and
          containsGarbageCollectedRef(t):
        # unfortunately this check is wrong for an object type that only contains
        # .cursor fields like 'Node' inside 'cycleleak'.
        internalError(m.config, info, "no attached trace proc found")
    result = rope("NIM_NIL")

proc genTypeInfoV2Impl(m: BModule, t, origType: PType, name: Rope; info: TLineInfo) =
  var typeName: Rope
  if t.kind in {tyObject, tyDistinct}:
    if incompleteType(t):
      localReport(m.config, info, reportTyp(
        rsemRttiRequestForIncompleteObject, t))

    typeName = genTypeInfo2Name(m, t)
  else:
    typeName = rope("NIM_NIL")

  discard cgsym(m, "TNimTypeV2")
  m.s[cfsData].addf("N_LIB_PRIVATE TNimTypeV2 $1;$n", [name])
  let destroyImpl = genHook(m, t, info, attachedDestructor)
  let traceImpl = genHook(m, t, info, attachedTrace)

  var flags = 0
  if not isCyclePossible(t, m.g.graph): flags = flags or 1 # acyclic

  addf(m.s[cfsTypeInit3], "$1.destructor = (void*)$2; $1.size = sizeof($3); $1.align = NIM_ALIGNOF($3); $1.name = $4;$n; $1.traceImpl = (void*)$5; $1.flags = $6;", [
    name, destroyImpl, getTypeDesc(m, t), typeName,
    traceImpl, rope(flags)])

  if t.kind == tyObject and t.len > 0 and t[0] != nil and optEnableDeepCopy in m.config.globalOptions:
    discard genTypeInfoV1(m, t, info)

proc genTypeInfoV2(m: BModule, t: PType; info: TLineInfo): Rope =
  let origType = t
  # distinct types can have their own destructors
  var t = skipTypes(origType, irrelevantForBackend + tyUserTypeClasses - {tyDistinct})

  let prefixTI = "(&"

  let sig = hashType(origType)
  result = m.typeInfoMarkerV2.getOrDefault(sig)
  if result != "":
    return prefixTI.rope & result & ")".rope

  let marker = m.g.typeInfoMarkerV2.getOrDefault(sig)
  if marker.str != "":
    discard cgsym(m, "TNimTypeV2")
    declareNimType(m, "TNimTypeV2", marker.str, marker.owner)
    # also store in local type section:
    m.typeInfoMarkerV2[sig] = marker.str
    return prefixTI.rope & marker.str & ")".rope

  result = "NTIv2$1_" % [rope($sig)]
  m.typeInfoMarkerV2[sig] = result

  let owner = t.skipTypes(typedescPtrs).itemId.module
  if owner != m.module.position and moduleOpenForCodegen(m.g.graph, FileIndex owner):
    # make sure the type info is created in the owner module
    discard genTypeInfoV2(m.g.modules[owner], origType, info)
    # reference the type info as extern here
    discard cgsym(m, "TNimTypeV2")
    declareNimType(m, "TNimTypeV2", result, owner)
    return prefixTI.rope & result & ")".rope

  m.g.typeInfoMarkerV2[sig] = (str: result, owner: owner)
  genTypeInfoV2Impl(m, t, origType, result, info)
  result = prefixTI.rope & result & ")".rope

proc openArrayToTuple(m: BModule; t: PType): PType =
  result = newType(tyTuple, nextTypeId m.idgen, t.owner)
  let p = newType(tyPtr, nextTypeId m.idgen, t.owner)
  let a = newType(tyUncheckedArray, nextTypeId m.idgen, t.owner)
  a.add t.lastSon
  p.add a
  result.add p
  result.add getSysType(m.g.graph, t.owner.info, tyInt)

proc typeToC(t: PType): string =
  ## Just for more readable names, the result doesn't have
  ## to be unique.
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
      # We mangle upper letters and digits too so that there cannot
      # be clashes with our special meanings
      result.addInt ord(c)

proc genTypeInfoV1(m: BModule, t: PType; info: TLineInfo): Rope =
  let origType = t
  var t = skipTypes(origType, irrelevantForBackend + tyUserTypeClasses)

  let prefixTI = "(&"

  let sig = hashType(origType)
  result = m.typeInfoMarker.getOrDefault(sig)
  if result != "":
    return prefixTI.rope & result & ")".rope

  let marker = m.g.typeInfoMarker.getOrDefault(sig)
  if marker.str != "":
    discard cgsym(m, "TNimType")
    discard cgsym(m, "TNimNode")
    declareNimType(m, "TNimType", marker.str, marker.owner)
    # also store in local type section:
    m.typeInfoMarker[sig] = marker.str
    return prefixTI.rope & marker.str & ")".rope

  result = "NTI$1$2_" % [rope(typeToC(t)), rope($sig)]
  m.typeInfoMarker[sig] = result

  let old = m.g.graph.emittedTypeInfo.getOrDefault($result)
  if old != FileIndex(0):
    discard cgsym(m, "TNimType")
    discard cgsym(m, "TNimNode")
    declareNimType(m, "TNimType", result, old.int)
    return prefixTI.rope & result & ")".rope

  var owner = t.skipTypes(typedescPtrs).itemId.module
  if owner != m.module.position and moduleOpenForCodegen(m.g.graph, FileIndex owner):
    # make sure the type info is created in the owner module
    discard genTypeInfoV1(m.g.modules[owner], origType, info)
    # reference the type info as extern here
    discard cgsym(m, "TNimType")
    discard cgsym(m, "TNimNode")
    declareNimType(m, "TNimType", result, owner)
    return prefixTI.rope & result & ")".rope
  else:
    owner = m.module.position.int32

  m.g.typeInfoMarker[sig] = (str: result, owner: owner)
  rememberEmittedTypeInfo(m.g.graph, FileIndex(owner), $result)

  case t.kind
  of tyEmpty, tyVoid: result = rope"0"
  of tyPointer, tyBool, tyChar, tyCstring, tyString, tyInt..tyUInt64, tyVar, tyLent:
    genTypeInfoAuxBase(m, t, t, result, rope"0", info)
  of tyStatic:
    m.config.internalAssert(t.n != nil, "genTypeInfoV1(" & $t.kind & ')')
    result = genTypeInfoV1(m, lastSon t, info)
  of tyUserTypeClasses:
    m.config.internalAssert t.isResolvedUserTypeClass
    return genTypeInfoV1(m, t.lastSon, info)
  of tyProc:
    if t.callConv != ccClosure:
      genTypeInfoAuxBase(m, t, t, result, rope"0", info)
    else:
      let x = fakeClosureType(m, t.owner)
      genTupleInfo(m, x, x, result, info)
  of tySequence, tyRef:
    genTypeInfoAux(m, t, t, result, info)
  of tyPtr, tyRange, tyUncheckedArray: genTypeInfoAux(m, t, t, result, info)
  of tyArray: genArrayInfo(m, t, result, info)
  of tySet: genSetInfo(m, t, result, info)
  of tyEnum: genEnumInfo(m, t, result, info)
  of tyObject:
    genObjectInfo(m, t, origType, result, info)
  of tyTuple:
    # if t.n != nil: genObjectInfo(m, t, result)
    # else:
    # BUGFIX: use consistently RTTI without proper field names; otherwise
    # results are not deterministic!
    genTupleInfo(m, t, origType, result, info)
  of tyOpenArray:
    let x = openArrayToTuple(m, t)
    genTupleInfo(m, x, origType, result, info)
  else: internalError(m.config, "genTypeInfoV1(" & $t.kind & ')')

  var op = getAttachedOp(m.g.graph, t, attachedDeepCopy)
  if op == nil:
    op = getAttachedOp(m.g.graph, origType, attachedDeepCopy)
  if op != nil:
    genDeepCopyProc(m, op, result)

  if t.kind == tyObject and sfImportc notin t.sym.flags:
    let v2info = genTypeInfoV2(m, origType, info)
    addf(m.s[cfsTypeInit3], "$1->typeInfoV1 = (void*)&$2; $2.typeInfoV2 = (void*)$1;$n", [
      v2info, result])

  result = prefixTI.rope & result & ")".rope

proc genTypeInfo*(config: ConfigRef, m: BModule, t: PType; info: TLineInfo): Rope =
  result = genTypeInfoV2(m, t, info)
