#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# this module contains routines for accessing and iterating over types

import
  intsets, ast, astalgo, trees, msgs, strutils, platform, renderer, options,
  lineinfos, int128, modulegraphs, astmsgs

type
  TPreferedDesc* = enum
    preferName, # default
    preferDesc, # probably should become what preferResolved is
    preferExported,
    preferModuleInfo, # fully qualified
    preferGenericArg,
    preferTypeName,
    preferResolved, # fully resolved symbols
    preferMixed,
      # most useful, shows: symbol + resolved symbols if it differs, e.g.:
      # tuple[a: MyInt{int}, b: float]

  TTypeRelation* = enum      # order is important!
    isNone, isConvertible,
    isIntConv,
    isSubtype,
    isSubrange,              # subrange of the wanted type; no type conversion
                             # but apart from that counts as ``isSubtype``
    isBothMetaConvertible    # generic proc parameter was matched against
                             # generic type, e.g., map(mySeq, x=>x+1),
                             # maybe recoverable by rerun if the parameter is
                             # the proc's return value
    isInferred,              # generic proc was matched against a concrete type
    isInferredConvertible,   # same as above, but requiring proc CC conversion
    isGeneric,
    isFromIntLit,            # conversion *from* int literal; proven safe
    isEqual

  ProcConvMismatch* = enum
    pcmNoSideEffect
    pcmNotGcSafe
    pcmLockDifference
    pcmNotIterator
    pcmDifferentCallConv

proc typeToString*(typ: PType; prefer: TPreferedDesc = preferName): string

proc addTypeDeclVerboseMaybe*(result: var string, conf: ConfigRef; typ: PType) =
  if optDeclaredLocs in conf.globalOptions:
    result.add typeToString(typ, preferMixed)
    result.addDeclaredLoc(conf, typ)
  else:
    result.add typeToString(typ)

template `$`*(typ: PType): string = typeToString(typ)

proc base*(t: PType): PType =
  result = t[0]

# ------------------- type iterator: ----------------------------------------
type
  TTypeIter* = proc (t: PType, closure: RootRef): bool {.nimcall.} # true if iteration should stop
  TTypeMutator* = proc (t: PType, closure: RootRef): PType {.nimcall.} # copy t and mutate it
  TTypePredicate* = proc (t: PType): bool {.nimcall.}

proc iterOverType*(t: PType, iter: TTypeIter, closure: RootRef): bool
  # Returns result of `iter`.
proc mutateType*(t: PType, iter: TTypeMutator, closure: RootRef): PType
  # Returns result of `iter`.

type
  TParamsEquality* = enum     # they are equal, but their
                              # identifiers or their return
                              # type differ (i.e. they cannot be
                              # overloaded)
                              # this used to provide better error messages
    paramsNotEqual,           # parameters are not equal
    paramsEqual,              # parameters are equal
    paramsIncompatible

proc equalParams*(a, b: PNode): TParamsEquality
  # returns whether the parameter lists of the procs a, b are exactly the same

const
  # TODO: Remove tyTypeDesc from each abstractX and (where necessary)
  # replace with typedescX
  abstractPtrs* = {tyVar, tyPtr, tyRef, tyGenericInst, tyDistinct, tyOrdinal,
                   tyTypeDesc, tyAlias, tyInferred, tySink, tyLent, tyOwned}
  abstractVar* = {tyVar, tyGenericInst, tyDistinct, tyOrdinal, tyTypeDesc,
                  tyAlias, tyInferred, tySink, tyLent, tyOwned}
  abstractRange* = {tyGenericInst, tyRange, tyDistinct, tyOrdinal, tyTypeDesc,
                    tyAlias, tyInferred, tySink, tyOwned}
  abstractInstOwned* = abstractInst + {tyOwned}
  skipPtrs* = {tyVar, tyPtr, tyRef, tyGenericInst, tyTypeDesc, tyAlias,
               tyInferred, tySink, tyLent, tyOwned}
  # typedescX is used if we're sure tyTypeDesc should be included (or skipped)
  typedescPtrs* = abstractPtrs + {tyTypeDesc}
  typedescInst* = abstractInst + {tyTypeDesc, tyOwned, tyUserTypeClass}

proc invalidGenericInst*(f: PType): bool =
  result = f.kind == tyGenericInst and lastSon(f) == nil

proc isPureObject*(typ: PType): bool =
  var t = typ
  while t.kind == tyObject and t[0] != nil:
    t = t[0].skipTypes(skipPtrs)
  result = t.sym != nil and sfPure in t.sym.flags

proc isUnsigned*(t: PType): bool =
  t.skipTypes(abstractInst).kind in {tyChar, tyUInt..tyUInt64}

proc getOrdValue*(n: PNode; onError = high(Int128)): Int128 =
  var k = n.kind
  if n.typ != nil and n.typ.skipTypes(abstractInst).kind in {tyChar, tyUInt..tyUInt64}:
    k = nkUIntLit

  case k
  of nkCharLit, nkUIntLit..nkUInt64Lit:
    # XXX: enable this assert
    #assert n.typ == nil or isUnsigned(n.typ), $n.typ
    toInt128(cast[uint64](n.intVal))
  of nkIntLit..nkInt64Lit:
    # XXX: enable this assert
    #assert n.typ == nil or not isUnsigned(n.typ), $n.typ.kind
    toInt128(n.intVal)
  of nkNilLit:
    int128.Zero
  of nkHiddenStdConv: getOrdValue(n[1], onError)
  else:
    # XXX: The idea behind the introduction of int128 was to finally
    # have all calculations numerically far away from any
    # overflows. This command just introduces such overflows and
    # should therefore really be revisited.
    onError

proc getFloatValue*(n: PNode): BiggestFloat =
  case n.kind
  of nkFloatLiterals: n.floatVal
  of nkHiddenStdConv: getFloatValue(n[1])
  else: NaN

proc isIntLit*(t: PType): bool {.inline.} =
  result = t.kind == tyInt and t.n != nil and t.n.kind == nkIntLit

proc isFloatLit*(t: PType): bool {.inline.} =
  result = t.kind == tyFloat and t.n != nil and t.n.kind == nkFloatLit

proc addTypeHeader*(result: var string, conf: ConfigRef; typ: PType; prefer: TPreferedDesc = preferMixed; getDeclarationPath = true) =
  result.add typeToString(typ, prefer)
  if getDeclarationPath: result.addDeclaredLoc(conf, typ.sym)

proc getProcHeader*(conf: ConfigRef; sym: PSym; prefer: TPreferedDesc = preferName; getDeclarationPath = true): string =
  assert sym != nil
  # consider using `skipGenericOwner` to avoid fun2.fun2 when fun2 is generic
  result = sym.owner.name.s & '.' & sym.name.s
  if sym.kind in routineKinds:
    result.add '('
    var n = sym.typ.n
    for i in 1..<n.len:
      let p = n[i]
      if p.kind == nkSym:
        result.add(p.sym.name.s)
        result.add(": ")
        result.add(typeToString(p.sym.typ, prefer))
        if i != n.len-1: result.add(", ")
      else:
        result.add renderTree(p)
    result.add(')')
    if n[0].typ != nil:
      result.add(": " & typeToString(n[0].typ, prefer))
  if getDeclarationPath: result.addDeclaredLoc(conf, sym)

proc elemType*(t: PType): PType =
  assert(t != nil)
  case t.kind
  of tyGenericInst, tyDistinct, tyAlias, tySink: result = elemType(lastSon(t))
  of tyArray: result = t[1]
  of tyError: result = t
  else: result = t.lastSon
  assert(result != nil)

proc enumHasHoles*(t: PType): bool =
  var b = t.skipTypes({tyRange, tyGenericInst, tyAlias, tySink})
  result = b.kind == tyEnum and tfEnumHasHoles in b.flags

proc isOrdinalType*(t: PType, allowEnumWithHoles: bool = false): bool =
  assert(t != nil)
  const
    baseKinds = {tyChar, tyInt..tyInt64, tyUInt..tyUInt64, tyBool, tyEnum}
    parentKinds = {tyRange, tyOrdinal, tyGenericInst, tyAlias, tySink, tyDistinct}
  result = (t.kind in baseKinds and (not t.enumHasHoles or allowEnumWithHoles)) or
    (t.kind in parentKinds and isOrdinalType(t.lastSon, allowEnumWithHoles))

proc iterOverTypeAux(marker: var IntSet, t: PType, iter: TTypeIter,
                     closure: RootRef): bool
proc iterOverNode(marker: var IntSet, n: PNode, iter: TTypeIter,
                  closure: RootRef): bool =
  if n != nil:
    case n.kind
    of nkNone..nkNilLit:
      # a leaf
      result = iterOverTypeAux(marker, n.typ, iter, closure)
    else:
      for i in 0..<n.len:
        result = iterOverNode(marker, n[i], iter, closure)
        if result: return

proc iterOverTypeAux(marker: var IntSet, t: PType, iter: TTypeIter,
                     closure: RootRef): bool =
  result = false
  if t == nil: return
  result = iter(t, closure)
  if result: return
  if not containsOrIncl(marker, t.id):
    case t.kind
    of tyGenericInst, tyGenericBody, tyAlias, tySink, tyInferred:
      result = iterOverTypeAux(marker, lastSon(t), iter, closure)
    else:
      for i in 0..<t.len:
        result = iterOverTypeAux(marker, t[i], iter, closure)
        if result: return
      if t.n != nil and t.kind != tyProc: result = iterOverNode(marker, t.n, iter, closure)

proc iterOverType(t: PType, iter: TTypeIter, closure: RootRef): bool =
  var marker = initIntSet()
  result = iterOverTypeAux(marker, t, iter, closure)

proc searchTypeForAux(t: PType, predicate: TTypePredicate,
                      marker: var IntSet): bool

proc searchTypeNodeForAux(n: PNode, p: TTypePredicate,
                          marker: var IntSet): bool =
  result = false
  case n.kind
  of nkRecList:
    for i in 0..<n.len:
      result = searchTypeNodeForAux(n[i], p, marker)
      if result: return
  of nkRecCase:
    assert(n[0].kind == nkSym)
    result = searchTypeNodeForAux(n[0], p, marker)
    if result: return
    for i in 1..<n.len:
      case n[i].kind
      of nkOfBranch, nkElse:
        result = searchTypeNodeForAux(lastSon(n[i]), p, marker)
        if result: return
      else: discard
  of nkSym:
    result = searchTypeForAux(n.sym.typ, p, marker)
  else: discard

proc searchTypeForAux(t: PType, predicate: TTypePredicate,
                      marker: var IntSet): bool =
  # iterates over VALUE types!
  result = false
  if t == nil: return
  if containsOrIncl(marker, t.id): return
  result = predicate(t)
  if result: return
  case t.kind
  of tyObject:
    if t[0] != nil:
      result = searchTypeForAux(t[0].skipTypes(skipPtrs), predicate, marker)
    if not result: result = searchTypeNodeForAux(t.n, predicate, marker)
  of tyGenericInst, tyDistinct, tyAlias, tySink:
    result = searchTypeForAux(lastSon(t), predicate, marker)
  of tyArray, tySet, tyTuple:
    for i in 0..<t.len:
      result = searchTypeForAux(t[i], predicate, marker)
      if result: return
  else:
    discard

proc searchTypeFor*(t: PType, predicate: TTypePredicate): bool =
  var marker = initIntSet()
  result = searchTypeForAux(t, predicate, marker)

proc isObjectPredicate(t: PType): bool =
  result = t.kind == tyObject

proc containsObject*(t: PType): bool =
  result = searchTypeFor(t, isObjectPredicate)

proc isObjectWithTypeFieldPredicate(t: PType): bool =
  result = t.kind == tyObject and t[0] == nil and
      not (t.sym != nil and {sfPure, sfInfixCall} * t.sym.flags != {}) and
      tfFinal notin t.flags

type
  TTypeFieldResult* = enum
    frNone,                   # type has no object type field
    frHeader,                 # type has an object type field only in the header
    frEmbedded                # type has an object type field somewhere embedded

proc analyseObjectWithTypeFieldAux(t: PType,
                                   marker: var IntSet): TTypeFieldResult =
  var res: TTypeFieldResult
  result = frNone
  if t == nil: return
  case t.kind
  of tyObject:
    if t.n != nil:
      if searchTypeNodeForAux(t.n, isObjectWithTypeFieldPredicate, marker):
        return frEmbedded
    for i in 0..<t.len:
      var x = t[i]
      if x != nil: x = x.skipTypes(skipPtrs)
      res = analyseObjectWithTypeFieldAux(x, marker)
      if res == frEmbedded:
        return frEmbedded
      if res == frHeader: result = frHeader
    if result == frNone:
      if isObjectWithTypeFieldPredicate(t): result = frHeader
  of tyGenericInst, tyDistinct, tyAlias, tySink:
    result = analyseObjectWithTypeFieldAux(lastSon(t), marker)
  of tyArray, tyTuple:
    for i in 0..<t.len:
      res = analyseObjectWithTypeFieldAux(t[i], marker)
      if res != frNone:
        return frEmbedded
  else:
    discard

proc analyseObjectWithTypeField*(t: PType): TTypeFieldResult =
  # this does a complex analysis whether a call to ``objectInit`` needs to be
  # made or initializing of the type field suffices or if there is no type field
  # at all in this type.
  var marker = initIntSet()
  result = analyseObjectWithTypeFieldAux(t, marker)

proc isGCRef(t: PType): bool =
  result = t.kind in GcTypeKinds or
    (t.kind == tyProc and t.callConv == ccClosure)
  if result and t.kind in {tyString, tySequence} and tfHasAsgn in t.flags:
    result = false

proc containsGarbageCollectedRef*(typ: PType): bool =
  # returns true if typ contains a reference, sequence or string (all the
  # things that are garbage-collected)
  result = searchTypeFor(typ, isGCRef)

proc isManagedMemory(t: PType): bool =
  result = t.kind in GcTypeKinds or
    (t.kind == tyProc and t.callConv == ccClosure)

proc containsManagedMemory*(typ: PType): bool =
  result = searchTypeFor(typ, isManagedMemory)

proc isTyRef(t: PType): bool =
  result = t.kind == tyRef or (t.kind == tyProc and t.callConv == ccClosure)

proc containsTyRef*(typ: PType): bool =
  # returns true if typ contains a 'ref'
  result = searchTypeFor(typ, isTyRef)

proc isHiddenPointer(t: PType): bool =
  result = t.kind in {tyString, tySequence, tyOpenArray, tyVarargs}

proc containsHiddenPointer*(typ: PType): bool =
  # returns true if typ contains a string, table or sequence (all the things
  # that need to be copied deeply)
  result = searchTypeFor(typ, isHiddenPointer)

proc canFormAcycleAux(marker: var IntSet, typ: PType, startId: int): bool
proc canFormAcycleNode(marker: var IntSet, n: PNode, startId: int): bool =
  result = false
  if n != nil:
    result = canFormAcycleAux(marker, n.typ, startId)
    if not result:
      case n.kind
      of nkNone..nkNilLit:
        discard
      else:
        for i in 0..<n.len:
          result = canFormAcycleNode(marker, n[i], startId)
          if result: return

proc canFormAcycleAux(marker: var IntSet, typ: PType, startId: int): bool =
  result = false
  if typ == nil: return
  if tfAcyclic in typ.flags: return
  var t = skipTypes(typ, abstractInst+{tyOwned}-{tyTypeDesc})
  if tfAcyclic in t.flags: return
  case t.kind
  of tyTuple, tyObject, tyRef, tySequence, tyArray, tyOpenArray, tyVarargs:
    if t.id == startId:
      result = true
    elif not containsOrIncl(marker, t.id):
      for i in 0..<t.len:
        result = canFormAcycleAux(marker, t[i], startId)
        if result: return
      if t.n != nil: result = canFormAcycleNode(marker, t.n, startId)
    # Inheritance can introduce cyclic types, however this is not relevant
    # as the type that is passed to 'new' is statically known!
    # er but we use it also for the write barrier ...
    if t.kind == tyObject and tfFinal notin t.flags:
      # damn inheritance may introduce cycles:
      result = true
  of tyProc: result = typ.callConv == ccClosure
  else: discard

proc isFinal*(t: PType): bool =
  let t = t.skipTypes(abstractInst)
  result = t.kind != tyObject or tfFinal in t.flags or isPureObject(t)

proc canFormAcycle*(typ: PType): bool =
  var marker = initIntSet()
  let t = skipTypes(typ, abstractInst+{tyOwned}-{tyTypeDesc})
  result = canFormAcycleAux(marker, t, t.id)

proc mutateTypeAux(marker: var IntSet, t: PType, iter: TTypeMutator,
                   closure: RootRef): PType
proc mutateNode(marker: var IntSet, n: PNode, iter: TTypeMutator,
                closure: RootRef): PNode =
  result = nil
  if n != nil:
    result = copyNode(n)
    result.typ = mutateTypeAux(marker, n.typ, iter, closure)
    case n.kind
    of nkNone..nkNilLit:
      # a leaf
      discard
    else:
      for i in 0..<n.len:
        result.add mutateNode(marker, n[i], iter, closure)

proc mutateTypeAux(marker: var IntSet, t: PType, iter: TTypeMutator,
                   closure: RootRef): PType =
  result = nil
  if t == nil: return
  result = iter(t, closure)
  if not containsOrIncl(marker, t.id):
    for i in 0..<t.len:
      result[i] = mutateTypeAux(marker, result[i], iter, closure)
    if t.n != nil: result.n = mutateNode(marker, t.n, iter, closure)
  assert(result != nil)

proc mutateType(t: PType, iter: TTypeMutator, closure: RootRef): PType =
  var marker = initIntSet()
  result = mutateTypeAux(marker, t, iter, closure)

proc valueToString(a: PNode): string =
  case a.kind
  of nkCharLit..nkUInt64Lit: result = $a.intVal
  of nkFloatLit..nkFloat128Lit: result = $a.floatVal
  of nkStrLit..nkTripleStrLit: result = a.strVal
  else: result = "<invalid value>"

proc rangeToStr(n: PNode): string =
  assert(n.kind == nkRange)
  result = valueToString(n[0]) & ".." & valueToString(n[1])

const
  typeToStr: array[TTypeKind, string] = ["None", "bool", "char", "empty",
    "Alias", "typeof(nil)", "untyped", "typed", "typeDesc",
    # xxx typeDesc=>typedesc: typedesc is declared as such, and is 10x more common.
    "GenericInvocation", "GenericBody", "GenericInst", "GenericParam",
    "distinct $1", "enum", "ordinal[$1]", "array[$1, $2]", "object", "tuple",
    "set[$1]", "range[$1]", "ptr ", "ref ", "var ", "seq[$1]", "proc",
    "pointer", "OpenArray[$1]", "string", "cstring", "Forward",
    "int", "int8", "int16", "int32", "int64",
    "float", "float32", "float64", "float128",
    "uint", "uint8", "uint16", "uint32", "uint64",
    "owned", "sink",
    "lent ", "varargs[$1]", "UncheckedArray[$1]", "Error Type",
    "BuiltInTypeClass", "UserTypeClass",
    "UserTypeClassInst", "CompositeTypeClass", "inferred",
    "and", "or", "not", "any", "static", "TypeFromExpr", "concept", # xxx bugfix
    "void", "iterable"]

const preferToResolveSymbols = {preferName, preferTypeName, preferModuleInfo,
  preferGenericArg, preferResolved, preferMixed}

template bindConcreteTypeToUserTypeClass*(tc, concrete: PType) =
  tc.add concrete
  tc.flags.incl tfResolved

# TODO: It would be a good idea to kill the special state of a resolved
# concept by switching to tyAlias within the instantiated procs.
# Currently, tyAlias is always skipped with lastSon, which means that
# we can store information about the matched concept in another position.
# Then builtInFieldAccess can be modified to properly read the derived
# consts and types stored within the concept.
template isResolvedUserTypeClass*(t: PType): bool =
  tfResolved in t.flags

proc addTypeFlags(name: var string, typ: PType) {.inline.} =
  if tfNotNil in typ.flags: name.add(" not nil")

proc typeToString(typ: PType, prefer: TPreferedDesc = preferName): string =
  let preferToplevel = prefer
  proc getPrefer(prefer: TPreferedDesc): TPreferedDesc =
    if preferToplevel in {preferResolved, preferMixed}:
      preferToplevel # sticky option
    else:
      prefer

  proc typeToString(typ: PType, prefer: TPreferedDesc = preferName): string =
    result = ""
    let prefer = getPrefer(prefer)
    let t = typ
    if t == nil: return
    if prefer in preferToResolveSymbols and t.sym != nil and
         sfAnon notin t.sym.flags and t.kind != tySequence:
      if t.kind == tyInt and isIntLit(t):
        result = t.sym.name.s & " literal(" & $t.n.intVal & ")"
      elif t.kind == tyAlias and t[0].kind != tyAlias:
        result = typeToString(t[0])
      elif prefer in {preferResolved, preferMixed}:
        case t.kind
        of IntegralTypes + {tyFloat..tyFloat128} + {tyString, tyCstring}:
          result = typeToStr[t.kind]
        of tyGenericBody:
          result = typeToString(t.lastSon)
        of tyCompositeTypeClass:
          # avoids showing `A[any]` in `proc fun(a: A)` with `A = object[T]`
          result = typeToString(t.lastSon.lastSon)
        else:
          result = t.sym.name.s
        if prefer == preferMixed and result != t.sym.name.s:
          result = t.sym.name.s & "{" & result & "}"
      elif prefer in {preferName, preferTypeName} or t.sym.owner.isNil:
        # note: should probably be: {preferName, preferTypeName, preferGenericArg}
        result = t.sym.name.s
        if t.kind == tyGenericParam and t.len > 0:
          result.add ": "
          var first = true
          for son in t.sons:
            if not first: result.add " or "
            result.add son.typeToString
            first = false
      else:
        result = t.sym.owner.name.s & '.' & t.sym.name.s
      result.addTypeFlags(t)
      return
    case t.kind
    of tyInt:
      if not isIntLit(t) or prefer == preferExported:
        result = typeToStr[t.kind]
      else:
        if prefer == preferGenericArg:
          result = $t.n.intVal
        else:
          result = "int literal(" & $t.n.intVal & ")"
    of tyGenericInst, tyGenericInvocation:
      result = typeToString(t[0]) & '['
      for i in 1..<t.len-ord(t.kind != tyGenericInvocation):
        if i > 1: result.add(", ")
        result.add(typeToString(t[i], preferGenericArg))
      result.add(']')
    of tyGenericBody:
      result = typeToString(t.lastSon) & '['
      for i in 0..<t.len-1:
        if i > 0: result.add(", ")
        result.add(typeToString(t[i], preferTypeName))
      result.add(']')
    of tyTypeDesc:
      if t[0].kind == tyNone: result = "typedesc"
      else: result = "typedesc[" & typeToString(t[0]) & "]"
    of tyStatic:
      if prefer == preferGenericArg and t.n != nil:
        result = t.n.renderTree
      else:
        result = "static[" & (if t.len > 0: typeToString(t[0]) else: "") & "]"
        if t.n != nil: result.add "(" & renderTree(t.n) & ")"
    of tyUserTypeClass:
      if t.sym != nil and t.sym.owner != nil:
        if t.isResolvedUserTypeClass: return typeToString(t.lastSon)
        return t.sym.owner.name.s
      else:
        result = "<invalid tyUserTypeClass>"
    of tyBuiltInTypeClass:
      result = case t.base.kind
        of tyVar: "var"
        of tyRef: "ref"
        of tyPtr: "ptr"
        of tySequence: "seq"
        of tyArray: "array"
        of tySet: "set"
        of tyRange: "range"
        of tyDistinct: "distinct"
        of tyProc: "proc"
        of tyObject: "object"
        of tyTuple: "tuple"
        of tyOpenArray: "openArray"
        else: typeToStr[t.base.kind]
    of tyInferred:
      let concrete = t.previouslyInferred
      if concrete != nil: result = typeToString(concrete)
      else: result = "inferred[" & typeToString(t.base) & "]"
    of tyUserTypeClassInst:
      let body = t.base
      result = body.sym.name.s & "["
      for i in 1..<t.len - 1:
        if i > 1: result.add(", ")
        result.add(typeToString(t[i]))
      result.add "]"
    of tyAnd:
      for i, son in t.sons:
        result.add(typeToString(son))
        if i < t.sons.high:
          result.add(" and ")
    of tyOr:
      for i, son in t.sons:
        result.add(typeToString(son))
        if i < t.sons.high:
          result.add(" or ")
    of tyNot:
      result = "not " & typeToString(t[0])
    of tyUntyped:
      #internalAssert t.len == 0
      result = "untyped"
    of tyFromExpr:
      if t.n == nil:
        result = "unknown"
      else:
        result = "typeof(" & renderTree(t.n) & ")"
    of tyArray:
      result = "array"
      if t.len > 0:
        if t[0].kind == tyRange:
          result &= "[" & rangeToStr(t[0].n) & ", " &
              typeToString(t[1]) & ']'
        else:
          result &= "[" & typeToString(t[0]) & ", " &
              typeToString(t[1]) & ']'
    of tyUncheckedArray:
      result = "UncheckedArray"
      if t.len > 0:
        result &= "[" & typeToString(t[0]) & ']'
    of tySequence:
      if t.sym != nil and prefer != preferResolved:
        result = t.sym.name.s
      else:
        result = "seq"
        if t.len > 0:
          result &= "[" & typeToString(t[0]) & ']'
    of tyOrdinal:
      result = "ordinal"
      if t.len > 0:
        result &= "[" & typeToString(t[0]) & ']'
    of tySet:
      result = "set"
      if t.len > 0:
        result &= "[" & typeToString(t[0]) & ']'
    of tyOpenArray:
      result = "openArray"
      if t.len > 0:
        result &= "[" & typeToString(t[0]) & ']'
    of tyDistinct:
      result = "distinct " & typeToString(t[0],
        if prefer == preferModuleInfo: preferModuleInfo else: preferTypeName)
    of tyIterable:
      # xxx factor this pattern
      result = "iterable"
      if t.len > 0:
        result &= "[" & typeToString(t[0]) & ']'
    of tyTuple:
      # we iterate over t.sons here, because t.n may be nil
      if t.n != nil:
        result = "tuple["
        assert(t.n.len == t.len)
        for i in 0..<t.n.len:
          assert(t.n[i].kind == nkSym)
          result.add(t.n[i].sym.name.s & ": " & typeToString(t[i]))
          if i < t.n.len - 1: result.add(", ")
        result.add(']')
      elif t.len == 0:
        result = "tuple[]"
      else:
        result = "("
        for i in 0..<t.len:
          result.add(typeToString(t[i]))
          if i < t.len - 1: result.add(", ")
          elif t.len == 1: result.add(",")
        result.add(')')
    of tyPtr, tyRef, tyVar, tyLent:
      result = typeToStr[t.kind]
      if t.len >= 2:
        setLen(result, result.len-1)
        result.add '['
        for i in 0..<t.len:
          result.add(typeToString(t[i]))
          if i < t.len - 1: result.add(", ")
        result.add ']'
      else:
        result.add typeToString(t[0])
    of tyRange:
      result = "range "
      if t.n != nil and t.n.kind == nkRange:
        result.add rangeToStr(t.n)
      if prefer != preferExported:
        result.add("(" & typeToString(t[0]) & ")")
    of tyProc:
      result = if tfIterator in t.flags: "iterator "
               elif t.owner != nil:
                 case t.owner.kind
                 of skTemplate: "template "
                 of skMacro: "macro "
                 of skConverter: "converter "
                 else: "proc "
              else:
                "proc "
      if tfUnresolved in t.flags: result.add "[*missing parameters*]"
      result.add "("
      for i in 1..<t.len:
        if t.n != nil and i < t.n.len and t.n[i].kind == nkSym:
          result.add(t.n[i].sym.name.s)
          result.add(": ")
        result.add(typeToString(t[i]))
        if i < t.len - 1: result.add(", ")
      result.add(')')
      if t.len > 0 and t[0] != nil: result.add(": " & typeToString(t[0]))
      var prag = if t.callConv == ccNimCall and tfExplicitCallConv notin t.flags: "" else: $t.callConv
      if tfNoSideEffect in t.flags:
        addSep(prag)
        prag.add("noSideEffect")
      if tfThread in t.flags:
        addSep(prag)
        prag.add("gcsafe")
      if t.lockLevel.ord != UnspecifiedLockLevel.ord:
        addSep(prag)
        prag.add("locks: " & $t.lockLevel)
      if prag.len != 0: result.add("{." & prag & ".}")
    of tyVarargs:
      result = typeToStr[t.kind] % typeToString(t[0])
    of tySink:
      result = "sink " & typeToString(t[0])
    of tyOwned:
      result = "owned " & typeToString(t[0])
    else:
      result = typeToStr[t.kind]
    result.addTypeFlags(t)
  result = typeToString(typ, prefer)

proc firstOrd*(conf: ConfigRef; t: PType): Int128 =
  case t.kind
  of tyBool, tyChar, tySequence, tyOpenArray, tyString, tyVarargs, tyProxy:
    result = Zero
  of tySet, tyVar: result = firstOrd(conf, t[0])
  of tyArray: result = firstOrd(conf, t[0])
  of tyRange:
    assert(t.n != nil)        # range directly given:
    assert(t.n.kind == nkRange)
    result = getOrdValue(t.n[0])
  of tyInt:
    if conf != nil and conf.target.intSize == 4:
      result = toInt128(-2147483648)
    else:
      result = toInt128(0x8000000000000000'i64)
  of tyInt8: result =  toInt128(-128)
  of tyInt16: result = toInt128(-32768)
  of tyInt32: result = toInt128(-2147483648)
  of tyInt64: result = toInt128(0x8000000000000000'i64)
  of tyUInt..tyUInt64: result = Zero
  of tyEnum:
    # if basetype <> nil then return firstOrd of basetype
    if t.len > 0 and t[0] != nil:
      result = firstOrd(conf, t[0])
    else:
      if t.n.len > 0:
        assert(t.n[0].kind == nkSym)
        result = toInt128(t.n[0].sym.position)
  of tyGenericInst, tyDistinct, tyTypeDesc, tyAlias, tySink,
     tyStatic, tyInferred, tyUserTypeClasses, tyLent:
    result = firstOrd(conf, lastSon(t))
  of tyOrdinal:
    if t.len > 0: result = firstOrd(conf, lastSon(t))
    else: internalError(conf, "invalid kind for firstOrd(" & $t.kind & ')')
  of tyUncheckedArray, tyCstring:
    result = Zero
  else:
    internalError(conf, "invalid kind for firstOrd(" & $t.kind & ')')
    result = Zero

proc firstFloat*(t: PType): BiggestFloat =
  case t.kind
  of tyFloat..tyFloat128: -Inf
  of tyRange:
    assert(t.n != nil)        # range directly given:
    assert(t.n.kind == nkRange)
    getFloatValue(t.n[0])
  of tyVar: firstFloat(t[0])
  of tyGenericInst, tyDistinct, tyTypeDesc, tyAlias, tySink,
     tyStatic, tyInferred, tyUserTypeClasses:
    firstFloat(lastSon(t))
  else:
    internalError(newPartialConfigRef(), "invalid kind for firstFloat(" & $t.kind & ')')
    NaN

proc lastOrd*(conf: ConfigRef; t: PType): Int128 =
  case t.kind
  of tyBool: result = toInt128(1'u)
  of tyChar: result = toInt128(255'u)
  of tySet, tyVar: result = lastOrd(conf, t[0])
  of tyArray: result = lastOrd(conf, t[0])
  of tyRange:
    assert(t.n != nil)        # range directly given:
    assert(t.n.kind == nkRange)
    result = getOrdValue(t.n[1])
  of tyInt:
    if conf != nil and conf.target.intSize == 4: result = toInt128(0x7FFFFFFF)
    else: result = toInt128(0x7FFFFFFFFFFFFFFF'u64)
  of tyInt8: result = toInt128(0x0000007F)
  of tyInt16: result = toInt128(0x00007FFF)
  of tyInt32: result = toInt128(0x7FFFFFFF)
  of tyInt64: result = toInt128(0x7FFFFFFFFFFFFFFF'u64)
  of tyUInt:
    if conf != nil and conf.target.intSize == 4:
      result = toInt128(0xFFFFFFFF)
    else:
      result = toInt128(0xFFFFFFFFFFFFFFFF'u64)
  of tyUInt8: result = toInt128(0xFF)
  of tyUInt16: result = toInt128(0xFFFF)
  of tyUInt32: result = toInt128(0xFFFFFFFF)
  of tyUInt64:
    result = toInt128(0xFFFFFFFFFFFFFFFF'u64)
  of tyEnum:
    if t.n.len > 0:
      assert(t.n[^1].kind == nkSym)
      result = toInt128(t.n[^1].sym.position)
  of tyGenericInst, tyDistinct, tyTypeDesc, tyAlias, tySink,
     tyStatic, tyInferred, tyUserTypeClasses, tyLent:
    result = lastOrd(conf, lastSon(t))
  of tyProxy: result = Zero
  of tyOrdinal:
    if t.len > 0: result = lastOrd(conf, lastSon(t))
    else: internalError(conf, "invalid kind for lastOrd(" & $t.kind & ')')
  of tyUncheckedArray:
    result = Zero
  else:
    internalError(conf, "invalid kind for lastOrd(" & $t.kind & ')')
    result = Zero

proc lastFloat*(t: PType): BiggestFloat =
  case t.kind
  of tyFloat..tyFloat128: Inf
  of tyVar: lastFloat(t[0])
  of tyRange:
    assert(t.n != nil)        # range directly given:
    assert(t.n.kind == nkRange)
    getFloatValue(t.n[1])
  of tyGenericInst, tyDistinct, tyTypeDesc, tyAlias, tySink,
     tyStatic, tyInferred, tyUserTypeClasses:
    lastFloat(lastSon(t))
  else:
    internalError(newPartialConfigRef(), "invalid kind for lastFloat(" & $t.kind & ')')
    NaN

proc floatRangeCheck*(x: BiggestFloat, t: PType): bool =
  case t.kind
  # This needs to be special cased since NaN is never
  # part of firstFloat(t)..lastFloat(t)
  of tyFloat..tyFloat128:
    true
  of tyRange:
    x in firstFloat(t)..lastFloat(t)
  of tyVar:
    floatRangeCheck(x, t[0])
  of tyGenericInst, tyDistinct, tyTypeDesc, tyAlias, tySink,
     tyStatic, tyInferred, tyUserTypeClasses:
    floatRangeCheck(x, lastSon(t))
  else:
    internalError(newPartialConfigRef(), "invalid kind for floatRangeCheck:" & $t.kind)
    false

proc lengthOrd*(conf: ConfigRef; t: PType): Int128 =
  if t.skipTypes(tyUserTypeClasses).kind == tyDistinct:
    result = lengthOrd(conf, t[0])
  else:
    let last = lastOrd(conf, t)
    let first = firstOrd(conf, t)
    result = last - first + One

# -------------- type equality -----------------------------------------------

type
  TDistinctCompare* = enum ## how distinct types are to be compared
    dcEq,                  ## a and b should be the same type
    dcEqIgnoreDistinct,    ## compare symmetrically: (distinct a) == b, a == b
                           ## or a == (distinct b)
    dcEqOrDistinctOf       ## a equals b or a is distinct of b

  TTypeCmpFlag* = enum
    IgnoreTupleFields      ## NOTE: Only set this flag for backends!
    IgnoreCC
    ExactTypeDescValues
    ExactGenericParams
    ExactConstraints
    ExactGcSafety
    AllowCommonBase
    PickyCAliases  # be picky about the distinction between 'cint' and 'int32'

  TTypeCmpFlags* = set[TTypeCmpFlag]

  TSameTypeClosure = object
    cmp: TDistinctCompare
    recCheck: int
    flags: TTypeCmpFlags
    s: seq[tuple[a,b: int]] # seq for a set as it's hopefully faster
                            # (few elements expected)

proc initSameTypeClosure: TSameTypeClosure =
  # we do the initialization lazily for performance (avoids memory allocations)
  discard

proc containsOrIncl(c: var TSameTypeClosure, a, b: PType): bool =
  result = c.s.len > 0 and c.s.contains((a.id, b.id))
  if not result:
    c.s.add((a.id, b.id))

proc sameTypeAux(x, y: PType, c: var TSameTypeClosure): bool
proc sameTypeOrNilAux(a, b: PType, c: var TSameTypeClosure): bool =
  if a == b:
    result = true
  else:
    if a == nil or b == nil: result = false
    else: result = sameTypeAux(a, b, c)

proc sameType*(a, b: PType, flags: TTypeCmpFlags = {}): bool =
  var c = initSameTypeClosure()
  c.flags = flags
  result = sameTypeAux(a, b, c)

proc sameTypeOrNil*(a, b: PType, flags: TTypeCmpFlags = {}): bool =
  if a == b:
    result = true
  else:
    if a == nil or b == nil: result = false
    else: result = sameType(a, b, flags)

proc equalParam(a, b: PSym): TParamsEquality =
  if sameTypeOrNil(a.typ, b.typ, {ExactTypeDescValues}) and
      exprStructuralEquivalent(a.constraint, b.constraint):
    if a.ast == b.ast:
      result = paramsEqual
    elif a.ast != nil and b.ast != nil:
      if exprStructuralEquivalent(a.ast, b.ast): result = paramsEqual
      else: result = paramsIncompatible
    elif a.ast != nil:
      result = paramsEqual
    elif b.ast != nil:
      result = paramsIncompatible
  else:
    result = paramsNotEqual

proc sameConstraints(a, b: PNode): bool =
  if isNil(a) and isNil(b): return true
  if a.len != b.len: return false
  for i in 1..<a.len:
    if not exprStructuralEquivalent(a[i].sym.constraint,
                                    b[i].sym.constraint):
      return false
  return true

proc equalParams(a, b: PNode): TParamsEquality =
  result = paramsEqual
  if a.len != b.len:
    result = paramsNotEqual
  else:
    for i in 1..<a.len:
      var m = a[i].sym
      var n = b[i].sym
      assert((m.kind == skParam) and (n.kind == skParam))
      case equalParam(m, n)
      of paramsNotEqual:
        return paramsNotEqual
      of paramsEqual:
        discard
      of paramsIncompatible:
        result = paramsIncompatible
      if m.name.id != n.name.id:
        # BUGFIX
        return paramsNotEqual # paramsIncompatible;
      # continue traversal! If not equal, we can return immediately; else
      # it stays incompatible
    if not sameTypeOrNil(a.typ, b.typ, {ExactTypeDescValues}):
      if (a.typ == nil) or (b.typ == nil):
        result = paramsNotEqual # one proc has a result, the other not is OK
      else:
        result = paramsIncompatible # overloading by different
                                    # result types does not work

proc sameTuple(a, b: PType, c: var TSameTypeClosure): bool =
  # two tuples are equivalent iff the names, types and positions are the same;
  # however, both types may not have any field names (t.n may be nil) which
  # complicates the matter a bit.
  if a.len == b.len:
    result = true
    for i in 0..<a.len:
      var x = a[i]
      var y = b[i]
      if IgnoreTupleFields in c.flags:
        x = skipTypes(x, {tyRange, tyGenericInst, tyAlias})
        y = skipTypes(y, {tyRange, tyGenericInst, tyAlias})

      result = sameTypeAux(x, y, c)
      if not result: return
    if a.n != nil and b.n != nil and IgnoreTupleFields notin c.flags:
      for i in 0..<a.n.len:
        # check field names:
        if a.n[i].kind == nkSym and b.n[i].kind == nkSym:
          var x = a.n[i].sym
          var y = b.n[i].sym
          result = x.name.id == y.name.id
          if not result: break
        else:
          return false
    elif a.n != b.n and (a.n == nil or b.n == nil) and IgnoreTupleFields notin c.flags:
      result = false

template ifFastObjectTypeCheckFailed(a, b: PType, body: untyped) =
  if tfFromGeneric notin a.flags + b.flags:
    # fast case: id comparison suffices:
    result = a.id == b.id
  else:
    # expensive structural equality test; however due to the way generic and
    # objects work, if one of the types does **not** contain tfFromGeneric,
    # they cannot be equal. The check ``a.sym.id == b.sym.id`` checks
    # for the same origin and is essential because we don't want "pure"
    # structural type equivalence:
    #
    # type
    #   TA[T] = object
    #   TB[T] = object
    # --> TA[int] != TB[int]
    if tfFromGeneric in a.flags * b.flags and a.sym.id == b.sym.id:
      # ok, we need the expensive structural check
      body

proc sameObjectTypes*(a, b: PType): bool =
  # specialized for efficiency (sigmatch uses it)
  ifFastObjectTypeCheckFailed(a, b):
    var c = initSameTypeClosure()
    result = sameTypeAux(a, b, c)

proc sameDistinctTypes*(a, b: PType): bool {.inline.} =
  result = sameObjectTypes(a, b)

proc sameEnumTypes*(a, b: PType): bool {.inline.} =
  result = a.id == b.id

proc sameObjectTree(a, b: PNode, c: var TSameTypeClosure): bool =
  if a == b:
    result = true
  elif a != nil and b != nil and a.kind == b.kind:
    var x = a.typ
    var y = b.typ
    if IgnoreTupleFields in c.flags:
      if x != nil: x = skipTypes(x, {tyRange, tyGenericInst, tyAlias})
      if y != nil: y = skipTypes(y, {tyRange, tyGenericInst, tyAlias})
    if sameTypeOrNilAux(x, y, c):
      case a.kind
      of nkSym:
        # same symbol as string is enough:
        result = a.sym.name.id == b.sym.name.id
      of nkIdent: result = a.ident.id == b.ident.id
      of nkCharLit..nkInt64Lit: result = a.intVal == b.intVal
      of nkFloatLit..nkFloat64Lit: result = a.floatVal == b.floatVal
      of nkStrLit..nkTripleStrLit: result = a.strVal == b.strVal
      of nkEmpty, nkNilLit, nkType: result = true
      else:
        if a.len == b.len:
          for i in 0..<a.len:
            if not sameObjectTree(a[i], b[i], c): return
          result = true

proc sameObjectStructures(a, b: PType, c: var TSameTypeClosure): bool =
  # check base types:
  if a.len != b.len: return
  for i in 0..<a.len:
    if not sameTypeOrNilAux(a[i], b[i], c): return
  if not sameObjectTree(a.n, b.n, c): return
  result = true

proc sameChildrenAux(a, b: PType, c: var TSameTypeClosure): bool =
  if a.len != b.len: return false
  result = true
  for i in 0..<a.len:
    result = sameTypeOrNilAux(a[i], b[i], c)
    if not result: return

proc isGenericAlias*(t: PType): bool =
  return t.kind == tyGenericInst and t.lastSon.kind == tyGenericInst

proc skipGenericAlias*(t: PType): PType =
  return if t.isGenericAlias: t.lastSon else: t

proc sameFlags*(a, b: PType): bool {.inline.} =
  result = eqTypeFlags*a.flags == eqTypeFlags*b.flags

proc sameTypeAux(x, y: PType, c: var TSameTypeClosure): bool =
  template cycleCheck() =
    # believe it or not, the direct check for ``containsOrIncl(c, a, b)``
    # increases bootstrapping time from 2.4s to 3.3s on my laptop! So we cheat
    # again: Since the recursion check is only to not get caught in an endless
    # recursion, we use a counter and only if it's value is over some
    # threshold we perform the expensive exact cycle check:
    if c.recCheck < 3:
      inc c.recCheck
    else:
      if containsOrIncl(c, a, b): return true

  if x == y: return true
  var a = skipTypes(x, {tyGenericInst, tyAlias})
  while a.kind == tyUserTypeClass and tfResolved in a.flags:
    a = skipTypes(a[^1], {tyGenericInst, tyAlias})
  var b = skipTypes(y, {tyGenericInst, tyAlias})
  while b.kind == tyUserTypeClass and tfResolved in b.flags:
    b = skipTypes(b[^1], {tyGenericInst, tyAlias})
  assert(a != nil)
  assert(b != nil)
  if a.kind != b.kind:
    case c.cmp
    of dcEq: return false
    of dcEqIgnoreDistinct:
      a = a.skipTypes({tyDistinct, tyGenericInst})
      b = b.skipTypes({tyDistinct, tyGenericInst})
      if a.kind != b.kind: return false
    of dcEqOrDistinctOf:
      a = a.skipTypes({tyDistinct, tyGenericInst})
      if a.kind != b.kind: return false

  # this is required by tunique_type but makes no sense really:
  if tyDistinct notin {x.kind, y.kind} and x.kind == tyGenericInst and IgnoreTupleFields notin c.flags:
    let
      lhs = x.skipGenericAlias
      rhs = y.skipGenericAlias
    if rhs.kind != tyGenericInst or lhs.base != rhs.base:
      return false
    for i in 1..<lhs.len - 1:
      let ff = rhs[i]
      let aa = lhs[i]
      if not sameTypeAux(ff, aa, c): return false
    return true

  case a.kind
  of tyEmpty, tyChar, tyBool, tyNil, tyPointer, tyString, tyCstring,
     tyInt..tyUInt64, tyTyped, tyUntyped, tyVoid:
    result = sameFlags(a, b)
    if result and PickyCAliases in c.flags:
      # additional requirement for the caching of generics for importc'ed types:
      # the symbols must be identical too:
      let symFlagsA = if a.sym != nil: a.sym.flags else: {}
      let symFlagsB = if b.sym != nil: b.sym.flags else: {}
      if (symFlagsA+symFlagsB) * {sfImportc, sfExportc} != {}:
        result = symFlagsA == symFlagsB
  of tyStatic, tyFromExpr:
    result = exprStructuralEquivalent(a.n, b.n) and sameFlags(a, b)
    if result and a.len == b.len and a.len == 1:
      cycleCheck()
      result = sameTypeAux(a[0], b[0], c)
  of tyObject:
    ifFastObjectTypeCheckFailed(a, b):
      cycleCheck()
      result = sameObjectStructures(a, b, c) and sameFlags(a, b)
  of tyDistinct:
    cycleCheck()
    if c.cmp == dcEq:
      if sameFlags(a, b):
        ifFastObjectTypeCheckFailed(a, b):
          result = sameTypeAux(a[0], b[0], c)
    else:
      result = sameTypeAux(a[0], b[0], c) and sameFlags(a, b)
  of tyEnum, tyForward:
    # XXX generic enums do not make much sense, but require structural checking
    result = a.id == b.id and sameFlags(a, b)
  of tyError:
    result = b.kind == tyError
  of tyTuple:
    cycleCheck()
    result = sameTuple(a, b, c) and sameFlags(a, b)
  of tyTypeDesc:
    if c.cmp == dcEqIgnoreDistinct: result = false
    elif ExactTypeDescValues in c.flags:
      cycleCheck()
      result = sameChildrenAux(x, y, c) and sameFlags(a, b)
    else:
      result = sameFlags(a, b)
  of tyGenericParam:
    result = sameChildrenAux(a, b, c) and sameFlags(a, b)
    if result and {ExactGenericParams, ExactTypeDescValues} * c.flags != {}:
      result = a.sym.position == b.sym.position
  of tyBuiltInTypeClass:
    assert a.len == 1
    assert a[0].len == 0
    assert b.len == 1
    assert b[0].len == 0
    result = a[0].kind == b[0].kind
  of tyGenericInvocation, tyGenericBody, tySequence, tyOpenArray, tySet, tyRef,
     tyPtr, tyVar, tyLent, tySink, tyUncheckedArray, tyArray, tyProc, tyVarargs,
     tyOrdinal, tyCompositeTypeClass, tyUserTypeClass, tyUserTypeClassInst,
     tyAnd, tyOr, tyNot, tyAnything, tyOwned:
    cycleCheck()
    if a.kind == tyUserTypeClass and a.n != nil: return a.n == b.n
    result = sameChildrenAux(a, b, c)
    if result:
      if IgnoreTupleFields in c.flags:
        result = a.flags * {tfVarIsPtr} == b.flags * {tfVarIsPtr}
      else:
        result = sameFlags(a, b)
    if result and ExactGcSafety in c.flags:
      result = a.flags * {tfThread} == b.flags * {tfThread}
    if result and a.kind == tyProc:
      result = ((IgnoreCC in c.flags) or a.callConv == b.callConv) and
               ((ExactConstraints notin c.flags) or sameConstraints(a.n, b.n))
  of tyRange:
    cycleCheck()
    result = sameTypeOrNilAux(a[0], b[0], c) and
        sameValue(a.n[0], b.n[0]) and
        sameValue(a.n[1], b.n[1])
  of tyGenericInst, tyAlias, tyInferred, tyIterable:
    cycleCheck()
    result = sameTypeAux(a.lastSon, b.lastSon, c)
  of tyNone: result = false
  of tyConcept:
    result = exprStructuralEquivalent(a.n, b.n)

proc sameBackendType*(x, y: PType): bool =
  var c = initSameTypeClosure()
  c.flags.incl IgnoreTupleFields
  c.cmp = dcEqIgnoreDistinct
  result = sameTypeAux(x, y, c)

proc compareTypes*(x, y: PType,
                   cmp: TDistinctCompare = dcEq,
                   flags: TTypeCmpFlags = {}): bool =
  ## compares two type for equality (modulo type distinction)
  var c = initSameTypeClosure()
  c.cmp = cmp
  c.flags = flags
  if x == y: result = true
  elif x.isNil or y.isNil: result = false
  else: result = sameTypeAux(x, y, c)

proc inheritanceDiff*(a, b: PType): int =
  # | returns: 0 iff `a` == `b`
  # | returns: -x iff `a` is the x'th direct superclass of `b`
  # | returns: +x iff `a` is the x'th direct subclass of `b`
  # | returns: `maxint` iff `a` and `b` are not compatible at all
  if a == b or a.kind == tyError or b.kind == tyError: return 0
  assert a.kind in {tyObject} + skipPtrs
  assert b.kind in {tyObject} + skipPtrs
  var x = a
  result = 0
  while x != nil:
    x = skipTypes(x, skipPtrs)
    if sameObjectTypes(x, b): return
    x = x[0]
    dec(result)
  var y = b
  result = 0
  while y != nil:
    y = skipTypes(y, skipPtrs)
    if sameObjectTypes(y, a): return
    y = y[0]
    inc(result)
  result = high(int)

proc commonSuperclass*(a, b: PType): PType =
  # quick check: are they the same?
  if sameObjectTypes(a, b): return a

  # simple algorithm: we store all ancestors of 'a' in a ID-set and walk 'b'
  # up until the ID is found:
  assert a.kind == tyObject
  assert b.kind == tyObject
  var x = a
  var ancestors = initIntSet()
  while x != nil:
    x = skipTypes(x, skipPtrs)
    ancestors.incl(x.id)
    x = x[0]
  var y = b
  while y != nil:
    var t = y # bug #7818, save type before skip
    y = skipTypes(y, skipPtrs)
    if ancestors.contains(y.id):
      # bug #7818, defer the previous skipTypes
      if t.kind != tyGenericInst: t = y
      return t
    y = y[0]

proc matchType*(a: PType, pattern: openArray[tuple[k:TTypeKind, i:int]],
                last: TTypeKind): bool =
  var a = a
  for k, i in pattern.items:
    if a.kind != k: return false
    if i >= a.len or a[i] == nil: return false
    a = a[i]
  result = a.kind == last


include sizealignoffsetimpl

proc computeSize*(conf: ConfigRef; typ: PType): BiggestInt =
  computeSizeAlign(conf, typ)
  result = typ.size

proc getReturnType*(s: PSym): PType =
  # Obtains the return type of a iterator/proc/macro/template
  assert s.kind in skProcKinds
  result = s.typ[0]

proc getAlign*(conf: ConfigRef; typ: PType): BiggestInt =
  computeSizeAlign(conf, typ)
  result = typ.align

proc getSize*(conf: ConfigRef; typ: PType): BiggestInt =
  computeSizeAlign(conf, typ)
  result = typ.size

proc containsGenericTypeIter(t: PType, closure: RootRef): bool =
  case t.kind
  of tyStatic:
    return t.n == nil
  of tyTypeDesc:
    if t.base.kind == tyNone: return true
    if containsGenericTypeIter(t.base, closure): return true
    return false
  of GenericTypes + tyTypeClasses + {tyFromExpr}:
    return true
  else:
    return false

proc containsGenericType*(t: PType): bool =
  result = iterOverType(t, containsGenericTypeIter, nil)

proc baseOfDistinct*(t: PType; g: ModuleGraph; idgen: IdGenerator): PType =
  if t.kind == tyDistinct:
    result = t[0]
  else:
    result = copyType(t, nextTypeId idgen, t.owner)
    copyTypeProps(g, idgen.module, result, t)
    var parent: PType = nil
    var it = result
    while it.kind in {tyPtr, tyRef, tyOwned}:
      parent = it
      it = it.lastSon
    if it.kind == tyDistinct and parent != nil:
      parent[0] = it[0]

proc safeInheritanceDiff*(a, b: PType): int =
  # same as inheritanceDiff but checks for tyError:
  if a.kind == tyError or b.kind == tyError:
    result = -1
  else:
    result = inheritanceDiff(a.skipTypes(skipPtrs), b.skipTypes(skipPtrs))

proc compatibleEffectsAux(se, re: PNode): bool =
  if re.isNil: return false
  for r in items(re):
    block search:
      for s in items(se):
        if safeInheritanceDiff(r.typ, s.typ) <= 0:
          break search
      return false
  result = true

type
  EffectsCompat* = enum
    efCompat
    efRaisesDiffer
    efRaisesUnknown
    efTagsDiffer
    efTagsUnknown
    efLockLevelsDiffer
    efEffectsDelayed

proc compatibleEffects*(formal, actual: PType): EffectsCompat =
  # for proc type compatibility checking:
  assert formal.kind == tyProc and actual.kind == tyProc
  #if tfEffectSystemWorkaround in actual.flags:
  #  return efCompat

  if formal.n[0].kind != nkEffectList or
     actual.n[0].kind != nkEffectList:
    return efTagsUnknown

  var spec = formal.n[0]
  if spec.len != 0:
    var real = actual.n[0]

    let se = spec[exceptionEffects]
    # if 'se.kind == nkArgList' it is no formal type really, but a
    # computed effect and as such no spec:
    # 'r.msgHandler = if isNil(msgHandler): defaultMsgHandler else: msgHandler'
    if not isNil(se) and se.kind != nkArgList:
      # spec requires some exception or tag, but we don't know anything:
      if real.len == 0: return efRaisesUnknown
      let res = compatibleEffectsAux(se, real[exceptionEffects])
      if not res: return efRaisesDiffer

    let st = spec[tagEffects]
    if not isNil(st) and st.kind != nkArgList:
      # spec requires some exception or tag, but we don't know anything:
      if real.len == 0: return efTagsUnknown
      let res = compatibleEffectsAux(st, real[tagEffects])
      if not res:
        #if tfEffectSystemWorkaround notin actual.flags:
        return efTagsDiffer
  if formal.lockLevel.ord < 0 or
      actual.lockLevel.ord <= formal.lockLevel.ord:

    for i in 1 ..< min(formal.n.len, actual.n.len):
      if formal.n[i].sym.flags * {sfEffectsDelayed} != actual.n[i].sym.flags * {sfEffectsDelayed}:
        result = efEffectsDelayed
        break

    result = efCompat
  else:
    result = efLockLevelsDiffer

proc isCompileTimeOnly*(t: PType): bool {.inline.} =
  result = t.kind in {tyTypeDesc, tyStatic}

proc containsCompileTimeOnly*(t: PType): bool =
  if isCompileTimeOnly(t): return true
  for i in 0..<t.len:
    if t[i] != nil and isCompileTimeOnly(t[i]):
      return true
  return false

proc safeSkipTypes*(t: PType, kinds: TTypeKinds): PType =
  ## same as 'skipTypes' but with a simple cycle detector.
  result = t
  var seen = initIntSet()
  while result.kind in kinds and not containsOrIncl(seen, result.id):
    result = lastSon(result)

type
  OrdinalType* = enum
    NoneLike, IntLike, FloatLike

proc classify*(t: PType): OrdinalType =
  ## for convenient type checking:
  if t == nil:
    result = NoneLike
  else:
    case skipTypes(t, abstractVarRange).kind
    of tyFloat..tyFloat128: result = FloatLike
    of tyInt..tyInt64, tyUInt..tyUInt64, tyBool, tyChar, tyEnum:
      result = IntLike
    else: result = NoneLike

proc skipConv*(n: PNode): PNode =
  result = n
  case n.kind
  of nkObjUpConv, nkObjDownConv, nkChckRange, nkChckRangeF, nkChckRange64:
    # only skip the conversion if it doesn't lose too important information
    # (see bug #1334)
    if n[0].typ.classify == n.typ.classify:
      result = n[0]
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    if n[1].typ.classify == n.typ.classify:
      result = n[1]
  else: discard

proc skipHidden*(n: PNode): PNode =
  result = n
  while true:
    case result.kind
    of nkHiddenStdConv, nkHiddenSubConv:
      if result[1].typ.classify == result.typ.classify:
        result = result[1]
      else: break
    of nkHiddenDeref, nkHiddenAddr:
      result = result[0]
    else: break

proc skipConvTakeType*(n: PNode): PNode =
  result = n.skipConv
  result.typ = n.typ

proc isEmptyContainer*(t: PType): bool =
  case t.kind
  of tyUntyped, tyNil: result = true
  of tyArray: result = t[1].kind == tyEmpty
  of tySet, tySequence, tyOpenArray, tyVarargs:
    result = t[0].kind == tyEmpty
  of tyGenericInst, tyAlias, tySink: result = isEmptyContainer(t.lastSon)
  else: result = false

proc takeType*(formal, arg: PType; g: ModuleGraph; idgen: IdGenerator): PType =
  # param: openArray[string] = []
  # [] is an array constructor of length 0 of type string!
  if arg.kind == tyNil:
    # and not (formal.kind == tyProc and formal.callConv == ccClosure):
    result = formal
  elif formal.kind in {tyOpenArray, tyVarargs, tySequence} and
      arg.isEmptyContainer:
    let a = copyType(arg.skipTypes({tyGenericInst, tyAlias}), nextTypeId(idgen), arg.owner)
    copyTypeProps(g, idgen.module, a, arg)
    a[ord(arg.kind == tyArray)] = formal[0]
    result = a
  elif formal.kind in {tyTuple, tySet} and arg.kind == formal.kind:
    result = formal
  else:
    result = arg

proc skipHiddenSubConv*(n: PNode; g: ModuleGraph; idgen: IdGenerator): PNode =
  if n.kind == nkHiddenSubConv:
    # param: openArray[string] = []
    # [] is an array constructor of length 0 of type string!
    let formal = n.typ
    result = n[1]
    let arg = result.typ
    let dest = takeType(formal, arg, g, idgen)
    if dest == arg and formal.kind != tyUntyped:
      #echo n.info, " came here for ", formal.typeToString
      result = n
    else:
      result = copyTree(result)
      result.typ = dest
  else:
    result = n

proc getProcConvMismatch*(c: ConfigRef, f, a: PType, rel = isNone): (set[ProcConvMismatch], TTypeRelation) =
  ## Returns a set of the reason of mismatch, and the relation for conversion.
  result[1] = rel
  if tfNoSideEffect in f.flags and tfNoSideEffect notin a.flags:
    # Formal is pure, but actual is not
    result[0].incl pcmNoSideEffect
    result[1] = isNone

  if tfThread in f.flags and a.flags * {tfThread, tfNoSideEffect} == {} and
    optThreadAnalysis in c.globalOptions:
    # noSideEffect implies ``tfThread``!
    result[0].incl pcmNotGcSafe
    result[1] = isNone

  if f.flags * {tfIterator} != a.flags * {tfIterator}:
    # One of them is an iterator so not convertible
    result[0].incl pcmNotIterator
    result[1] = isNone

  if f.callConv != a.callConv:
    # valid to pass a 'nimcall' thingie to 'closure':
    if f.callConv == ccClosure and a.callConv == ccNimCall:
      case result[1]
      of isInferred: result[1] = isInferredConvertible
      of isBothMetaConvertible: result[1] = isBothMetaConvertible
      elif result[1] != isNone: result[1] = isConvertible
    else:
      result[1] = isNone
      result[0].incl pcmDifferentCallConv

  if f.lockLevel.ord != UnspecifiedLockLevel.ord and
     a.lockLevel.ord != UnspecifiedLockLevel.ord:
       # proctypeRel has more logic to catch this difference,
       # so dont need to do `rel = isNone`
       # but it's a pragma mismatch reason which is why it's here
       result[0].incl pcmLockDifference

proc addPragmaAndCallConvMismatch*(message: var string, formal, actual: PType, conf: ConfigRef) =
  assert formal.kind == tyProc and actual.kind == tyProc
  let (convMismatch, _) = getProcConvMismatch(conf, formal, actual)
  var
    gotPragmas = ""
    expectedPragmas = ""
  for reason in convMismatch:
    case reason
    of pcmDifferentCallConv:
      message.add "\n  Calling convention mismatch: got '{.$1.}', but expected '{.$2.}'." % [$actual.callConv, $formal.callConv]
    of pcmNoSideEffect:
      expectedPragmas.add "noSideEffect, "
    of pcmNotGcSafe:
      expectedPragmas.add "gcsafe, "
    of pcmLockDifference:
      gotPragmas.add("locks: " & $actual.lockLevel & ", ")
      expectedPragmas.add("locks: " & $formal.lockLevel & ", ")
    of pcmNotIterator: discard

  if expectedPragmas.len > 0:
    gotPragmas.setLen(max(0, gotPragmas.len - 2)) # Remove ", "
    expectedPragmas.setLen(max(0, expectedPragmas.len - 2)) # Remove ", "
    message.add "\n  Pragma mismatch: got '{.$1.}', but expected '{.$2.}'." % [gotPragmas, expectedPragmas]


proc typeMismatch*(conf: ConfigRef; info: TLineInfo, formal, actual: PType, n: PNode) =
  if formal.kind != tyError and actual.kind != tyError:
    let actualStr = typeToString(actual)
    let formalStr = typeToString(formal)
    let desc = typeToString(formal, preferDesc)
    let x = if formalStr == desc: formalStr else: formalStr & " = " & desc
    let verbose = actualStr == formalStr or optDeclaredLocs in conf.globalOptions
    var msg = "type mismatch:"
    if verbose: msg.add "\n"
    if conf.isDefined("nimLegacyTypeMismatch"):
      msg.add  " got <$1>" % actualStr
    else:
      msg.add  " got '$1' for '$2'" % [actualStr, n.renderTree]
    if verbose:
      msg.addDeclaredLoc(conf, actual)
      msg.add "\n"
    msg.add " but expected '$1'" % x
    if verbose: msg.addDeclaredLoc(conf, formal)

    if formal.kind == tyProc and actual.kind == tyProc:
      msg.addPragmaAndCallConvMismatch(formal, actual, conf)
      case compatibleEffects(formal, actual)
      of efCompat: discard
      of efRaisesDiffer:
        msg.add "\n.raise effects differ"
      of efRaisesUnknown:
        msg.add "\n.raise effect is 'can raise any'"
      of efTagsDiffer:
        msg.add "\n.tag effects differ"
      of efTagsUnknown:
        msg.add "\n.tag effect is 'any tag allowed'"
      of efLockLevelsDiffer:
        msg.add "\nlock levels differ"
      of efEffectsDelayed:
        msg.add "\n.effectsOf annotations differ"
    localError(conf, info, msg)

proc isTupleRecursive(t: PType, cycleDetector: var IntSet): bool =
  if t == nil:
    return false
  if cycleDetector.containsOrIncl(t.id):
    return true
  case t.kind
  of tyTuple:
    var cycleDetectorCopy: IntSet
    for i in 0..<t.len:
      assign(cycleDetectorCopy, cycleDetector)
      if isTupleRecursive(t[i], cycleDetectorCopy):
        return true
  of tyAlias, tyRef, tyPtr, tyGenericInst, tyVar, tyLent, tySink,
      tyArray, tyUncheckedArray, tySequence, tyDistinct:
    return isTupleRecursive(t.lastSon, cycleDetector)
  else:
    return false

proc isTupleRecursive*(t: PType): bool =
  var cycleDetector = initIntSet()
  isTupleRecursive(t, cycleDetector)

proc isException*(t: PType): bool =
  # check if `y` is object type and it inherits from Exception
  assert(t != nil)

  var t = t.skipTypes(abstractInst)
  while t.kind == tyObject:
    if t.sym != nil and t.sym.magic == mException: return true
    if t[0] == nil: break
    t = skipTypes(t[0], abstractPtrs)
  return false

proc isDefectException*(t: PType): bool =
  var t = t.skipTypes(abstractPtrs)
  while t.kind == tyObject:
    if t.sym != nil and t.sym.owner != nil and
        sfSystemModule in t.sym.owner.flags and
        t.sym.name.s == "Defect":
      return true
    if t[0] == nil: break
    t = skipTypes(t[0], abstractPtrs)
  return false

proc isSinkTypeForParam*(t: PType): bool =
  # a parameter like 'seq[owned T]' must not be used only once, but its
  # elements must, so we detect this case here:
  result = t.skipTypes({tyGenericInst, tyAlias}).kind in {tySink, tyOwned}
  when false:
    if isSinkType(t):
      if t.skipTypes({tyGenericInst, tyAlias}).kind in {tyArray, tyVarargs, tyOpenArray, tySequence}:
        result = false
      else:
        result = true

proc lookupFieldAgain*(ty: PType; field: PSym): PSym =
  var ty = ty
  while ty != nil:
    ty = ty.skipTypes(skipPtrs)
    assert(ty.kind in {tyTuple, tyObject})
    result = lookupInRecord(ty.n, field.name)
    if result != nil: break
    ty = ty[0]
  if result == nil: result = field

proc isCharArrayPtr*(t: PType; allowPointerToChar: bool): bool =
  let t = t.skipTypes(abstractInst)
  if t.kind == tyPtr:
    let pointsTo = t[0].skipTypes(abstractInst)
    case pointsTo.kind
    of tyUncheckedArray:
      result = pointsTo[0].kind == tyChar
    of tyArray:
      result = pointsTo[1].kind == tyChar and firstOrd(nil, pointsTo[0]) == 0 and
        skipTypes(pointsTo[0], {tyRange}).kind in {tyInt..tyInt64}
    of tyChar:
      result = allowPointerToChar
    else:
      discard
