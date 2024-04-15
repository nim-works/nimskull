## Implements a Table-like type for mapping a ``PType`` to some value. The
## ``PType`` keys undergo basic canoncalization, meaning that two
## different - in terms of reference equality - ``PType`` instances can
## represent the same key.
##
## The canonicalization makes sure to not ignore type information relevant to
## the mid-end and code generation stages.

import
  std/[
    hashes,
    tables
  ],
  compiler/ast/[
    ast_types,
    ast_query
  ],
  compiler/utils/[
    idioms
  ]

type
  Type = distinct PType
    # a distinct type so that a new hash and equality operator can be
    # attached
  TypeTable*[T] = object
    inner: Table[Type, T]

proc safeId(n: PNode): NodeId {.inline.} =
  if n != nil: n.id
  else:        NodeId -1

proc cmp(a, b: PType): bool {.noSideEffect.}

proc cmpElements(a, b: PType): bool =
  if a.len != b.len:
    return false

  for i in 0..<a.len:
    if not cmp(a[i], b[i]):
      return false

  result = true

proc cmpProc(a, b: PType): bool =
  if a.len != b.len:
    return false

  # nil in the return type slot means 'void'; handle it separately
  if a[0].isNil != b[0].isNil or (a[0] != nil and not cmp(a[0], b[0])):
    return false

  for i in 1..<a.len:
    if not cmp(a[i], b[i]):
      return false

  result = true

proc cmp(a, b: PNode): bool =
  if a.kind != b.kind:
    return false
  case a.kind
  of nkFloatLiterals:
    result = cast[BiggestInt](a.floatVal) == cast[BiggestInt](b.floatVal)
  of nkIntLiterals:
    result = a.intVal == b.intVal
  else:
    unreachable()

proc cmp(a, b: PType): bool =
  # generic types are also handled here, since they can be part of
  # ``tyTypeDesc``s
  if a.id == b.id: # quick check: same type instance?
    # FIXME: user type-classes are sometimes improperly instantiated,
    #        producing different types that share the same ID.
    #        ``concepts/tcomparable.nim`` is a test where removing the extra
    #        condition would cause a failure
    a.kind notin tyUserTypeClasses
  elif a.kind != b.kind or a.sym != b.sym:
    false
  elif a.sym != nil and a.kind == tyObject and a.sym == b.sym:
    # note: only object types have proper symbols at the moment. For all other
    # types, instantiations of a generic invocation all use the symbol of the
    # generic type
    true
  else:
    case a.kind
    of tyVoid, tyBool, tyChar, tyPointer, tyNil, tyInt..tyInt64,
       tyUInt..tyUInt64, tyFloat..tyFloat64, tyString, tyCstring, tyEmpty,
       tyAnything:
      true
    of tySet, tySequence, tyOpenArray, tyVarargs, tyUncheckedArray, tyPtr,
       tyRef, tyLent, tyVar, tyAlias, tyOrdinal, tySink, tyTypeDesc, tyNot:
      # simple structural-like types that are equal if their element types are
      cmp(a[^1], b[^1])
    of tyArray:
      cmp(a[0], b[0]) and cmp(a[1], b[1])
    of tyRange:
      # only the range description matters
      cmp(a.n[0], b.n[0]) and cmp(a.n[1], b.n[1])
    of tyTuple:
      # named and unnamed tuple distinction matters
      safeId(a.n) == safeId(b.n) and cmpElements(a, b)
    of tyProc:
      # also consider the flags
      a.flags == b.flags and a.callConv == b.callConv and cmpProc(a, b)
    of tyAnd, tyOr:
      cmp(a[0], b[1]) and cmp(a[1], b[1])
    of tyBuiltInTypeClass:
      a[0].kind == b[0].kind
    of tyObject, tyDistinct, tyEnum, tyGenericInst, tyStatic,
       tyUserTypeClasses, tyCompositeTypeClass, tyInferred:
      # ids are not the same, so it must be a different type
      # XXX: ideally, ``tyStatic`` would not be supported here, but ``mirgen``
      #      does add those types
      # FIXME: ``tyInferred`` reaches here, but shouldn't. Needs further
      #        investigation.
      false
    else:
      unreachable()

proc hash(n: PNode): Hash =
  case n.kind
  of nkFloatLiterals:
    hash(cast[BiggestInt](n.floatVal))
  of nkIntLiterals:
    hash(n.intVal)
  else:
    unreachable(n.kind)

proc hash(t: PType): Hash =
  # ``hash(a)`` must be ``== hash(b)`` if ``cmp(a, b)`` is true
  if t.sym != nil:
    # for types with symbols, only the symbol matters
    result = !$(hash(true) !& hash(t.sym.id))
  else:
    result = hash(false) !& hash(t.kind)
    case t.kind
    of tyVoid, tyBool, tyChar, tyPointer, tyNil, tyInt..tyInt64,
       tyUInt..tyUInt64, tyFloat..tyFloat64, tyString, tyCstring, tyEmpty:
      discard "leaf type"
    of tySet, tySequence, tyOpenArray, tyVarargs, tyUncheckedArray, tyPtr,
       tyRef, tyLent, tyVar, tyTypeDesc, tyAlias, tyOrdinal, tySink:
      result = result !& hash(t[0])
    of tyArray:
      result = result !& hash(t[0]) !& hash(t[1])
    of tyTuple:
      # only hash the number of elements
      result = result !& hash(t.len) !& hash(safeId t.n)
    of tyRange:
      result = result !& hash(t.n[0]) !& hash(t.n[1])
    of tyProc:
      # only hash the number of parameters
      result = result !& hash(t.flags) !& hash(t.callConv) !& hash(t.len)
    of tyAnd, tyOr:
      result = result !& hash(t[0]) !& hash(t[1])
    of tyBuiltInTypeClass:
      result = result !& hash(t[0].kind)
    of tyObject, tyDistinct, tyEnum, tyGenericInst, tyStatic,
       tyUserTypeClasses, tyCompositeTypeClass, tyInferred:
      result = result !& hash(t.id)
    else:
      unreachable()

    result = !$(result)

# note: hash and the equality procedure need to be be exported for symbol
# binding in ``Table`` routines to work properly

proc `==`*(a, b: Type): bool {.inline.} =
  ## Leaked implementation detail -- do not use.
  cmp(a.PType, b.PType)

proc hash*(x: Type): Hash {.inline.} =
  ## Leaked implementation detail -- do not use.
  hash(PType x)

proc `[]`*[T](t: TypeTable[T], key: PType): lent T {.inline.} =
  ## Looks up the item for `key`.
  t.inner[Type key]

proc `[]=`*[T](t: var TypeTable[T], key: PType, val: sink T) {.inline.} =
  t.inner[Type key] = val

proc mgetOrPut*[T](t: var TypeTable, key: PType, val: T): var T =
  ## If `key` has no mapping in `t`, adds one with `val` as the value first.
  t.inner.mgetOrPut(Type(key), val)
