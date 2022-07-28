## The definitions for the type representation used by the compiler back-end (mid-end?) IR.

type RecordNodeKind* = enum
  rnkEmpty # meant to be used by the garbage collector to fill cleaned slots
  rnkList
  rnkCase
  rnkBranch

type RecordNode* = object
  kind: RecordNodeKind
  len: uint32 ## the number of items
  a: uint32   ##
  b: uint32

type RecordId* = distinct uint32

type TypeId* = distinct uint32

type SymId* = distinct uint32

type TypeNodeKind = enum
  tnkEmpty

  tnkInt
  tnkFloat

  tnkRef
  tnkPtr
  tnkVar
  tnkLent

  tnkSeq
  tnkOpenArray
  tnkString
  #tnkSink # XXX: ?

  tnkRecord # tuples and objects
  tnkArray

  tnkProc

  tnkDistinct # XXX: not sure

  tnkImported # an imported type. Has one child node, used to derive the access semantics from
  tnkName # a reference to named type

  #tnkAlias

type FieldDesc* = object
  sym: SymId # may be empty
  typ: TypeId
  # XXX: bitsize should likely be stored as part of FieldDesc

type TypeNode* = object
  kind: TypeNodeKind
  a: uint32
  b: uint32

type TypeEnv* = object
  ## Holds the data for all types
  # XXX: in general, a `seq[seq]` could be used for `records`, `fields`, and
  #      `types`. This would make access a bit simpler; require less copying
  #      on resize; and make garbge collection easier. It would also increase
  #      memory fragmentation and reduce cache locality
  records: seq[RecordNode] ## the bodies for all record-like types (objects and tuples) in one contiguous seq
  fields: seq[FieldDesc] ## all fields
  types: seq[TypeNode] ## all types in one contiguous seq
  # XXX: maybe a redirection table for `tnkName` makes sense? Alternatively,
  #      indirections to another tnkName could be allowed

type TypeLookup* = object
  ## Data needed for mapping ``PType`` to the ``TypeId``


# XXX: copied from `ccgtypes`, might need some adjustments
const
  irrelevantForBackend = {tyGenericBody, tyGenericInst, tyGenericInvocation,
                          tyDistinct, tyRange, tyStatic, tyAlias, tySink,
                          tyInferred, tyOwned}

func skipTypesConsiderImported(t: PType, kinds: TTypeKinds): tuple[imported: bool, t: PType] =
  result.t = t
  while result.t.kind in kinds:
    result.imported = t.sym != nil and sfImportc in t.sym.flags
    if result.imported:
      return
    result.t = lastSon(result.t)

func addField(dest: var TypeEnv, s: PSym) =
  discard

func translate(dest: var TypeEnv, n: PNode): tuple[fields, entries: int] =
  func `+=`(a: var (int, int), b: (int, int)) {.inline.} =
    a[0] += b[0]
    a[1] += b[1]

  case n.kind
  of nkSym:
    addField(dest, n)
    result = (1, 0)
  of nkRecList:
    dest.records.add RecordNode(kind: rnkList)
    result.entries += 1

    for it in n.sons:
      result += translate(dest, it)

  of nkRecCase:
    dest.records.add RecordNode(kind: rnkCase)
    result.entries = n.len # the case entry and all branch entries

    addField(dest, n[0].sym) # discriminator

    result.entries = n.len

  else:
    unreachable(n.kind)


func translate(dest: var TypeEnv, t: PType) =
  let r = t.skipTypesConsiderImported(irrelevantForBackend)

  let t =
    if r.imported:
      dest.add TypeNode(kind: tnkImported)
      r.t.skipTypes(irrelevantForBackend)
    else:
      r.t.skipTypes()

  case t.kind
  of tyObject:
    dest.types.add TypeNode(kind: tnkRecord, a: RecordId(dest.records.len))
    translate(t.n)

  of tyObject

  of tyRef, tyPtr, tyVar, tyLent, tySequence, tyOpenArray:
    const Map = {tyRef: tnkRef, tyPtr: tnkPtr, tyVar: tnkVar, tyLent: tnkLent, tySequence: tnkSeq, tyOpenArray: tnkOpenArray}.toTable
    dest.types.add TypeNode(kind: Map[t.kind])
    translate(dest, t.lastSon)

  else:
    unreachable(t.kind)
