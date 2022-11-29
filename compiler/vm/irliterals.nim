## This module implements the storage and interacting with literal data in the
## context of the back-end

import
  std/[
    algorithm, # for ``sort``
    hashes,
    tables
  ],
  compiler/ast/[ast_types, ast],
  compiler/ic/[
    bitabs
  ]

from compiler/vm/vmdef import unreachable

type
  StringNode = distinct PNode

  LiteralKind* = enum
    # note: when adding new literal kinds, remember to adjust the parts
    #       depending on the number of bits a `LiteralKind` occupies
    lkComplex # a complex structure
    lkNumber # an int, uint, or float
    lkString
    lkPacked # a packed array

  LiteralId* = distinct uint32
    ## Names data in ``LiteralData``. A `LiteralId` is a
    ## ``(LiteralKind, name)`` pair compressed into a ``uint32`` - the kind is
    ## stored in the high bits and the name in the low bits.
    ##
    ## To allow for a value indicating 'none' or 'nil', the name is offset by
    ## '+1' when ``kind == low(LiteralKind)``.

  ConstDataNodeKind* = enum
    conRecord # a collection of (int32, node) pairs
    conConst ## a ``ConstId``
    conConstAddr ## a ``ConstId``
    conLit # a ``LiteralId``
    conImm # an immediate 24-bit signed integer value placed directly inside
           # the node
    # XXX: the only usage of `conExt` is for storing ``ProcId``s. Maybe rename
    #      it to `conProc`?
    conExt # an external ID
    conArray # an array of nodes
    #conPackedArray # a packed array of untyped integers

  ConstDataNode = object
    # TODO: rename
    # TODO: use a variant object, but make sure that the size stays the
    #       same (8 byte)
    kind*: ConstDataNodeKind
    data: uint32

  ChangeKind = enum
    ckRedirect
    ckReplace

  DChanges* = object
    list: seq[tuple[kind: ChangeKind, struct, pos, n: uint32]]
    temp: seq[ConstDataNode]

  DataIter* = object
    # TODO: rename to ``DataCursor``?
    start: int
    pos: int

    kind*: ConstDataNodeKind
    # XXX: `i` is neither used nor is it incremented properly
    i*, len*: uint32

    modified: bool # XXX: not necessary - ``list.len > 0`` already provides
                   #      this information
    # TODO: instead of requiring one to ``swap`` the changes in and out, see
    #       if the use of ``sink`` parameters could allow for an API with
    #       zero (deep) copies
    changes*: DChanges

  LiteralData* = object
    ## Stores all literal data managed by the back-end. Literal data is
    ## *immutable* - once added, it can't be modified again
    nodes: seq[ConstDataNode]
    packedArrays: seq[tuple[start, len, elemSize: uint32]]
    packed: seq[uint8] ## stores raw binary data

    numbers: BiTable[BiggestUInt] ## stores the raw bit-patterns of numbers
                                  ## (ints and floats)
    strings: BiTable[StringNode] ## stores all used strings. Strings reach the
      ## back-end wrapped in ``PNode``s, so to save memory, we keep them as such

    # XXX: using views (i.e. storing a ``var LiteralData`` in ``SessionBase``)
    #      instead of the run-time lock mechanism would be much nicer, as
    #      the implementation would be simpler and and the compiler could
    #      statically prevent multiple active sessions
    lock: int
    locks: int

    # XXX: rename to `staging`?
    temp: seq[ConstDataNode] ## used as a staging area during the creation
                             ## of nested structures

  SessionBase* {.inheritable, pure.} = object
    ## Adding something to ``LiteralData`` requires a session object.
    # XXX: the whole `SessionBase` API is a bit awkward - it likely needs some
    #      further tweaking
    prev, name: int
    id: LiteralId
    usesTemp: bool ## if the data is located in the staging area
    #target: var LiteralData

  AddRecordSession* = object of SessionBase
    num: int

  AddPackedSession*[T] = object of SessionBase
    p: ptr UncheckedArray[T]
    len: int

const
  idKindShift = 30 ## the bit position at where the 'kind' is located
  idNameMask = (1 shl idKindShift) - 1 ## the bit-mask of ID's content part

const NoneLit* = LiteralId(0)

func `==`*(a, b: LiteralId): bool {.borrow.}

func hash(n: StringNode): Hash {.inline.} =
  hash(PNode(n).strVal)

func `==`(a, b: StringNode): bool {.inline.} =
  PNode(a).strVal == PNode(b).strVal


template kind*(id: LiteralId): LiteralKind =
  LiteralKind(uint32(id) shr idKindShift)

template rawName(id: LiteralId): uint32 =
  ## Extracts the name part from the ID without accounting for the offset
  uint32(id) and idNameMask

template node(id: LiteralId): uint32 =
  ## For IDs referring to complex data, extracts the node index
  assert id.kind == lkComplex
  assert id != NoneLit
  # for complex data IDs, the name is offse by 1
  rawName(id) - 1

template number(id: LiteralId): LitId =
  assert id.kind == lkNumber
  rawName(id).LitId

template str(id: LiteralId): LitId =
  assert id.kind == lkString
  rawName(id).LitId

template arr(id: LiteralId): uint32 =
  assert id.kind == lkPacked
  rawName(id)

template toId(src: uint32, kind: LiteralKind): LiteralId =
  LiteralId((uint32(src) and idNameMask) or (uint32(kind) shl idKindShift))

template toId(src: LitId, kind: LiteralKind): LiteralId =
  toId(src.uint32, kind)

template toNode(node: SomeInteger): LiteralId =
  # offset the index by 1 so that the resulting ``LiteralId``'s value is
  # never 0
  toId(uint32(node) + 1, lkComplex)

# locks are meant as a way to catch bugs by validating that only the session
# that owns the lock is modifying the ``nodes`` and ``temp`` list

template isLocked(d: LiteralData): bool =
  d.lock != 0

proc testLock(d: LiteralData, s: SessionBase) =
  assert d.lock == s.name

proc lock(d: var LiteralData): int =
  result = d.locks + 1
  d.lock = result
  inc d.locks

proc switch(d: var LiteralData, name, newName: int) =
  assert d.lock == name
  d.lock = newName


proc llAdd(d: var LiteralData, s: SessionBase, n: ConstDataNode) =
  ## Appends `n` to the buffer the rest of the data for `s` is located in
  if s.usesTemp:
    d.temp.add n
  else:
    d.nodes.add n


func copyMem[T](dst: var openArray[T], src: openArray[T], dstP, srcP: Natural, len: Natural) =
  assert srcP + len <= src.len
  assert dstP + len <= dst.len
  copyMem(addr dst[dstP], unsafeAddr src[srcP], len * sizeof(T))

func copyMem[T](dst: var openArray[T], dstP, srcP: Natural, len: Natural) =
  ## Copies the memory from ``dst[srcP..<srcP+len]`` to
  ## ``dst[dstP..<dstP+len]``. The regions must not overlap.
  assert srcP + len <= dst.len
  assert dstP + len <= dst.len
  copyMem(addr dst[dstP], addr dst[srcP], len * sizeof(T))

func add*[T: SomeInteger](d: var LiteralData, x: openArray[T]): LiteralId =
  assert not d.isLocked
  let i = d.packed.len
  d.packed.setLen(d.packed.len + (x.len * sizeof(T)))

  # TODO: decide on whether or not packed data should care about endianess
  #       (would be required if ``LiteralData`` is to be written to disk)
  copyMem(addr d.packed[i], cast[ptr UncheckedArray[T]](x), sizeof(T) * x.len)

  let litId = toId(d.packedArrays.len.uint32, lkPacked)
  d.packedArrays.add (i.uint32, x.len.uint32, sizeof(T).uint32)

  result = toNode(d.nodes.len)
  d.nodes.add ConstDataNode(kind: conLit, data: litId.uint32)

func addPackedArray*[T](d: var LiteralData, len: Natural): AddPackedSession[T] =
  const elemSize = sizeof(T)
  let start = d.packed.len
  let litId = toId(d.packedArrays.len.uint32, lkPacked)
  # add an entry for the array:
  d.packedArrays.add (start.uint32, len.uint32, elemSize.uint32)

  # allocate the required amount of space:
  d.packed.setLen(start + (len * elemSize))

  result = AddPackedSession[T](id: toNode(d.nodes.len),
                               p: cast[ptr UncheckedArray[T]](addr d.packed[start]),
                               len: len)
  d.nodes.add ConstDataNode(kind: conLit, data: litId.uint32)


func addExt*(d: var LiteralData, s: SessionBase, id: uint32) =
  testLock(d, s)
  d.llAdd s, ConstDataNode(kind: conExt, data: id)

func addLit*(d: var LiteralData, s: SessionBase, id: LiteralId) =
  testLock(d, s)
  d.llAdd s, ConstDataNode(kind: conLit, data: id.uint32)

func startSession(d: var LiteralData, r: var SessionBase) =
  r.name = lock(d)
  r.id = toNode(d.nodes.len)

func begin*(d: var LiteralData): SessionBase =
  assert not isLocked(d)
  startSession(d, result)

func startArray*(d: var LiteralData, len: Natural): SessionBase =
  assert len <= high(uint32).int
  startSession(d, result)
  d.nodes.add ConstDataNode(kind: conArray, data: len.uint32)
  # the actual array data follows after this node

func addGet(a: var seq[ConstDataNode], b: openArray[ConstDataNode]): int =
  result = a.len
  a.setLen(result + b.len)
  if b.len > 0:
    copyMem(a, b, result, 0, b.len)

func migrate(d: var LiteralData, s: var SessionBase) =
  ## Moves the unfinished structure into the `d.temp` buffer if it's not
  ## located there already
  if not s.usesTemp:
    let start = s.id.node.int
    s.id = toNode addGet(d.temp, d.nodes.toOpenArray(start, d.nodes.high))
    s.usesTemp = true

    d.nodes.setLen(start)

func isNode*(id: LiteralId): bool {.deprecated: "use `id == lkComplex` instead".} =
  id.kind == lkComplex

func startArray*(d: var LiteralData, s: var SessionBase, len: Natural): SessionBase =
  testLock(d, s)
  migrate(d, s)
  result = startArray(d, len)
  result.prev = s.name

func startRecordI(d: var LiteralData) =
  d.nodes.add ConstDataNode(kind: conRecord)
  # the number of items is set when finishing the session

func startRecord*(d: var LiteralData): AddRecordSession =
  assert not d.isLocked
  startSession(d, result)
  startRecordI(d)

func add*(d: var LiteralData, s: var AddRecordSession, pos: int32) =
  testLock(d, s)
  # the first half of the record entry pair
  d.llAdd s, ConstDataNode(kind: conImm, data: cast[uint32](pos))
  inc s.num


func startRecord*(d: var LiteralData, s: var SessionBase): AddRecordSession =
  testLock(d, s)
  migrate(d, s)
  startSession(d, result)
  result.prev = s.name
  startRecordI(d)

func finish*(d: var LiteralData, s: SessionBase): LiteralId =
  switch(d, s.name, s.prev)

  if s.usesTemp:
    # copy the data back
    let tmpStart = s.id.node.int
    result = toNode addGet(d.nodes, d.temp.toOpenArray(tmpStart, d.temp.high))

    # deallocate the temp region
    d.temp.setLen(tmpStart)
  else:
    result = s.id

func finish*(d: var LiteralData, s: AddRecordSession): LiteralId  {.inline.} =
  result = finish(d, SessionBase(s))
  # set the number of record entries:
  d.nodes[result.node].data = s.num.uint32

template iterImpl[T](d: LiteralData, id: LiteralId) =
  let arr = d.packedArrays[id.arr]

  var i = arr.start
  let
    len = arr.len
    size = arr.elemSize
    lsh = 8 * (sizeof(T) - size.int)
    last = i + (len * size) - 1

  while i <= last:
    var r: T
    r = T(d.packed[i])
    for j in 1..<size:
      r = r or cast[T](BiggestUInt(d.packed[i + j]) shl j * 8)

    when T is SomeSignedInt:
      # sign-extend the result
      r = ashr(r shl lsh, lsh)

    yield r

    # because of uint wrap-around, this is safe even if
    # ``last + size > high(uint32)``
    i += size

iterator ints*(d: LiteralData, id: LiteralId): BiggestInt =
  iterImpl[BiggestInt](d, id)

func packedLen*(d: LiteralData, id: LiteralId): int {.inline.} =
  d.packedArrays[id.arr].len.int

iterator uints*(d: LiteralData, id: LiteralId): BiggestUInt =
  iterImpl[BiggestUInt](d, id)

template toNodeIndex(iter: DataIter): uint32 =
  iter.pos.uint32

template toNodeIndex(id: LiteralId): uint32 =
  node(id)

func len*(d: LiteralData, x: LiteralId|DataIter): int {.inline.} =
  let i = toNodeIndex(x)
  assert d.nodes[i].kind in {conArray, conRecord}
  d.nodes[i].data.int

func getRecPos*(d: LiteralData, iter: DataIter): int {.inline.} =
  # the field position is stored as a ``conImm`` which is also used for other
  # things, so we can't be fully certain that we're pointing at is actually a
  # field position
  assert d.nodes[iter.pos].kind == conImm
  d.nodes[iter.pos].data.int

func next*(iter: var DataIter) {.inline.} =
  inc iter.pos
  inc iter.i

template asStrNode*(n: PNode): StringNode =
  assert n != nil
  assert n.kind in nkStrKinds
  StringNode(n)

template newNumber(v: untyped): LiteralId =
  toId e.numbers.getOrIncl(v), lkNumber

func newLit*(e: var LiteralData, i: SomeSignedInt): LiteralId {.inline.}  =
  newNumber cast[BiggestUInt](BiggestInt(i))

func newLit*(e: var LiteralData, i: SomeUnsignedInt): LiteralId {.inline.} =
  newNumber BiggestUInt(i)

func newLit*(e: var LiteralData, f: BiggestFloat): LiteralId {.inline.} =
  newNumber cast[BiggestUInt](f)


func newLit*(e: var LiteralData, s: sink string): LiteralId {.inline.} =
  # TODO: don't create a temporary node
  let n = newNode(nkStrLit)
  n.strVal = move s
  toId e.strings.getOrIncl(n.StringNode), lkString

func newLit*(e: var LiteralData, s: StringNode): LiteralId {.inline.} =
  toId e.strings.getOrIncl(s), lkString

template getLit*(e: var LiteralData, v: SomeSignedInt|SomeUnsignedInt|BiggestFloat|string|StringNode): LiteralId {.deprecated: "use ``newLit`` instead".}=
  newLit(e, v)

func skipChildren(data: LiteralData, pos: int): int =
  var
    i = pos.uint32
    last = i

  while i <= last:
    let n = data.nodes[i]
    case n.kind
    of conRecord:
      last += n.data * 2 # a ``conRecord`` stores pairs
    of conArray:
      last += n.data
    of conConst, conConstAddr, conLit, conImm, conExt:
      discard "leaf node"

    inc i

  result = i.int - 1

# XXX: hm, maybe each ``LiteralKind`` needs it's own ID type? That would allow
#      for more validation at compile-time
func initDataIter*(data: LiteralData, lit: LiteralId): DataIter =
  assert lit.kind == lkComplex, "literal not iterable"
  result.start = lit.node.int
  result.pos = result.start - 1

func moveInto*(iter: var DataIter, data: LiteralData) =
  inc iter.pos
  case data.nodes[iter.pos].kind
  of conArray, conRecord:
    iter.len = data.nodes[iter.pos].data
    iter.kind = data.nodes[iter.pos].kind
  else:
    discard

func next*(iter: var DataIter, d: LiteralData): ConstDataNode =
  inc iter.pos
  d.nodes[iter.pos]

func skipChildren*(iter: var DataIter, d: LiteralData) =
  iter.pos = skipChildren(d, iter.pos)
  assert iter.pos.int < d.nodes.len

func get*(iter: DataIter, d: LiteralData): ConstDataNode =
  d.nodes[iter.pos]

func asId*(iter: DataIter): LiteralId =
  ## Returns the ID of the node the iterator currently points to
  # XXX: I'm not sure if it's a good idea to allow non-top-level nodes to be
  #      addressed via a ``LiteralId``
  assert iter.pos >= iter.start, "invalid iterator"
  toNode(iter.pos)

func getLit*(d: LiteralData, iter: DataIter): LiteralId =
  let n = d.nodes[iter.pos]
  assert n.kind == conLit
  LiteralId(n.data)

func replaceWithNode(iter: var DataIter, n: ConstDataNode) =
  let npos = iter.changes.temp.len.uint32
  iter.changes.temp.add(n)

  iter.modified = true
  iter.changes.list.add (ckReplace, iter.start.uint32, iter.pos.uint32, npos)

func replaceWithConst*(iter: var DataIter, id: uint32) {.inline.} =
  # TODO: `id` should be a ``SymId``
  replaceWithNode(iter): ConstDataNode(kind: conConst, data: id)

func replaceWithConstAddr*(iter: var DataIter, id: uint32) {.inline.} =
  # TODO: `id` should be a ``SymId``
  replaceWithNode(iter): ConstDataNode(kind: conConstAddr, data: id)

func replaceWithLit*(iter: var DataIter, id: LiteralId) {.inline.} =
  replaceWithNode(iter): ConstDataNode(kind: conLit, data: id.uint32)

func applyInternal(d: var LiteralData, c: sink DChanges, remap: var Table[LiteralId, LiteralId])

template apply*(d: var LiteralData, c: sink DChanges, remap: var Table[LiteralId, LiteralId]) =
  applyInternal(d, c, remap)

func finish*(d: var LiteralData, iter: sink DataIter): LiteralId =
  ## Commits the changes collected by `iter` to `d`. When not ``finish``ing a
  ## ``DataIter``, the changes collected by it will be thrown away at the end
  ## of it's lifetime - the ``LiteralData`` object is not modified
  # TODO: rename to ``commit``?
  if iter.modified:
    var r: Table[LiteralId, LiteralId]
    # XXX: map is not necessary
    applyInternal(d, iter.changes, r)
    result = r[toNode(iter.start)]
  else:
    # return the original ID
    result = toNode(iter.start)

func getStr*(d: LiteralData, id: LiteralId): lent string {.inline.} =
  d.strings[id.str].PNode.strVal

func getStr*(d: LiteralData, iter: DataIter): lent string {.inline.} =
  let n = d.nodes[iter.pos]
  assert n.kind == conLit
  getStr(d, n.data.LiteralId)

iterator sliceListIt*(d: LiteralData, id: LiteralId): (LiteralId, LiteralId) =
  let i = id.node
  assert d.nodes[i].kind == conArray
  let L = d.nodes[i].data
  template get(idx: uint32): LiteralId =
    assert d.nodes[idx].kind == conLit, $d.nodes[idx].kind
    d.nodes[idx].data.LiteralId

  for j in countup(i+1, i+1+L-1, 2):
    yield (get(j+0), get(j+1))

template get*[T](s: AddPackedSession[T]): openArray[T] =
  toOpenArray(s.p, 0, s.len - 1)

func getInt*(d: LiteralData, id: LiteralId): BiggestInt {.inline.} =
  cast[BiggestInt](d.numbers[id.number])

func getUInt*(d: LiteralData, id: LiteralId): BiggestUInt {.inline.} =
  d.numbers[id.number]

func getFloat*(d: LiteralData, id: LiteralId): BiggestFloat {.inline.} =
  cast[BiggestFloat](d.numbers[id.number])

func sym*(d: LiteralData, x: LiteralId|DataIter): uint32 =
  let i = x.toNodeIndex
  assert d.nodes[i].kind in {conConst, conConstAddr}
  d.nodes[i].data

func getExt*(d: LiteralData, x: LiteralId|DataIter): uint32 {.inline.} =
  let i = x.toNodeIndex
  assert d.nodes[i].kind == conExt
  d.nodes[i].data

func enter*(iter: var DataIter, d: LiteralData): DataIter =
  ## Calling ``next`` on the resulting iterator will move it to the first
  ## sub-node of the entered structure
  var n = d.nodes[iter.pos]
  # follow indirections:
  if n.kind == conLit and (let id = n.data.LiteralId; id.kind == lkComplex):
    let start = id.node
    n = d.nodes[start]
    result.start = start.int
  else:
    result.start = iter.pos

  # same as with iterators created with ``initDataIter``, the resulting
  # iterator starts invalid
  result.pos = result.start
  result.kind = n.kind

  case n.kind
  of conArray, conRecord:
    result.len = n.data
  else:
    # support "entering" into non-structures. They're treated as a zero-length
    # structure
    result.len = 0

  swap(iter.changes, result.changes)

iterator mslice[T, A, B](x: var seq[T], s: HSlice[A, B]): var T =
  for i in s:
    yield x[i]

func multiMerge*(c: var DChanges, other: varargs[DChanges]) =
  # TODO: resize the lists first and then copy
  for x in other.items:
    let start = c.list.len
    c.list.add x.list

    # the references to temporary nodes need to be patched
    for it in mslice(c.list, start..<c.list.len):
      case it.kind
      of ckRedirect: discard
      of ckReplace: it.n += c.temp.len.uint32

    # merge the temporary nodes
    c.temp.add x.temp

func applyInternal(d: var LiteralData, c: sink DChanges, remap: var Table[LiteralId, LiteralId]) =
  ## Applies the changes `c` to `d`. Mappings between the original ID and the
  ## ID of the modified version are written to `remap`. To allow for resource
  ## reuse, `remap` is a ``var`` parameter instead of a return value.
  assert remap.len == 0

  type Tup = typeof(c.list[0])
  # sort the changes by modification position. This makes sure that the
  # "only-backward references" property is kept intact.
  # Two changes to the same node are not allowed.
  # TODO: different sorting algorithm? Also use quicksort? (we don't need
  #       stable ordering)
  sort(c.list, proc(a, b: Tup): int =
    if a.struct < b.struct:
      -1
    elif a.struct > b.struct:
      1
    else:
      a.pos.int - b.pos.int
  )

  func finish(d: var LiteralData, struct: LiteralId, src: Natural) =
    # Copies the remaining nodes from the original structure to the new one
    let
      dst = d.nodes.len
      start = struct.node.int
      len = skipChildren(d, start) + 1 - src

    if len > 0:
      d.nodes.setLen(dst + len)
      copyMem(d.nodes, dst, src, len)

  var struct = NoneLit # the start of the current structure to which
                         # modification are applied
  var src: int # copy-source position
  var prev: Tup
  for it in c.list.items:
    let id = it.struct.toNode

    if id != struct:
      # modification are for a different structure
      if struct != NoneLit:
        # no more changes to the previous structure
        finish(d, struct, src)

      struct = id

      remap[id] = toNode d.nodes.len
      src = it.struct.int
    else:
      if it.kind == ckRedirect and it.kind == prev.kind and it.pos == prev.pos:
        # support multiple redirect requests for the same node. It's treated
        # as a no-op
        continue

    prev = it

    # if this assertion triggers, either a node is modified more than once or
    # an inlined structure was modified with a cursor not properly using
    # ``enter``/``close``
    assert it.pos.int >= src

    let
      num = it.pos.int - src
      start = d.nodes.len

    # copy the unmodified nodes from the original
    if num > 0:
      d.nodes.setLen(start + num)
      copyMem(d.nodes, start, src, num)

    # changes are always replacements, so don't copy the children of a
    # modified node
    src = skipChildren(d, it.pos.int) + 1

    case it.kind
    of ckRedirect:
      # redirect a node reference to the location of the modified version. The
      # referenced structure is required to be modified as part of the same
      # changeset
      # TODO: redirection of inlined structures also needs to be supported
      assert d.nodes[it.pos].kind == conLit
      d.nodes.add ConstDataNode(kind: conLit, data: remap[d.nodes[it.pos].data.LiteralId].uint32)
    of ckReplace:
      d.nodes.add c.temp[it.n]

  if struct != NoneLit:
    finish(d, struct, src)

func close*(iter: var DataIter, toClose: sink DataIter) =
  ## Closes `toClose`. Each ``enter`` *must* be paired with a matching ``close``
  swap(iter.changes, toClose.changes)

  inc iter.i

  if toClose.modified:
    # the sub-structure was modified (a modified version of it was created),
    # which also counts as a modification to the structure `iter` is
    # traversing.
    iter.modified = true

    # `iter` still points to the node that ``enter`` was called on. We replace
    # it with a reference to the new sub-structure
    iter.changes.list.add (ckRedirect, iter.start.uint32, iter.pos.uint32, 0'u32)

  # adjust the position so that the next ``next`` moves `iter` to the sibling
  # of the node `toClose` was entered from
  if toClose.start == iter.pos:
    iter.pos = toClose.pos

# ------ tools for debugging

proc dumpAux(d: LiteralData, pos: var int, deref: bool, indent: string) =
  let n = d.nodes[pos]
  inc pos
  echo indent, n
  case n.kind
  of conRecord:
    for i in 0..<n.data:
      dumpAux(d, pos, deref, indent & "  ") # position
      dumpAux(d, pos, deref, indent & "  ") # data

  of conArray:
    for i in 0..<n.data:
      dumpAux(d, pos, deref, indent & "  ")

  of conLit:
    if deref and n.data.LiteralId.kind == lkComplex:
      # follow the node reference. They're never form cycles, so there's no
      # need for guards
      var np = n.data.LiteralId.node.int
      dumpAux(d, np, deref, indent & "  ")

  else:
    discard

proc dump*(d: LiteralData, id: LiteralId, deref = false) =
  ## Echoes the tree representation of the complex data with the given `id`.
  ## If `deref` is true, references to other nodes are followed
  var pos = id.node.int
  dumpAux(d, pos, deref, "")

func `$`*(id: LiteralId): string =
  result.add "(kind: "
  result.add $id.kind
  result.add ", name: "
  result.addInt id.rawName
  result.add ")"

func enumNames[E: enum](): array[E, cstring] {.compileTime.} =
  for x in low(E)..high(E):
    result[x] = cstring($x)

func stats*(e: LiteralData): seq[tuple[name: cstring, val: int]] =
  ## Returns various statistics about the contents of `e`
  result.newSeq(5 + (low(ConstDataNodeKind)..high(ConstDataNodeKind)).len)
  result[0] = (cstring"numbers", e.numbers.len)
  result[1] = (cstring"strings", e.strings.len)
  result[2] = (cstring"nodes", e.nodes.len)
  result[3] = (cstring"packed arrays", e.packedArrays.len)
  result[4] = (cstring"size packed", e.packed.len)

  var count: array[ConstDataNodeKind, int]
  for x in e.nodes.items:
    inc count[x.kind]

  const Names = enumNames[ConstDataNodeKind]()

  for i, n in count.pairs:
    result[5 + ord(i)] = (Names[i], n)