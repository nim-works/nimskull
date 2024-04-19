## Implements a bi-directional table that associates constant data
## (represented by a subset of the MIR) with a ``DataId`` and vice versa.

import
  std/[
    hashes
  ],
  compiler/mir/[
    mirtrees
  ],
  compiler/utils/[
    containers,
    idioms
  ]

type
  ConstrTree* = MirTree
  DataTable* = object
    ## A bi-directional table that associates constant expressions with an ID.
    vals: Store[DataId, ConstrTree]
    keys: seq[uint32]
      ## indexed by ``hash(ConstrTree)``, using an open-addressing scheme.
      ## Empty slots have value '0'. Subtracting one from the value in a non-
      ## empty slot yields a ``DataId``

func hashTree(tree: ConstrTree): Hash =
  ## Computes the `tree`. The hash is meant to be used for lookup in a hash
  ## table. Keep this synchronized with `cmp <#cmp,ConstrTree,ConstrTree>`_.
  func hash(n: MirNode): Hash {.nimcall.} =
    result = hash(n.kind)
    case n.kind
    of mnkIntLit, mnkUIntLit, mnkFloatLit:
      result = result !& hash(n.number)
    of mnkStrLit:
      result = result !& hash(n.strVal)
    of mnkAstLit:
      result = result !& hash(n.ast)
    of mnkProcVal:
      result = result !& hash(n.prc.ord)
    of mnkSetConstr, mnkRange, mnkArrayConstr, mnkSeqConstr, mnkTupleConstr,
       mnkClosureConstr, mnkObjConstr, mnkRefConstr:
      result = result !& hash(n.len)
    of mnkField:
      result = result !& hash(n.field)
    of mnkArg, mnkEnd, mnkNilLit:
      discard
    of AllNodeKinds - ConstrTreeNodes:
      unreachable(n.kind)

  # hash the nodes:
  for _, it in tree.pairs:
    result = result !& hash(it)

  result = result !& hash(tree[0].typ)
  result = !$(result)

proc cmp(a, b: ConstrTree): bool =
  ## Compares two MIR constant expressions for structural equality.
  proc `==`(a, b: MirNode): bool {.nimcall.} =
    if a.kind != b.kind:
      return false # cannot be the same

    case a.kind
    of mnkIntLit, mnkUIntLit, mnkFloatLit:
      a.number == b.number
    of mnkStrLit:
      a.strVal == b.strVal
    of mnkAstLit:
      a.ast == b.ast
    of mnkProcVal:
      a.prc == b.prc
    of mnkSetConstr, mnkRange, mnkArrayConstr, mnkSeqConstr, mnkTupleConstr,
       mnkClosureConstr, mnkObjConstr, mnkRefConstr:
      a.len == b.len
    of mnkField:
      a.field == b.field
    of mnkArg, mnkEnd, mnkNilLit:
      true # same node kind -> equal nodes
    of AllNodeKinds - ConstrTreeNodes:
      unreachable(a.kind)

  if a[0].typ != b[0].typ or a.len != b.len:
    # the (backend-)type is different -> not the same constant expressions
    return false

  # both trees are known to have the same length at this point
  # traverse both trees simultaneously and look for a divergence:
  for i in 0..<a.len:
    if a[i] != b[i]:
      return false # divergence -> not equal

  # the cursor reached the end; the trees are equal
  result = true

proc nextTry(h, maxHash: Hash): Hash {.inline.} =
  result = (h + 1) and maxHash

template maxHash(t: DataTable): untyped = high(t.keys)
template isFilled(x: uint32): bool = x.uint32 > 0'u32
template toId(x: uint32): DataId = DataId(x - 1)

iterator pairs*(t: DataTable): (DataId, lent ConstrTree) =
  for id, t in t.vals.pairs:
    yield (id, t)

proc mustRehash(length, counter: int): bool {.inline.} =
  assert(length > counter)
  result = (length * 2 < counter * 3) or (length - counter < 4)

proc enlarge(t: var DataTable) =
  # allocate a new sequence:
  var s: seq[uint32]
  newSeq(s, t.keys.len * 2)
  swap(t.keys, s)

  # move all elements from the old sequence into the new one:
  for i in 0..<s.len:
    let key = s[i]
    if isFilled(key):
      var j = hashTree(t.vals[toId(key)]) and maxHash(t)
      while isFilled(t.keys[j]):
        j = nextTry(j, maxHash(t))
      t.keys[j] = key

proc len*(t: DataTable): int {.inline.} =
  ## The number of items in `t`.
  t.vals.nextId().int

proc getOrPut*(t: var DataTable; tree: sink MirTree): DataId =
  ## Adds `tree` to `t` and returns the ID the tree can be queried with later.
  ## If the tree already exists in the table, only the ID is returned.
  let origH = hashTree(tree)
  var h = origH and maxHash(t)
  if t.keys.len > 0:
    while isFilled(t.keys[h]):
      if cmp(t.vals[toId(t.keys[h])], tree):
        return DataId(t.keys[h] - 1)
      h = nextTry(h, maxHash(t))

    # not found, we need to insert it:
    if mustRehash(t.keys.len, t.len):
      enlarge(t)
      # recompute where to insert:
      h = origH and maxHash(t)
      while isFilled(t.keys[h]):
        h = nextTry(h, maxHash(t))
  else:
    t.keys.setLen(16)
    h = origH and maxHash(t)

  result = t.vals.add(tree)
  t.keys[h] = uint32(result) + 1

func `[]`*(t: DataTable, id: DataId): lent ConstrTree {.inline.} =
  ## Returns the (existing) expression associated with `id`.
  t.vals[id]

func checkpoint*(t: DataTable): Checkpoint {.inline.} =
  checkpoint(t.vals)

proc rewind*(t: var DataTable, p: Checkpoint) =
  for id, it in since(t.vals, p):
    var h = hashTree(it) and maxHash(t)
    # look for the slot holding the ID and clear it:
    while isFilled(t.keys[h]):
      if cmp(t.vals[toId(t.keys[h])], it):
        t.keys[h] = 0 # clear the slot
        break
      h = nextTry(h, maxHash(t))

  # now trim the store:
  rewind(t.vals, p)

iterator since*(t: DataTable, p: Checkpoint): (DataId, lent ConstrTree) =
  ## Returns all entries added since `p` was created.
  for id, it in since(t.vals, p):
    yield (id, it)