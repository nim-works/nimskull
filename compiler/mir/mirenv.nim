## Implements the `MirEnv <#MirEnv>`_ data type together with routines for
## querying and manipulating it.

import
  std/[
    hashes,
    tables
  ],
  compiler/ast/[
    ast_query,
    ast_types,
    trees,
    types
  ],
  compiler/mir/[
    mirtrees
  ],
  compiler/utils/[
    containers,
    idioms
  ]

type
  ConstrTree = PNode
  DataTable* = object
    ## A bi-directional table that associates constant expressions with an ID.
    vals: Store[DataId, ConstrTree]
    keys: seq[uint32]
      ## indexed by ``hash(ConstrTree)``, using an open-addressing scheme.
      ## Empty slots have value '0'. Subtracting one from the value in a non-
      ## empty slot yields a ``DataId``

  SymbolTable*[I; T] = object
    ## Acts as the central store for a kind of symbol during the mid- and
    ## backend phase. Also keeps mappings for ``PSym`` to the corresponding
    ## backend symbol.
    ##
    ## In the mid- and backend, symbols are represented with IDs rather than
    ## with ``ref`` types.
    data: Store[I, T]
    map: Table[int, I]
      ## maps a symbol ID to the corresponding entity ID

  MirEnv* = object
    ## Stores everything MIR-related that is shared/exists across MIR bodies,
    ## such as information about global variables, global constants, etc.
    data*: DataTable
      ## stores all constant data referenced by constants and MIR code
    constants*:  SymbolTable[ConstId, PSym]
    globals*:    SymbolTable[GlobalId, PSym]
      ## includes both normal globals and threadvars
    procedures*: SymbolTable[ProcedureId, PSym]

  EnvCheckpoint* = tuple
    ## A low-cost snapshot of a `MirEnv <#MirEnv>`_.
    procs, globals, consts, data: Checkpoint

# ------ DataTable implementation ------

func hashTree(tree: ConstrTree): Hash =
  ## Computes the `tree`. The hash is meant to be used for lookup in a hash
  ## table. Keep this synchronized with `cmp <#cmp,ConstrTree,ConstrTree>`_.
  proc hash(n: PNode): Hash {.nimcall.} =
    result = hash(n.kind)
    case n.kind
    of nkIntKinds:
      result = result !& hash(n.intVal)
    of nkFloatLiterals:
      # make sure to hash the bit representation, so that NaNs are accounted
      # for
      result = result !& hash(cast[BiggestUInt](n.floatVal))
    of nkStrKinds:
      result = result !& hash(n.strVal)
    of nkNilLit:
      discard
    of nkSym:
      result = result !& hash(n.sym.id)
    of nkBracket, nkCurly, nkTupleConstr, nkClosure, nkExprColonExpr, nkRange:
      for it in n.items:
        result = result !& hash(it)
    of nkObjConstr:
      for i in 1..<n.len:
        result = result !& hash(n[i])
    else:
      unreachable(n.kind)

  result = hash(tree)
  # only hash the kind of the type. This trades more collisions for faster
  # hashing
  result = result !& hash(tree.typ.kind)
  result = !$(result)

proc cmp(a, b: ConstrTree): bool =
  # same structure and same (backend) type means that the construction
  # expressions produce the same value
  result = exprStructuralEquivalent(a, b) and sameBackendType(a.typ, b.typ)

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

proc getOrPut*(t: var DataTable; tree: PNode): DataId =
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

proc rewind(t: var DataTable, p: Checkpoint) =
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

# -------

func toMir(s: sink PSym, t: typedesc): PSym =
  ## Translates a symbol to the MIR representation for the given entity.
  ## Overload this procedure for customizing the translation process.
  result = s

# ------- SymbolTable API -------

func `[]`*[I; T](tab: SymbolTable[I, T], s: PSym): I {.inline.} =
  ## Looks up the ID that `s` is represented with in `tab`. `s` must have
  ## already been registered in the table.
  tab.map[s.id]

func `[]`*[I; T](tab: SymbolTable[I, T], id: I): lent T {.inline.} =
  tab.data[id]

func contains*[I, T](tab: SymbolTable[I, T], s: PSym): bool {.inline.} =
  ## Returns whether `s` was already registered with `tab`.
  tab.map.contains(s.id)

func add*[I; T](tab: var SymbolTable[I, T], s: PSym): I =
  ## If `s` was not yet added to `tab`, translates `s` to the corresponding
  ## MIR representation and returns the ID to fetch the entity from `tab`
  ## with. If `s` was already added, returns the ID of the already-translated
  ## entity.
  let next = tab.data.nextId()
  result = tab.map.mgetOrPut(s.id, next)
  if result == next:
    let x = tab.data.add(toMir(s, I))
    assert x == result

proc len*[I, T](tab: SymbolTable[I, T]): int =
  ## The number of entities in `tab`.
  tab.data.nextId().int

func checkpoint*[I, T](tab: SymbolTable[I, T]): Checkpoint =
  ## Creates a checkpoint that represents the current state of `tab`. This
  ## is a very low-cost operation, which doesn't perform any heap allocation.
  checkpoint(tab.data)

# ------- MirEnv API --------

func `[]`*(env: MirEnv, id: ConstId): lent PSym {.inline.} =
  env.constants.data[id]

func `[]`*(env: MirEnv, id: GlobalId): lent PSym {.inline.} =
  env.globals.data[id]

func `[]`*(env: MirEnv, id: ProcedureId): lent PSym {.inline.} =
  env.procedures.data[id]

func `[]`*(env: MirEnv, id: DataId): lent ConstrTree {.inline.} =
  env.data.vals[id]

func checkpoint*(env: MirEnv): EnvCheckpoint =
  ## Creates a snapshot of `env`. This is a low-cost operation, where no
  ## copies are involved.
  (env.procedures.checkpoint, env.globals.checkpoint,
   env.constants.checkpoint, env.data.vals.checkpoint)

proc rewind*(env: var MirEnv, to: EnvCheckpoint) =
  ## Undoes all additions to `env` since `to` was created. Do note that all
  ## checkpoints created after `to` are invalidated.
  template rewind[I](tab: SymbolTable[I, PSym], to: Checkpoint) =
    # remove the mappings
    for _, it in since(tab.data, to):
      tab.map.del(it.id)

    # then rewind the Store
    rewind(tab.data, to)

  rewind(env.procedures, to.procs)
  rewind(env.constants, to.consts)
  rewind(env.globals, to.globals)
  rewind(env.data, to.data)

iterator items*[I, T](tab: SymbolTable[I, T]): (I, lent T) =
  ## Returns all entities in `tab` together with their ID.
  for id, it in tab.data.pairs:
    yield (id, it)

iterator since*[I; T](tab: SymbolTable[I, T], p: Checkpoint): (I, lent T) =
  ## Returns all entities from `tab` that were added since `p` was created.
  for id, it in since(tab.data, p):
    yield (id, it)
