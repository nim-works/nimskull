## Implements the `MirEnv <#MirEnv>`_ data type together with routines for
## querying and manipulating it.

import
  std/[
    hashes,
    tables
  ],
  compiler/ast/[
    ast_query,
    ast_types
  ],
  compiler/mir/[
    datatables,
    mirtrees
  ],
  compiler/ic/[
    bitabs
  ],
  compiler/utils/[
    containers
  ]

type
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

    numbers*: BiTable[BiggestInt]
      ## all numerical values referenced by the MIR, stored as bit patterns
    strings*: BiTable[string]
      ## all string data referenced by the MIR
    asts*: Store[AstId, PNode]
      ## all AST fragments referenced by the MIR. No unification is
      ## performed

    bodies*: OrdinalSeq[ConstId, DataId]
      ## associates each user-defined constant with its content
      ## ## TODO: this needs to be merged into `constants`

  EnvCheckpoint* = tuple
    ## A low-cost snapshot of a `MirEnv <#MirEnv>`_.
    procs, globals, consts, data: Checkpoint

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
  env.data[id]

func getInt*(env: MirEnv, id: NumberId): BiggestInt {.inline.} =
  env.numbers[LitId id]

func getUInt*(env: MirEnv, id: NumberId): BiggestUInt {.inline.} =
  cast[BiggestUInt](env.numbers[LitId id])

func getFloat*(env: MirEnv, id: NumberId): BiggestFloat {.inline.} =
  cast[BiggestFloat](env.numbers[LitId id])

func `[]`*(env: MirEnv, id: StringId): lent string {.inline.} =
  env.strings[LitId id]

func `[]`*(env: MirEnv, id: AstId): lent PNode {.inline.} =
  env.asts[id]

func getOrIncl*(env: var MirEnv, v: BiggestInt|BiggestUInt|BiggestFloat
               ): NumberId {.inline.} =
  ## If not registered already, adds `v` to the environment.
  NumberId env.numbers.getOrIncl(cast[BiggestInt](v))

func getOrIncl*(env: var MirEnv, str: string): StringId {.inline.} =
  ## If not registered already, adds `str` to the environment.
  StringId env.strings.getOrIncl(str)

func setData*(env: var MirEnv, id: ConstId, data: DataId) =
  ## Sets the body for the constant identified by `id`.
  synchronize(env.bodies, env.constants.data)
  env.bodies[id] = data

func dataFor*(env: MirEnv, id: ConstId): DataId =
  ## Returns the ID of the constant expression associated with `id`.
  if isAnon(id):
    extract(id)
  else:
    env.bodies[id]

func checkpoint*(env: MirEnv): EnvCheckpoint =
  ## Creates a snapshot of `env`. This is a low-cost operation, where no
  ## copies are involved.
  (env.procedures.checkpoint, env.globals.checkpoint,
   env.constants.checkpoint, env.data.checkpoint)

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
  setLen(env.bodies, to.consts.int)

iterator items*[I, T](tab: SymbolTable[I, T]): (I, lent T) =
  ## Returns all entities in `tab` together with their ID.
  for id, it in tab.data.pairs:
    yield (id, it)

iterator since*[I; T](tab: SymbolTable[I, T], p: Checkpoint): (I, lent T) =
  ## Returns all entities from `tab` that were added since `p` was created.
  for id, it in since(tab.data, p):
    yield (id, it)
