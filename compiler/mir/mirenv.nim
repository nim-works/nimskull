## Implements the `MirEnv <#MirEnv>`_ data type together with routines for
## querying and manipulating it.

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_query,
    ast_types
  ],
  compiler/mir/[
    mirtrees
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
    constants*:  SymbolTable[ConstId, PSym]
    globals*:    SymbolTable[GlobalId, PSym]
      ## includes both normal globals and threadvars
    procedures*: SymbolTable[ProcedureId, PSym]

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

# ------- MirEnv API --------

func `[]`*(env: MirEnv, id: ConstId): lent PSym {.inline.} =
  env.constants.data[id]

func `[]`*(env: MirEnv, id: GlobalId): lent PSym {.inline.} =
  env.globals.data[id]

func `[]`*(env: MirEnv, id: ProcedureId): lent PSym {.inline.} =
  env.procedures.data[id]

iterator items*[I, T](tab: SymbolTable[I, T]): (I, lent T) =
  ## Returns all entities in `tab` together with their ID.
  for id, it in tab.data.pairs:
    yield (id, it)
