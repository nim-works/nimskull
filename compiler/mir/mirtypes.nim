## Implements the type IR for the MIR phase (not yet), plus the
## `TypeEnv <#TypeEnv>`_, which stores the data for all types.
##
## All types are addressed via ``TypeId``, with the built-in types using
## static IDs.

import
  std/[
    hashes,
    tables
  ],
  compiler/ast/[
    ast_types,
    lineinfos
  ],
  compiler/front/[
    options
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ],
  compiler/mir/[
    mirtrees,
    typemaps
  ],
  compiler/utils/[
    containers,
    idioms
  ]

type
  TypeEnv* {.requiresInit.} = object
    ## Stores the data associated with types. Has no valid default value, and
    ## must be explicitly initialized first.
    map: TypeTable[TypeId]
      ## maps the hash of a type. Since the hash is not guaranteed to be
      ## unique, hash collisions are possible!
      # XXX: ^^ the collision needs to be addressed at some point. A proper,
      #      non-sighash-based comparision needs to be used
    types: Store[TypeId, PType]
    sizeType: TypeId
      ## the target-dependent integer type to use for size values

const
  VoidType*    = TypeId 0
  BoolType*    = TypeId 1
  CharType*    = TypeId 2
  Int8Type*    = TypeId 3
  Int16Type*   = TypeId 4
  Int32Type*   = TypeId 5
  Int64Type*   = TypeId 6
  UInt8Type*   = TypeId 7
  UInt16Type*  = TypeId 8
  UInt32Type*  = TypeId 9
  UInt64Type*  = TypeId 10
  Float32Type* = TypeId 11
  Float64Type* = TypeId 12
  StringType*  = TypeId 13
  CstringType* = TypeId 14
  PointerType* = TypeId 15

proc initTypeEnv*(graph: ModuleGraph): TypeEnv =
  ## Returns a fully initialized type environment instance.
  result = TypeEnv(map: default(TypeTable[TypeId]),
                   types: default(Store[TypeId, PType]),
                   sizeType: VoidType)

  template add(kind: TTypeKind, expect: TypeId) =
    let
      typ = graph.getSysType(unknownLineInfo, kind)
      id  = result.types.add(typ)
    assert id == expect
    # the type needs to be mapped too
    result.map[typ] = id

  add(tyVoid, VoidType)
  add(tyBool, BoolType)
  add(tyChar, CharType)
  add(tyInt8, Int8Type)
  add(tyInt16, Int16Type)
  add(tyInt32, Int32Type)
  add(tyInt64, Int64Type)
  add(tyUInt8, UInt8Type)
  add(tyUInt16, UInt16Type)
  add(tyUInt32, UInt32Type)
  add(tyUInt64, UInt64Type)
  add(tyFloat32, Float32Type)
  add(tyFloat64, Float64Type)
  add(tyString, StringType)
  add(tyCstring, CstringType)
  add(tyPointer, PointerType)

  # also register the built-in unspecified-width types. This prevents int/float
  # literal types from being added to the environment
  add(tyInt,   TypeId(ord(PointerType) + 1))
  add(tyFloat, TypeId(ord(PointerType) + 2))

  result.sizeType =
    case graph.config.target.intSize
    of 1, 2, 4: Int32Type
    of 8:       Int64Type
    else:       unreachable()

proc add*(env: var TypeEnv, t: PType): TypeId =
  ## If not registered yet, adds `t` to `env` and returns the ID to later
  ## look it up with. Basic structural type unification is performed.
  result = env.map.mgetOrPut(t, env.types.nextId())
  if result == env.types.nextId():
    result = env.types.add(t)

func `[]`*(env: TypeEnv, id: TypeId): lent PType {.inline.} =
  env.types[id]

func sizeType*(env: TypeEnv): TypeId {.inline.} =
  ## Returns the type to use for values representing some size. This is a
  ## signed integer type of target-dependent bit-width.
  env.sizeType
