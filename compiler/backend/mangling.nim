## Implements the "name mangling" used by the code generators. Name mangling
## needs to make sure that:
## * the resulting identifier is valid according to the target's rules
## * different entities get different names

import
  compiler/ast/[
    ast_types,
    ast_query
  ],
  compiler/mir/[
    mirtypes,
    mirtrees
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/utils/[
    idioms
  ]

from compiler/backend/ccgutils import mangle

const
  CallConvToShort: array[TCallingConvention, string] = [
    "ni", "st", "cd", "sa", "sy", "in", "ni", "fa", "cl", "nc"
  ] ## every name must be unique and composed out of the same number of
    ## letters

proc mangle(g: ModuleGraph, env: TypeEnv, desc: TypeHeader): string
proc mangle(g: ModuleGraph, env: TypeEnv, id: TypeId): string

proc mangle(g: ModuleGraph, typ: PType): string =
  ## For object types, returns a unique name based on the original symbol.
  ## An empty string otherwise.
  if typ.isNil:
    return ""

  proc withLen(s: string): string {.inline.} =
    result = $s.len
    result.add s

  template mangleWithLen(s: string): string =
    withLen(mangle(s))

  if typ.kind == tyObject and typ.typeInst != nil:
    # XXX: not very stable, but guaranteed to be unique and it also works
    #      with the current IC mechanism
    result = "I" & mangleWithLen(typ.typeInst[0].sym.name.s) & "_" & $typ.id
  elif typ.kind == tyObject or (typ.sym != nil and sfImportc in typ.sym.flags):
    # imported types are nominal types from the perspective of name mangling
    let
      s = typ.sym
      m {.cursor.} = g.ifaces[typ.itemId.module].uniqueName

    # the underscores are not strictly necessary; they're only added for
    # better readability
    if sfExported in s.flags:
      # exported object types have unique name within their defining module.
      # "G" stands for "global"
      result = "G" & mangleWithLen(s.name.s) & "_M" & withLen(m)
    elif sfAnon in s.flags:
      # use the local ID as the name. "H" stands for "hidden"
      result = "H" & $s.itemId.item & "_M" & withLen(m)
    else:
      # non-exported objec type's don't necessarily have unique names within a
      # module; the ID is included to produced a unique name
      result = "L" & mangleWithLen(s.name.s) & "_" & $s.itemId.item & "_M" &
               withLen(m)
  else:
    result = "" # needs a structure-derived name

proc mangleStruct(g: ModuleGraph, env: TypeEnv, prefix: string,
                  desc: TypeHeader): string =
  result = prefix
  result.addInt desc.numFields
  # bitsize, offset, custom alignment, etc. can only be used in records that
  # don't use structure-basd mangling, and can thus be ignored here
  for (_, f) in fields(env, desc):
    result.add mangle(g, env, f.typ)

proc mangleProc(result: var string, g: ModuleGraph, env: TypeEnv,
                desc: TypeHeader) =
  ## (return type)(parameter count)(parameters)*(E)?
  let hasVarargs = desc.hasVarargs(env)
  result.add mangle(g, env, desc.retType(env))
  result.addInt desc.numParams + ord(hasVarargs)
  for _, it, flags in env.params(desc):
    result.add mangle(g, env, it)
    # the flags are also part of the procedural type
    if pfByRef in flags:
      result.add "_R"

  if hasVarargs:
    result.add "E" # E for ellipsis

proc mangle(g: ModuleGraph, env: TypeEnv, desc: TypeHeader): string =
  ## Produces a mangled name from the *content* of the type `desc`.
  template recurse(id: TypeId): string =
    mangle(g, env, id)

  case desc.kind
  of tkRecord:
    # no name specified, derive one from the structure
    result = mangleStruct(g, env, "T", desc)
  of tkUnion:
    # no name specified, derive one from the structure
    result = mangleStruct(g, env, "U", desc)
  of tkImported, tkTaggedUnion:
    # requires the original type name
    unreachable()
  of tkInt:
    result = "i"
    result.addInt desc.size(env) * 8
  of tkUInt:
    result = "u"
    result.addInt desc.size(env) * 8
  of tkFloat:
    result = "f"
    result.addInt desc.size(env) * 8
  of tkChar:
    result = "c"
  of tkBool:
    result = "b"
  of tkArray:
    result = "A"
    result.addInt desc.arrayLen(env)
    result.add mangle(g, env, desc.elem)
  of tkProc:
    result = "P"
    result.add CallConvToShort[desc.callConv(env)]
    mangleProc(result, g, env, desc)
  of tkClosure:
    result = "C"
    mangleProc(result, g, env, desc)
  of tkUncheckedArray:
    result = "a"
    result.add recurse(desc.elem)
  of tkSeq:
    result = "s"
    result.add recurse(desc.elem)
  of tkCstring:
    result = "x"
  of tkOpenArray:
    result = "o"
    result.add recurse(desc.elem)
  of tkRef:
    result = "r"
    result.add recurse(desc.elem)
  of tkPtr:
    result = "p"
    result.add recurse(desc.elem)
  of tkPointer:
    result = "pV" # mangle as ``ptr void``
  of tkVar:
    result = "v"
    result.add recurse(desc.elem)
  of tkLent:
    result = "l"
    result.add recurse(desc.elem)
  of tkVoid:
    result = "V"
  of tkString:
    result = "S"
  of tkSet:
    result = "e"
    result.addInt desc.count
  of tkIndirect:
    unreachable("cannot mangle")

proc mangle(g: ModuleGraph, env: TypeEnv, id: TypeId): string =
  result = mangle(g, env[id])
  if result.len == 0:
    # derive the mangled name from the canonical representation
    result = mangle(g, env, env.headerFor(id, Canonical))

  assert result.len > 0

proc computeTypeName*(g: ModuleGraph, env: TypeEnv, typ: TypeId): string =
  ## Computes the name to address the type with in the generated code. Mangled
  ## names are always prefixed with an underscore.
  let n = env.get(env.canonical(typ)).desc[Canonical]

  case env[n].kind
  of tkRecord, tkUnion:
    let inst = env.get(typ).inst
    if inst != nil and inst.sym != nil and sfExportc in inst.sym.flags:
      # the type has an external name, use that verbatim
      inst.sym.extname
    else:
      # use the mangled/decorated name
      "_" & mangle(g, env, typ)
  of tkImported:
    # use the specified external name as-is
    env.get(typ).inst.sym.extname
  of tkString:
    "NimStringV2"
  else:
    "_" & mangle(g, env, env[n])
