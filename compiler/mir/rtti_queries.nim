## Some RTTI-related query routines.

import
  compiler/mir/[
    mirtrees,
    mirtypes
  ]

proc hasRttiHeader*(env: TypeEnv, typ: TypeId): bool =
  ## Returns whether a value of `typ` has an RTTI header itself (embedded
  ## types are not considered).
  var typ = env.canonical(typ)
  if env.headerFor(typ, Lowered).kind == tkStruct:
    # seek to the root type:
    while env.headerFor(typ, Lowered).base(env) != VoidType:
      typ = env.headerFor(typ, Lowered).base(env)

    # if the first field of the root type is at position -1, the type
    # has an RTTI header
    result = env.headerFor(typ, Lowered).fieldOffset(env) == -1
  else:
    # non-record types never have an RTTI header themselves
    result = false

proc visit(env: TypeEnv, typ: TypeId, considerBase: bool): bool =
  let desc = env.headerFor(typ, Lowered)
  case desc.kind
  of tkStruct, tkUnion:
    if desc.kind == tkStruct:
      # also traverse the parent type(s)
      if desc.base(env) == VoidType:
        result = considerBase and desc.fieldOffset(env) == -1
      else:
        result = visit(env, desc.base(env), considerBase)
      if result:
        return

    for _, recf in env.fields(desc):
      if visit(env, recf.typ, true):
        return true
    result = false
  of tkArray:
    result = visit(env, desc.elem, true)
  else:
    result = false

proc hasEmbeddedRttiHeaders*(env: TypeEnv, typ: TypeId): bool {.inline.} =
  ## Returns whether a value of `typ` transitively contains types that have
  ## RTTI headers.
  visit(env, typ, false)

proc containsTypeHeaders*(env: TypeEnv, typ: TypeId): bool {.inline.} =
  ## Returns whether `typ` has a type header itself or transitively contains
  ## types that have one.
  visit(env, typ, true)
