discard """
  target: native
  description: "Unit tests for the `packed_env` module"
"""

import
  compiler/ast/[
    ast_types
  ],
  compiler/front/[
    options
  ],
  compiler/vm/[
    packed_env,
    vmdef
  ]

block empty_nodes:
  # make sure that storing and loading an `nkEmpty` node works
  let
    placeholder = PVmType(kind: akInt) # content is not relevant
    typ = PType(n: PNode(kind: nkEmpty))

  var
    ctx: TCtx
    env: PackedEnv
    enc: PackedEncoder

  # creating a ``ConfigRef`` is unfortunately required for now
  ctx.config = ConfigRef()

  ctx.types.add placeholder
  ctx.rtti.add VmTypeInfo(internal: placeholder, nimType: typ)

  # store the rtti. Must not fail:
  init(enc, ctx.types)
  storeEnv(enc, env, ctx)

  # load the rtti:
  let infos = loadTypeInfos(env, ctx.types)
  doAssert infos.len == 1
  doAssert infos[0].nimType.n.kind == nkEmpty
