## A temporary module that implements convenience routines for the ``PNode``
## AST to ``CgNode`` translation.

import
  compiler/ast/[
    ast_types,
    ast_idgen,
    ast
  ],
  compiler/backend/[
    cgir,
    cgirgen,
    cgirutils
  ],
  compiler/front/[
    options
  ],
  compiler/mir/[
    mirbodies,
    mirenv,
    mirgen,
    utils
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/utils/[
    astrepr
  ]

export GenOption

template writeBody(config: ConfigRef, header: string, body: untyped) =
  # NOTE: if the debug traces should be kept, they should be properly
  #       integrated into the tracing pipeline
  config.writeln(header)
  body
  config.writeln("-- end")

let reprConfig = block:
  var rc = implicitTReprConf
  rc.flags.excl trfShowFullSymTypes
  rc.flags.excl trfShowNodeTypes
  rc.flags.incl trfShowSymKind
  rc

template isEnabled(config: ConfigRef, ir: IrName, name: string): bool =
  # debugging the IR must be enabled globally or locally
  ir in config.toDebugIr or config.isDebugEnabled(ir, name)

proc echoInput*(config: ConfigRef, owner: PSym, body: PNode) =
  ## If requested via the define, renders the input AST `body` and writes the
  ## result out through ``config.writeLine``.
  if config.isEnabled(irTransf, owner.name.s):
    writeBody(config, "-- input AST: " & owner.name.s):
      config.writeln(treeRepr(config, body, reprConfig))

proc echoMir*(config: ConfigRef, owner: PSym, body: MirBody) =
  ## If requested via the define, renders the `body` and writes the result out
  ## through ``config.writeln``.
  if config.isEnabled(irMirIn, owner.name.s):
    writeBody(config, "-- MIR: " & owner.name.s):
      config.writeln(treeRepr(body.code))

proc echoOutput*(config: ConfigRef, owner: PSym, body: Body) =
  ## If requested via the define, renders the output IR `body` and writes the
  ## result out through ``config.writeLine``.
  if config.isEnabled(irCgir, owner.name.s):
    writeBody(config, "-- CGIR: " & owner.name.s):
      config.writeln(treeRepr(body.code))

proc canonicalize*(graph: ModuleGraph, idgen: IdGenerator, env: var MirEnv,
                   owner: PSym, body: PNode, config: TranslationConfig): Body =
  ## Legacy routine. Translates the body `body` of the procedure `owner` to
  ## MIR code, and the MIR code to ``CgNode`` IR.
  echoInput(graph.config, owner, body)
  # step 1: generate a ``MirTree`` from the input AST
  let body = generateCode(graph, env, owner, config, body)
  echoMir(graph.config, owner, body, env)

  # step 2: generate the ``CgNode`` tree
  result = cgirgen.generateIR(graph, idgen, env, owner, body)
  echoOutput(graph.config, owner, result)