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

proc getStrDefine(config: ConfigRef, name: string): string =
  if config.isDefined(name):
    result = config.getDefined(name)
  else:
    result = ""

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

# NOTE: the ``echoX`` are used as a temporary solution for inspecting inputs
# and outputs in the context of compiler debugging until a more
# structured/integrated solution is implemented

proc echoInput*(config: ConfigRef, owner: PSym, body: PNode) =
  ## If requested via the define, renders the input AST `body` and writes the
  ## result out through ``config.writeLine``.
  if config.getStrDefine("nimShowMirInput") == owner.name.s:
    writeBody(config, "-- input AST: " & owner.name.s):
      config.writeln(treeRepr(config, body, reprConfig))

proc echoMir*(config: ConfigRef, owner: PSym, body: MirBody) =
  ## If requested via the define, renders the `body` and writes the result out
  ## through ``config.writeln``.
  if config.getStrDefine("nimShowMir") == owner.name.s:
    writeBody(config, "-- MIR: " & owner.name.s):
      config.writeln(treeRepr(body.code))

proc echoOutput*(config: ConfigRef, owner: PSym, body: Body) =
  ## If requested via the define, renders the output IR `body` and writes the
  ## result out through ``config.writeLine``.
  if config.getStrDefine("nimShowMirOutput") == owner.name.s:
    writeBody(config, "-- output AST: " & owner.name.s):
      config.writeln(treeRepr(body.code))

proc canonicalize*(graph: ModuleGraph, idgen: IdGenerator, env: var MirEnv,
                   owner: PSym, body: PNode, config: TranslationConfig): Body =
  ## Legacy routine. Translates the body `body` of the procedure `owner` to
  ## MIR code, and the MIR code to ``CgNode`` IR.
  echoInput(graph.config, owner, body)
  # step 1: generate a ``MirTree`` from the input AST
  let body = generateCode(graph, env, owner, config, body)
  echoMir(graph.config, owner, body)

  # step 2: generate the ``CgNode`` tree
  result = cgirgen.generateIR(graph, idgen, env, owner, body)
  echoOutput(graph.config, owner, result)