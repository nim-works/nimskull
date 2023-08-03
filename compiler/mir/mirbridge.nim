## A temporary module that implements convenience routines for the ``PNode``
## AST <-> ``MirTree`` translation.
##
## With the current implementation, the code-generators are responsible for
## running both translation steps (plus any MIR passes), but this is the wrong
## approach -- the code reaching the code-generators should have already been
## processed (i.e. MIR translation and passes done).

import
  compiler/ast/[
    ast_types,
    ast_idgen,
    ast
  ],
  compiler/front/[
    options
  ],
  compiler/mir/[
    astgen,
    mirtrees,
    mirgen,
    sourcemaps,
    utils
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    injectdestructors,
    varpartitions
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

proc echoInput(config: ConfigRef, owner: PSym, body: PNode) =
  ## If requested via the define, renders the input AST `body` and writes the
  ## result out through ``config.writeLine``.
  if config.getStrDefine("nimShowMirInput") == owner.name.s:
    writeBody(config, "-- input AST: " & owner.name.s):
      config.writeln(treeRepr(config, body, reprConfig))

proc echoMir(config: ConfigRef, owner: PSym, tree: MirTree) =
  ## If requested via the define, renders the `tree` and writes the result out
  ## through ``config.writeln``.
  if config.getStrDefine("nimShowMir") == owner.name.s:
    writeBody(config, "-- MIR: " & owner.name.s):
      config.writeln(print(tree))

proc echoOutput(config: ConfigRef, owner: PSym, body: PNode) =
  ## If requested via the define, renders the output AST `body` and writes the
  ## result out through ``config.writeLine``.
  if config.getStrDefine("nimShowMirOutput") == owner.name.s:
    writeBody(config, "-- output AST: " & owner.name.s):
      config.writeln(treeRepr(config, body, reprConfig))

proc canonicalize*(graph: ModuleGraph, idgen: IdGenerator, owner: PSym,
                   body: PNode, options: set[GenOption]): PNode =
  ## No MIR passes exist yet, so the to-and-from translation is treated as a
  ## canonicalization step. To be able to step-by-step rewrite
  ## transformations done in ``transf`` and in the back-ends as MIR passes, it
  ## is important that ``canonicalize`` is applied to *all* code reaching
  ## the code-generators, so that they can depend on the shape of the
  ## resulting AST
  let config = graph.config
  if config.getStrDefine("nimShowMirInput") == owner.name.s:
    writeBody(config, "-- input AST: " & owner.name.s):
      config.writeln(treeRepr(config, body, reprConfig))

  # step 1: generate a ``MirTree`` from the input AST
  let (tree, sourceMap) = generateCode(graph, owner, options, body)

  if graph.config.getStrDefine("nimShowMir") == owner.name.s:
    writeBody(config, "-- MIR: " & owner.name.s):
      config.writeln(print(tree))

  # step 2: translate it back
  result = generateAST(graph, idgen, owner, tree, sourceMap)

  if config.getStrDefine("nimShowMirOutput") == owner.name.s:
    writeBody(config, "-- output AST: " & owner.name.s):
      config.writeln(treeRepr(config, result, reprConfig))

proc canonicalizeWithInject*(graph: ModuleGraph, idgen: IdGenerator,
                             owner: PSym, body: PNode,
                             options: set[GenOption]): PNode =
  ## Transforms `body` through the following steps:
  ## 1. cursor inference
  ## 2. translation to MIR code
  ## 3. application of the ``injectdestructors`` pass
  ## 4. translation back to AST
  ##
  ## Cursor inference and destructor injection are only performed if `owner`
  ## is eligible according to ``injectdestructors.shouldInjectDestructorCalls``
  let config = graph.config

  # cursor inference is not a MIR pass yet, so it has to be applied before
  # the MIR translation/processing
  let inject = shouldInjectDestructorCalls(owner)
  if inject and optCursorInference in config.options:
    computeCursors(owner, body, graph)

  echoInput(config, owner, body)

  # step 1: generate a ``MirTree`` from the input AST
  var (tree, sourceMap) = generateCode(graph, owner, options, body)
  echoMir(config, owner, tree)

  # step 2: run the ``injectdestructors`` pass
  if inject:
    injectDestructorCalls(graph, idgen, owner, tree, sourceMap)

  # step 3: translate the MIR code back to an AST
  result = generateAST(graph, idgen, owner, tree, sourceMap)
  echoOutput(config, owner, result)

proc canonicalizeSingle*(graph: ModuleGraph, idgen: IdGenerator, owner: PSym,
                         n: PNode, options: set[GenOption]): PNode =
  ## Similar to ``canonicalize``, but accepts a freestanding expression or
  ## statement. The `owner` is used as the owner when generating the necessary
  ## new symbols or types
  var
    tree: MirTree
    sourceMap: SourceMap

  # step 1: generate a ``MirTree`` from the input AST
  generateCode(graph, options, n, tree, sourceMap)
  # step 2: translate it back, but only if there is something to translate
  result =
    if tree.len > 0: generateAST(graph, idgen, owner, tree, sourceMap)
    else:            newNode(nkEmpty)