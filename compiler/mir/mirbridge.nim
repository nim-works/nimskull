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
    mirchangesets,
    mirconstr,
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

proc rewriteGlobalDefs(body: var MirTree, sourceMap: var SourceMap,
                       outermost: bool) =
  ## Removes definitions of non-pure globals from `body`, replacing them with
  ## as assignment if necessary.
  ##
  ## If `outermost` is true, only definitions in the outermost scope will be
  ## removed. This is a hack, but its currently required for turning module-level
  ## AST into a procedure in a mostly transparent way.
  var
    changes = initChangeset(body)
    depth   = 0
    i       = NodePosition 0

  while i.int < body.len:
    let n {.cursor.} = body[i]
    case n.kind
    of DefNodes:
      let def = i + 1
      if body[def].kind == mnkGlobal and
        body[def].sym.owner.kind == skModule and
        (not outermost or depth == 1):
        let
          sym = body[def].sym
          typ = sym.typ
        changes.seek(i)
        if sfPure in sym.flags:
          # HACK: yet another hack for the JS backend not using
          #       ``transf.extractGlobals``...
          discard "do nothing"
        elif hasInput(body, Operation i):
          # the definition has a starting value
          # XXX: consider disallowing inputs to 'def's, they complicate things
          #      quite a bit (as made evident here)
          changes.replaceMulti(buf):
            let tmp = changes.getTemp()
            buf.subTree MirNode(kind: mnkDef):
              # assign to a temporary first, and then assign the temporary to the
              # globals
              buf.add MirNode(kind: mnkTemp, temp: tmp, typ: typ)

            argBlock(buf):
              buf.add MirNode(kind: mnkGlobal, sym: sym, typ: typ)
              buf.add MirNode(kind: mnkTag, effect: ekReassign, typ: typ)
              buf.add MirNode(kind: mnkName, typ: typ)
              buf.add MirNode(kind: mnkTemp, temp: tmp, typ: typ)
              buf.add MirNode(kind: mnkConsume, typ: typ)
            buf.add MirNode(kind: mnkInit)

        elif sym.typ.kind in {tyVar, tyLent}:
          # XXX: this works around an issue in ``transf``, where initialization
          #      of the view is disjoint from its definition. Fix the bug and
          #      then remove this special case
          changes.remove()
        elif sfImportc notin sym.flags and
             {lfDynamicLib, lfNoDecl} * sym.loc.flags == {}:
          # XXX: ^^ re-think this condition from first principles. Right now,
          #      it's just meant to make some tests work
          # the location doesn't have an explicit starting value. Initialize
          # it to the type's default value.
          changes.replaceMulti(buf):
            argBlock(buf):
              buf.add MirNode(kind: mnkGlobal, sym: body[def].sym, typ: typ)
              buf.add MirNode(kind: mnkTag, effect: ekReassign, typ: typ)
              buf.add MirNode(kind: mnkName, typ: typ)
              argBlock(buf): discard
              buf.add MirNode(kind: mnkMagic, magic: mDefault, typ: typ)
              buf.add MirNode(kind: mnkConsume, typ: typ)
            buf.add MirNode(kind: mnkInit)
        else:
          # just remove the def:
          changes.remove()

      inc i, 2 # skip the whole sub-tree ('def', name, and 'end' node)
    of mnkScope:
      inc depth
    of mnkEnd:
      if n.start == mnkScope:
        dec depth
    else:
      discard "ignore"

    inc i

  let prepared = prepare(changes, sourceMap)
  updateSourceMap(sourceMap, prepared)
  apply(body, prepared)

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
  ## 3. removal of non-pure global definitions from the outermost scope
  ## 4. application of the ``injectdestructors`` pass
  ## 5. (temporarily) removal of remaining non-pure global definitions
  ## 6. translation back to AST
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

  rewriteGlobalDefs(tree, sourceMap, outermost = true)

  # step 2: run the ``injectdestructors`` pass
  if inject:
    injectDestructorCalls(graph, idgen, owner, tree, sourceMap)

  # XXX: this is a hack. See the documentation of the routine for more
  #      details
  rewriteGlobalDefs(tree, sourceMap, outermost = false)

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