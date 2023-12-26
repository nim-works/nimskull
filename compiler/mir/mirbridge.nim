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

proc echoMir*(config: ConfigRef, owner: PSym, tree: MirTree) =
  ## If requested via the define, renders the `tree` and writes the result out
  ## through ``config.writeln``.
  if config.getStrDefine("nimShowMir") == owner.name.s:
    writeBody(config, "-- MIR: " & owner.name.s):
      config.writeln(treeRepr(tree))

proc echoOutput*(config: ConfigRef, owner: PSym, body: Body) =
  ## If requested via the define, renders the output IR `body` and writes the
  ## result out through ``config.writeLine``.
  if config.getStrDefine("nimShowMirOutput") == owner.name.s:
    writeBody(config, "-- output AST: " & owner.name.s):
      config.writeln(treeRepr(body.code))

proc rewriteGlobalDefs*(body: var MirTree, sourceMap: var SourceMap) =
  ## Rewrites definitions of globals in the outermost scope into assignments.
  # XXX: integrate the pass into ``mirgen`` once the dependency collection
  #      also happens there
  var
    changes = initChangeset(body)
    depth   = 0

  for i, n in body.pairs:
    case n.kind
    of DefNodes:
      let def = body.child(i, 0)
      if body[def].kind == mnkGlobal:
        let
          sym = body[def].sym
          typ = sym.typ
        if depth > 1:
          # don't rewrite the def
          discard
        elif body[i, 1].kind != mnkNone:
          # the global has a starting value
          changes.replaceMulti(body, i, buf):
            let val = buf.inline(body, body.child(i, 1))
            buf.subTree mnkInit:
              buf.use symbol(mnkGlobal, sym)
              buf.use val
        elif {sfImportc, sfNoInit} * sym.flags == {} and
             {exfDynamicLib, exfNoDecl} * sym.extFlags == {}:
          # XXX: ^^ re-think this condition from first principles. Right now,
          #      it's just meant to make some tests work
          # the location doesn't have an explicit starting value. Initialize
          # it to the type's default value.
          changes.replaceMulti(body, i, buf):
            buf.subTree mnkInit:
              buf.use symbol(mnkGlobal, sym)
              buf.buildMagicCall mDefault, typ:
                discard
        else:
          # just remove the def:
          changes.remove(body, i)

    of mnkScope:
      inc depth
    of mnkEnd:
      if n.start == mnkScope:
        dec depth
    else:
      discard "ignore"

  apply(body, prepare(changes))

proc canonicalize*(graph: ModuleGraph, idgen: IdGenerator, owner: PSym,
                   body: PNode, options: set[GenOption]): Body =
  ## Legacy routine. Translates the body `body` of the procedure `owner` to
  ## MIR code, and the MIR code to ``CgNode`` IR.
  echoInput(graph.config, owner, body)
  # step 1: generate a ``MirTree`` from the input AST
  let (tree, sourceMap) = generateCode(graph, owner, options, body)
  echoMir(graph.config, owner, tree)

  # step 2: generate the ``CgNode`` tree
  result = generateIR(graph, idgen, owner, tree, sourceMap)
  echoOutput(graph.config, owner, result)