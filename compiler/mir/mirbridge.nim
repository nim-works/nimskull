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
      config.writeln(print(tree))

proc echoOutput*(config: ConfigRef, owner: PSym, body: Body) =
  ## If requested via the define, renders the output IR `body` and writes the
  ## result out through ``config.writeLine``.
  if config.getStrDefine("nimShowMirOutput") == owner.name.s:
    writeBody(config, "-- output AST: " & owner.name.s):
      config.writeln(treeRepr(body.code))

proc restoreGlobal(s: PSym): PSym =
    ## If the global `s` is a duplicate that was introduced in order to make
    ## the code temporarily semantically correct, restores the original
    ## symbol -- otherwise returns `s` as is.
    ##
    ## Refer to ``transf.freshVars`` for why this workaround exists.
    if s.owner.kind in {skVar, skLet, skForVar}:
      s.owner
    else:
      s

proc rewriteGlobalDefs*(body: var MirTree, sourceMap: var SourceMap;
                        patch: bool) =
  ## Rewrites definitions of globals in the outermost scope into assignments.
  ## If `patch` is true, also restores the correct symbol for globals that
  ## were introduced in ``transf``.
  var
    changes = initChangeset(body)
    depth   = 0
    i       = NodePosition 0

  while i.int < body.len:
    let n {.cursor.} = body[i]
    case n.kind
    of DefNodes:
      let def = i + 1
      if body[def].kind == mnkGlobal:
        let
          sym = restoreGlobal(body[def].sym)
          typ = sym.typ
        if depth > 1:
          # don't rewrite the def, but still patch the symbol if requested
          if patch:
            changes.replace(body, i + 1):
              MirNode(kind: mnkGlobal, sym: sym, typ: typ)
        # HACK: ``vmjit`` currently passes us expressions where a 'def' can
        #       be the very first node, something that ``hasInput`` doesn't
        #       support. We thus have to guard against i == 0
        elif i.int > 0 and hasInput(body, Operation i):
          # the global has a starting value
          changes.replaceMulti(body, i, buf):
            let tmp = changes.getTemp()
            buf.subTree MirNode(kind: mnkDef):
              # assign to a temporary first, and then assign the temporary to the
              # global
              buf.add MirNode(kind: mnkTemp, temp: tmp, typ: typ)

            argBlock(buf):
              chain(buf): symbol(mnkGlobal, sym) => tag(ekReassign) => name()
              chain(buf): temp(typ, tmp) => consume()
            buf.add MirNode(kind: mnkInit)
        elif {sfImportc, sfNoInit} * sym.flags == {} and
             {exfDynamicLib, exfNoDecl} * sym.extFlags == {}:
          # XXX: ^^ re-think this condition from first principles. Right now,
          #      it's just meant to make some tests work
          # the location doesn't have an explicit starting value. Initialize
          # it to the type's default value.
          changes.replaceMulti(body, i, buf):
            argBlock(buf):
              chain(buf): symbol(mnkGlobal, sym) => tag(ekReassign) => name()
              argBlock(buf): discard
              chain(buf): magicCall(mDefault, typ) => consume()
            buf.add MirNode(kind: mnkInit)
        else:
          # just remove the def:
          changes.remove(body, i)

      inc i, 2 # skip the whole sub-tree ('def', name, and 'end' node)
    of mnkGlobal:
      # remove the temporary duplicates of nested globals again:
      if patch and depth > 1:
        let s = restoreGlobal(n.sym)
        if s != n.sym:
          changes.replace(body, i):
            MirNode(kind: mnkGlobal, sym: s, typ: s.typ)

    of mnkScope:
      inc depth
    of mnkEnd:
      if n.start == mnkScope:
        dec depth
    else:
      discard "ignore"

    inc i

  apply(body, prepare(changes))

proc patchGlobals*(body: var MirTree, sourceMap: var SourceMap) =
  # Restores the correct symbol for all globals duplicated during
  # ``transf``.
  for i in 0..<body.len:
    if body[i].kind == mnkGlobal:
      let s = restoreGlobal(body[i].sym)
      # use in-place patching; it's more efficient than going through
      # a changeset
      if s != body[i].sym:
        body[i].sym = s

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