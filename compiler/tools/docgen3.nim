## This module provides logic for registration of the documentable entries,
## serialization into simple formats (json, line dump, ctags)

import
  sem/[
    semdata,
    sem,
    passes
  ],
  front/[
    options,
    msgs
  ],
  ast/[
    ast,
    renderer,
    lineinfos,
    parser
  ],
  modules/[
    modulegraphs,
    modules
  ],
  utils/[
    astrepr,
    debugutils,
    pathutils
  ],
  ./docgen_types,
  ./docgen_file_tracking,
  ./docgen_use_registration,
  ./docgen_unparser,
  ./docgen_ast_aux,
  ./docgen_sqlite,
  ./docgen_text,
  ./dump_ir,
  std/[
    tables,
    hashes,
    strutils,
    intsets,
  ]

import std/options as std_options

static:
  assert(
    defined(useNodeIds),
    "Documentation generator requires node ids to be enabled")


func withUser(visitor: sink DocVisitor, user: DocEntryId): DocVisitor =
  result = visitor
  result.docUser = user

func withParent(visitor: sink DocVisitor, user: DocEntryId): DocVisitor =
  result = visitor
  result.parent = user

proc getFirstComment(node: PNode): string =
  if node.isNil():
    return

  elif node.comment.len != 0:
    return node.comment

  elif node.safeLen() != 0:
    for sub in node:
      return getFirstComment(sub)

  
proc addDocText(db: var DocDb, node: PNode, module: DocEntryId): DocTextId =
  if node.kind == nkCall:
    # runnable example
    result = db.add DocText(
      isRunnable: true,
      text: strip($node[^1], chars = {'\n'}),
      implicit: module,
      location: db.add nodeLocation(node)
    )

  else:
    result = db.add DocText(
      text: node.getFirstComment(),
      location: db.add nodeLocation(node))

proc registerPreUses(
    conf: ConfigRef,
    db: var DocDb,
    visitor: DocVisitor,
    node: PNode
  ) =
  ## Register all usage information that can be meaningfully collected
  ## before semantic passes. For now this includes conditional import and
  ## include usages.
  let file = node.info.fileIndex

  proc addImport(node: PNode) =
    discard "TODO"

  proc addInclude(node: PNode) =
    discard "TODO"

  proc aux(node: PNode, vistor: DocVisitor) =
    case node.kind:
      of nkImportStmt:
        let file = node.info.fileIndex
        for sub in node:
          addImport(sub)

      of nkFromStmt, nkImportExceptStmt:
        addImport(node[0])

      of nkIncludeStmt:
        for sub in node:
          addInclude(sub)

      of nkStmtList, nkBlockStmt, nkStmtListExpr, nkBlockExpr:
        for sub in node:
          aux(sub, visitor)

      of nkWhenStmt:
        for (visitor, body) in visitWhen(visitor, node):
          aux(body, visitor)

      else:
        discard

  aux(node, visitor)




proc getEntryName(node: PNode): DefName =
  case node.kind:
    of nkIdentDefs, nkTypeDef, nkConstDef, nkProcDeclKinds:
      result = getEntryName(node[0])

    else:
      result = unparseName(node)

proc classifyDeclKind(db: var DocDb, def: DefTree): DocEntryKind =
  ## Get declaration kind based on the definition tree and existing
  ## declaration context in the database. Most mappings are done directly,
  ## based on the definition tree, but `object` can be mapped into
  ## 'defect', 'effect', 'exception' and 'object' depending on it's parent
  ## type's mapping.
  case def.kind:
    of deftArg: result = ndkArg
    of deftLet, deftVar, deftConst: result = ndkVar
    of deftMagic: result = ndkBuiltin
    of deftField: result = ndkField
    of deftEnumField: result = ndkEnumField
    of deftDistinct: result = ndkDistinctAlias
    of deftEnum: result = ndkEnum
    of deftProc:
      case def.node.kind:
        of nkProcDef:      result = ndkProc
        of nkTemplateDef:  result = ndkTemplate
        of nkMacroDef:     result = ndkMacro
        of nkMethodDef:    result = ndkMethod
        of nkIteratorDef:  result = ndkIterator
        of nkFuncDef:      result = ndkFunc
        of nkConverterDef: result = ndkConverter
        else:
          failNode def.node

    of deftObject:
      case def.getSName():
        of "CatchableError": result = ndkException
        of "Defect":         result = ndkDefect
        of "RootEffect":     result = ndkEffect
        else:
          if def.objBase.isNone():
            result = ndkObject

          elif db.approxContains(def.objBase.get()):
            var base = db[def.objBase.get()]
            result = db[base].kind
            # Traverse chain of intermediate alias declarations to get to
            # the base object
            while result notin ndkStructKinds:
              # Examples of types inherited from aliases - `stream` module,
              # and `PipeOutStream` in `streamwrapper`
              if db[base].baseTypeId.isSome():
                # FIXME when sem-only parsing is used `baseTypeId` should
                # always be present, and if non-sem parsing is used
                # `objBase` ID should always be empty ID, so this check is
                # means that `baseTypeId =` creation in `deftAlias`
                # regitration does not work properly.
                base = db[base].baseTypeId.get()
                result = db[base].kind

              else:
                result = ndkObject

          else:
            result = ndkObject

    of deftAlias:
      if def.node.kind in {nkInfix}:
        result = ndkTypeclass

      else:
        result = ndkAlias



proc getDeprecated(name: DefName): Option[string] =
  ## Get optional text of the deprecation message. If annotation had no
  ## message, return `some("")`
  let depr = name.pragmas.filterPragmas(@["deprecated"])
  if 0 < len(depr):
    let depr = depr[0]
    if depr.kind == nkExprColonExpr:
      result = some depr[1].getSName()

    else:
      result = some ""

proc updateCommon(
    db: var DocDb, id: DocEntryId, decl: DefTree, visitor: DocVisitor) =
  ## Update common fields from the unparsed definition tree
  # Set deprecation message
  db.setDeprecatedMsg(id, getDeprecated(decl.name))
  # Store location of the entry declaration
  db[id].location = some db.add(decl.nameNode().nodeLocation())
  # Store full declaration extent
  db[id].extent = some db.add(decl.node().nodeExtent())

  for doc in decl.docs:
    db[id].docs.add db.addDocText(doc, visitor.activeModule)

  if decl.hasSym():
    db[id].sym = decl.sym

  if decl.exported:
    db[id].visibility = dvkPublic


const nkEntryDeclarationKinds = nkProcDeclKinds + {
  nkTypeDef,
  nkVarSection,
  nkLetSection,
  nkConstSection
}

proc registerEntryDef(
  db: var DocDb, visitor: DocVisitor, node: PNode): seq[DocEntryId]

proc registerNestedDecls(db: var DocDb, node: PNode, visitor: DocVisitor)

proc registerIdentDefs(
    db: var DocDb,
    visitor: DocVisitor,
    node: PNode,
    nodeKind: DocEntryKind
  ): seq[DocEntryId] =

  let defs = unparseDefs(node)
  for def in defs:
    let pragma = def.pragmas.filterPragmas(
      @["intdefine", "strdefine", "booldefine"])

    let nodeKind = tern(0 < len(pragma), ndkCompileDefine, nodeKind)

    var doc = db.newDocEntry(visitor, nodeKind, def.nameNode())
    db.registerNestedDecls(def.typ, visitor.withUser(doc))
    db.registerNestedDecls(def.initExpr, visitor.withUser(doc))
    db.updateCommon(doc, def, visitor)
    result.add doc


proc registerNestedDecls(db: var DocDb, node: PNode, visitor: DocVisitor) =
  ## Registed recursively nested symbol declarations in the node
  if db.isFromMacro(node) or isRunnableExamples(node):
    return

  case node.kind:
    of nkEntryDeclarationKinds:
      discard registerEntryDef(db, visitor, node)

    of nkIdentDefs, nkConstDef:
      # Nested identifier definitions without var/let/const section can
      # appear only in the procedure arguments
      discard registerIdentDefs(db, visitor, node, ndkArg)

    of nkTupleTy:
      discard registerIdentDefs(db, visitor, node[0], ndkTupleField)

    of nkSym:
      if node.sym.kind in {
        skLet, # `except XXXX as e` introduces let symbol
        skForVar # `for a in XXXX` creates new variable
      } and node notin db:
        discard db.newDocEntry(visitor, ndkLet, node)

    else:
      for sub in node:
        db.registerNestedDecls(sub, visitor)

proc registerProcDef(
    db: var DocDb, visitor: DocVisitor, def: DefTree): DocEntryId =
  ## Register new procedure definition in the documentation database,
  ## return constructed entry
  result = db.newDocEntry(
    visitor, db.classifyDeclKind(def), def.nameNode())

  let visitor = visitor.withParent(result)
  let name = def.getSName()

  # Classify procedure kind
  var pkind: DocProcKind
  case name:
    of "=destroy": pkind = dpkDestructor
    of "=sink":    pkind = dpkMoveOverride
    of "=copy":    pkind = dpkCopyOverride
    of "=":        pkind = dpkAsgnOverride
    else:
      if name[^1] == '=' and name[0] in IdentStartChars:
        pkind = dpkPropertySet

      elif name[0] in IdentStartChars:
        if name.startsWith("init") or name.startsWith("new"):
          pkind = dpkConstructor

        else:
          pkind = dpkRegular

      else:
        pkind = dpkOperator

  db[result].procKind = pkind

  # Register arguments and nested symbol declarations
  for argument in def.arguments:
    let arg = db.newDocEntry(
      visitor = visitor,
      kind = ndkArg,
      name = argument.nameNode()
    )

    db.updateCommon(arg, argument, visitor)
    db.registerNestedDecls(
      argument.typ, visitor.withUser(arg).withParent(arg))

    db[arg].argType = argument.typ
    db[arg].argDefault = argument.initExpr

  # Register nested documentable entry declarations
  db.registerNestedDecls(
    def.node[bodyPos], visitor.withUser(result))

  db.registerNestedDecls(
    def.node[genericParamsPos], visitor.withUser(result))

proc registerTypeDef(
    db: var DocDb, visitor: DocVisitor, decl: DefTree): DocEntryId =
  case decl.kind:
    of deftObject:
      let declKind = db.classifyDeclKind(decl)
      result = db.newDocEntry(visitor, declKind, decl.nameNode())

      if decl.objBase.isSome():
        let base = decl.objBase.get()
        if db.approxContains(base):
          # QUESTION - `else:` somehow store the unnamed base type string?
          # This would require handling `superTypes` differently
          db[result].superTypes.add db[base]


      let objId = result
      var db = addr db
      # "'db' is of type <var DocDb> which cannot be captured as it would
      # violate memory safety". Yes, the `db` is a `ref` type, but I still
      # get this error, so I'm using pointer hack here.

      proc auxField(field: DefField): DocEntryId =
        let head = field.head
        result = db[].newDocEntry(
          visitor = visitor,
          parent = objId,
          kind = ndkField,
          name = head.nameNode()
        )

        if not head.typ.isNil():
          db[].registerNestedDecls(head.typ, visitor.withUser(result))

        db[].updateCommon(result, head, visitor)
        db[][result].fieldType = head.typ


      proc auxField(field: DefField, visitor: DocVisitor): seq[DocEntryId] =
        case field.kind:
          of deffWrapCase:
            let resField = auxField(field)
            for branch in field.branches:
              db[][resField].switchesInto.add((branch.branchExprs, @[]))
              for subfield in branch.subfields:
                # `when` might have nore than one nested field returned
                # from aux, all other cases are covered by `subfields`
                # iteration above.
                for subId in auxField(subfield, visitor):
                  db[][resField].switchesInto[^1].subfields.add subId

            # `case` only yields a single toplevel field
            result.add resField

          of deffWrapWhen:
            # 'wrap when' does not have a direct field declared, instead
            # return all the nested fields directly
            for branch in field.branches:
              var visitor = visitor
              if branch.branchExprs.len > 0:
                visitor.declContext.whenConditions.add branch.branchExprs[0]

              for subfield in branch.subfields:
                result.add auxField(subfield, visitor)

          of deffPlain:
            result.add auxField(field)

      for field in decl.objFields:
        discard auxField(field, visitor)


    of deftEnum:
      result = db.newDocEntry(visitor, ndkEnum, decl.nameNode())
      # Calling 'updateCommon' can be done multiple times, and here we need
      # it in order to get correct visibility annotations on the enum
      # fields.
      updateCommon(db, result, decl, visitor)

      for field in decl.enumFields:
        let fId = db.newDocEntry(
          visitor = visitor,
          parent = result,
          kind = ndkEnumField,
          name = field.nameNode()
        )

        db.updateCommon(fId, field, visitor)
        db[fId].visibility = db[result].visibility

        if not field.strOverride.isNil():
          db[fId].enumStringOverride = some field.strOverride.getSName()

        if not field.valOverride.isNil():
          db[fId].enumValueOverride = some field.valOverride

    of deftAlias, deftDistinct:
      result = db.newDocEntry(
        visitor,
        db.classifyDeclKind(decl),
        decl.nameNode()
      )

      db[result].baseType = decl.baseType
      if decl.baseType in db:
        db[result].baseTypeId = some db[decl.baseType]

      db.registerNestedDecls(decl.baseType, visitor.withUser(result))

    of deftMagic:
      result = db.newDocEntry(
        visitor, ndkBuiltin, decl.nameNode())

    of deftProc, deftArg, deftLet, deftVar,
       deftConst, deftField, deftEnumField:
      assert false, $decl.kind & " is not a type definition"


proc registerDeclSection(
    db: var DocDb,
    visitor: DocVisitor,
    node: PNode,
    nodeKind: DocEntryKind = ndkGlobalVar
  ): seq[DocEntryId] =
  ## Register let, var or const declaration section

  case node.kind:
    of nkConstSection, nkVarSection, nkLetSection:
      let nodeKind =
        if db[visitor.parent].kind in ndkGlobalKinds:
          case node.kind:
            of nkConstSection: ndkGlobalConst
            of nkVarSection:  ndkGlobalVar
            else: ndkGlobalLet
        else:
          case node.kind:
            of nkConstSection: ndkConst
            of nkVarSection:  ndkVar
            else: ndkLet

      for subnode in node:
        result.add db.registerDeclSection(
          visitor, subnode, nodeKind)

    of nkConstDef, nkIdentDefs, nkVarTuple:
      result = db.registerIdentDefs(visitor, node, nodeKind)

    else:
      failNode node


proc registerGenericParams(db: var DocDb, def: DefTree, user: DocEntryId) =
  ## Store list of nested generic parameter declarations under `user`.
  for (name, constraint) in def.genericParams:
    discard db.newDocEntry(user, ndkParam, name)

proc registerEntryDef(
    db: var DocDb, visitor: DocVisitor, node: PNode): seq[DocEntryId] =
  ## Registers single toplevel entry definition, return all found
  ## definitions.
  case node.kind:
    of nkProcDeclKinds:
      let def = unparseDefs(node)[0]
      var doc = db.registerProcDef(visitor, def)
      db.updateCommon(doc, def, visitor)
      db.registerGenericParams(def, doc)
      result = @[doc]

    of nkTypeDef:
      let def = unparseDefs(node)[0]
      var doc = db.registerTypeDef(visitor, def)
      assert not doc.isNil()
      db.updateCommon(doc, def, visitor)
      db.registerGenericParams(def, doc)
      result = @[doc]

    of nkVarSection, nkLetSection, nkConstSection:
      result = db.registerDeclSection(visitor, node)

    else:
      failNode node


proc registerTopLevel(db: var DocDb, visitor: DocVisitor, node: PNode) =
  ## Register all toplevel definition entries, recursively visiting the
  ## `node`
  if db.isFromMacro(node):
    return

  case node.kind:
    of nkEntryDeclarationKinds:
      discard db.registerEntryDef(visitor, node)

    of nkStmtList:
      for subnode in node:
        db.registerTopLevel(visitor, subnode)

    of nkTypeSection:
      for typeDecl in node:
        registerTopLevel(db, visitor, typeDecl)

    of nkWhenStmt:
      for (visitor, body) in visitWhen(visitor, node):
        registerTopLevel(db, visitor, body)

    of nkIfStmt:
      for branch in node:
        registerTopLevel(db, visitor, branch[PosBody])

    else:
      discard

proc registerTopLevelDoc(
    db: var DocDb, module: DocEntryId, node: PNode, inModuleBody: bool)  =
  ## Register toplevel documentation block, either adding it as a part of
  ## documentation comment for the toplevel module (if not `inModuleBody`),
  ## or as a standalone toplevel documentation.
  if inModuleBody:
    let doc = db.newDocEntry(
      parent = module,
      kind = ndkComment,
      name = ""
    )

    db[doc].location = some db.add(node.nodeLocation())
    db[doc].docs.add db.addDocText(node, module)

  else:
    db[module].docs.add db.addDocText(node, module)

type
  DocPreContext* = ref object of TPassContext
    ## Initial documntation analysis context that constructs a list of potential
    ## documentable entries using pre-sem visitation.
    db*: DocDb
    graph*: ModuleGraph
    docModule*: DocEntryId ## Toplevel entry - module currently being
    ## processed

  DocContext* = ref object of PContext
    ## Documentation context object that is constructed for each open and close
    ## operation. This documentation further elaborates on analysis of the daa

    config*: ConfigRef
    db*: DocDb ## Documentation database that is persistent across all
    ## processing passes, for both pre-sem and in-sem visitation.
    docModule*: DocEntryId ## Toplevel entry - module currently being
    ## processed
    inModuleBody*: bool
    # Fields to track expansion context
    activeExpansion*: ExpansionId ## Id of the current active expansion.
    ## Points to valid one only if the expansion stack is not empty,
    ## otherwise stores the information about the last expansion.
    expansionStack*: seq[ExpansionId] ## Intermediate location for
    ## expansion data store - when expansion is closed it is moved to
    ## `db.expansion`
    toplevelExpansions*: seq[ExpansionId] ## List of toplevel macro or
    ## template expansions



proc initContextCommon(ctx: DocContext | DocPreContext, module: PSym) =
  let conf = ctx.graph.config
  var db = ctx.db
  if ctx.docModule.isNil():
    ctx.docModule = newDocEntry(db, ndkModule, module)
    db[ctx.docModule].visibility = dvkPublic

  db.fileModules[module.info.fileIndex] = ctx.docModule




proc `[]`*(ctx: DocContext, n: PType | PNode | PSym): DocEntryId = ctx.db[n]

func expandCtx(ctx: DocContext): int =
  ctx.expansionStack.len

func inExpansion(ctx: DocContext): bool =
  result = 0 < expandCtx(ctx)

func isSkip(context: PContext): bool =
  not context.config.isCompilerDebug()

proc preExpand(context: PContext, expr: PNode, sym: PSym) =
  ## Hook to be executed before macro expansion happens
  if context.isSkip(): return

  let ctx = DocContext(context)
  let active = ctx.db.expansions.add Expansion(
    expansionOf: sym,
    expandDepth: ctx.expansionStack.len,
    expandedFrom: expr.copyTree(),
  )

  ctx.activeExpansion = active
  if ctx.expansionStack.len == 0:
    # If there are no other active expansions, register current one as a
    # toplevel entry
    ctx.toplevelExpansions.add active

  else:
    ctx.db.expansions[ctx.expansionStack[^1]].nested.add active

  ctx.expansionStack.add active

proc postExpand(context: PContext, expr: PNode, sym: PSym) =
  ## Hook to execute after macro expansion happens
  if context.isSkip(): return
  let ctx = DocContext(context)
  let last = ctx.expansionStack.pop()
  ctx.db.expansions[last].resultNode = expr.copyTree()

  proc aux(n: PNode) =
    ctx.db.expandedNodes[n.id] = last
    if 0 < safeLen(n):
      for sub in items(n):
        aux(sub)

  if not ctx.inExpansion():
    aux(expr)

proc preResem(context: PContext, expr: PNode, sym: PSym) =
  ## Hook to execute before newly expanded macro result is passed into
  ## semantic checking layer.
  if context.isSkip(): return

  let ctx = DocContext(context)
  ctx.db.expansions[ctx.activeExpansion].immediateResult = expr.copyTree()

proc callHead(n: PNode): PNode =
  case n.kind:
    of nkIdent, nkSym:
      result = n

    of nkCall:
      result = callHead(n[0])

    else:
      failNode n

proc resolve(context: PContext, expr, call: PNode) =
  if context.isSkip(): return

  let ctx = DocContext(context)
  if not ctx.inExpansion(): return

  ctx.db.expansions[ctx.activeExpansion].resolveMap[
    callHead(expr).id] = callHead(call).sym

type
  DocBackend = ref object of RootObj
    ## Used to transfer persistent data (database) between open/close pairs
    ## for the documentation generation pass pass.
    db: DocDb


proc docPreSemOpen(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext {.nimcall.} =
  var db = DocBackend(graph.backend).db
  let pre = DocPreContext(graph: graph, db: db)
  pre.initContextCommon(module)
  return pre

proc docPreSemProcess(c: PPassContext, n: PNode): PNode {.nimcall.} =
  var ctx = DocPreContext(c)
  assert not ctx.db.isNil()
  var visitor = DocVisitor()
  visitor.docUser = ctx.docModule
  visitor.parent = ctx.docModule
  visitor.declContext.preSem = true

  # Perform initial registration of all the entries in the code -
  # this one excludes all macro-generated ones, but includes
  # conditionally enabled elements.
  registerTopLevel(ctx.db, visitor, n)

  registerPreUses(ctx.graph.config, ctx.db, visitor, n)

  result = n

proc docPreSemClose(graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.} =
  # Pre-sem documenter does not have any special 'close' actions for now
  discard

proc docInSemOpen(
    graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext {.nimcall.} =
  ## Create new instance of documentation generation context
  var back = DocBackend(graph.backend)
  var db = back.db

  var ctx = DocContext(
    db: db,
    config: graph.config,
    resolveHook:  SemResolveHook(resolve),
    expandHooks: (
      preMacro:         SemExpandHook(preExpand),
      preMacroResem:    SemExpandHook(preResem),
      postMacro:        SemExpandHook(postExpand),
      preTemplate:      SemExpandHook(preExpand),
      preTemplateResem: SemExpandHook(preResem),
      postTemplate:     SemExpandHook(postExpand)))
  ctx = DocContext(newContext(graph, module, ctx))

  ctx.initContextCommon(module)

  return semPassSetupOpen(ctx, graph, module, idgen)

proc docInSemProcess(c: PPassContext, n: PNode): PNode {.nimcall.} =
  result = semPassProcess(c, n)
  var ctx = DocContext(c)
  let conf = ctx.graph.config
  var db = ctx.db
  var visitor = DocVisitor()

  visitor.docUser = ctx.docModule
  visitor.parent = ctx.docModule
  visitor.activeModule = ctx.docModule

  assert not db.isNil()

  if isDocumentationNode(n):
    # Registering toplevel documentation elements for module only in
    # the post-sem analysis, since documentation is not subject to
    # conditional compile-time hiding (supposedly. And if it is then
    # it most likely is the author's intention. Althoug this
    # assumption can certainly be revised later on).
    registerTopLevelDoc(
      db, ctx.docModule, n, ctx.inModuleBody)

  elif n.kind == nkDiscardStmt:
    # Ignoring toplevel discard statements - the can be used as an
    # old-style comments, or be a testament spec (test files can also
    # have toplevel documentation)
    discard

  else:
    # Register toplevel declaration entries generated in the code,
    # excluding ones that were generated as a result of macro
    # expansions.
    registerTopLevel(db, visitor, result)
    ctx.inModuleBody = false
    # Register immediate uses of the macro expansions
    var state = initRegisterState()
    state.user = ctx.docModule
    state.moduleId = ctx.docModule
    registerUses(db, result, state)

proc docInSemClose(graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.} =
  result = semPassClose(graph, p, n)
  var ctx = DocContext(p)
  # Add information about known macro expansions that were processed
  # during compilation
  ctx.db.toplevelExpansions.add((ctx.docModule, ctx.toplevelExpansions))





proc setupDocPasses(graph: ModuleGraph): DocDb =
  ## Setup necessary documentation pass context for module graph.
  # Persistent data storage for all documentable entries. The data persists
  # across all open/close triggers.
  var db = DocDb()
  # implicitTReprConf.excl {
  #   trfShowSymFlags,
  #   trfShowSymId,
  #   trfShowSymName,
  #   trfShowSymOwner,
  #   trfShowNodeId
  #   trfShowNodeFlags,
  #   trfShowTypeOwner,
  #   trfShowNodeTypes,
  #   trfShowSymTypes,
  # }

  implicitTReprConf = onlyStructureTReprConf
  implicitTReprConf.incl {
    trfShowNodeLineInfo, trfPackedFields, trfShowSymPosition }

  setImplicitDebugConfRef graph.config
  if false:
    implicitTReprConf.extraSymInfo = proc(sym: PSym): ColText =
      result.add "sym location " & graph.config$sym.info
      result.add "\n"
      result.add "hashdata " & hashdata(sym) & "\n"

      if sym in db:
        result.add "db entry: "
        result.add db $ db[sym]

      else:
        result.add "[NO DB ENTRY]" + fgRed + styleReverse

    implicitTReprConf.extraNodeInfo = proc(node: PNode): ColText =
      result.add "node location " & graph.config$node.info
      if node.kind in {nkIdent, nkSym}:
        if node.approxLoc() in db.locationSigmap:
          result.add "\n"
          result.add "APPROX MAP: " + fgGreen
          result.add db$db.locationSigmap[node.approxLoc()]

        elif node.kind == nkSym:
          result.add ("\nNO APPROX LOC" + fgYellow) + styleReverse

  graph.backend = DocBackend(db: db)

  let preSemPass = makePass(
    TPassOpen(docPreSemOpen),
    TPassProcess(docPreSemProcess),
    TPassClose(docPreSemClose),
    isFrontend = true)

  let inSemPass = makePass(
    TPassOpen(docInSemOpen),
    TPassProcess(docInSemProcess),
    TPassClose(docInSemClose),
    isFrontend = true)


  case graph.config.docMode:
    of docDefault, docOnefile:
      registerPass(graph, inSemPass)

    of docUntyped:
      registerPass(graph, preSemPass)

    of docUntypedOnefile:
      # Documenting single file entry, without system module
      let
        conf = graph.config
        file = addFileExt(AbsoluteFile mainCommandArg(conf), NimExt)
        idx = conf.fileInfoIdx(file)

      let context = DocPreContext(
        graph: graph,
        db: db,
        docModule: db.newDocEntry(
          ndkFile, conf[idx].projPath.string)
      )

      for statement in parseString(
        readFile(file.string),
        cache = graph.cache,
        config = conf,
        filename = file.string
      ):
        discard docPreSemProcess(context, statement)

  return db



proc writeFlatDump*(conf: ConfigRef, db: DocDb) =
  var res = open("/tmp/res_dump", fmWrite)
  res.writeLine("DOCUMENTABLE ENTRIES:")
  for id, entry in db.entries:
    res.writeLine(db $ id)

  res.writeLine("OCCURENCIES:")
  for id, entry in db.occurencies:
    res.writeLine(db $ id)

  res.close()

const ndkOnlyNested* = {ndkArg, ndkInject}

proc writeEtags*(conf: ConfigRef, db: DocDb, file: AbsoluteFile) =
  var tagfile = open(file.string, fmWrite)
  # https://git.savannah.gnu.org/cgit/emacs.git/tree/etc/ETAGS.EBNF

  const
    FF = '\x0c'  # tag section starter
    LF = '\x0a'  # line terminator
    DEL = '\x7f' # pattern terminator
    SOH = '\x01' # name terminator

  var perFile: OrderedTable[FileIndex, DocEntrySet]
  for id, entry in db.entries:
    if entry.kind notin ndkOnlyNested and entry.location.isSome():
      perFile.mgetOrPut(
        db[entry.location.get()].file, DocEntrySet()).incl id

  for id, file in conf.m.fileInfos:
    # etags require actual parts of the source code to be present and
    # searchable, so calling `numLines` here to populate `.lines` data.
    discard conf.numLines(FileIndex(id))

  # `tagfile ::= { tagsection }` - etags file consists of multiple
  # (unlimited number of) repeating sections
  for file, ids in perFile:
    # Each tagsection is `tagsection ::= FF LF ( includesec | regularsec ) LF` -
    # in this case we are only writing `regularspec` for now.
    tagfile.write(FF, LF) # Common stat indicators
    # `regularsec ::= filename "," [ unsint ] [ LF fileprop ] { LF tag }`

    # `filename`
    tagfile.write(conf.toFilenameOption(file, foAbs), ",")
    for id in ids: # `{ LF tag }`
      tagfile.write(LF)
      # `tag ::= directtag | patterntag`
      # `patterntag ::= pattern DEL [ tagname SOH ] position`
      # `position ::= realposition | ","`
      # `realposition ::= "," unsint | unsint "," | unsint "," unsint`
      let loc = db[db[id].location.get()]
      tagfile.write(
        # `pattern DEl`. In that canse pattern is a piece of code that
        # emacs will progressively try to search in the target file, so
        # instead of trying to reconstruct all possible prefixes on all
        # possible indentation levels just reusing original source code
        # here.
        conf[loc.file].lines[loc.line - 1], DEL,
        # `tagname SOH` - tagname will displayed in the emacs, so rendering
        # actual signature here.
        db.fullName(id), SOH,
        # `position -> realposition -> unsint "," unsint`
        loc.line, ",", loc.column
      )

  close(tagfile)

import std/json

proc writeJsonLines*(conf: ConfigRef, db: DocDb) =
  var jfile = open("/tmp/jsontags.json", fmWrite)

  for id, entry in db.entries:
    var res = %*{
      "name": entry.name,
      "id": id.int,
      "kind": $entry.kind,
      "visibility": $entry.visibility
    }

    if db.hasDeprecatedMsg(id):
      res["deprecated"] = %db.getDeprecatedMsg(id)

    if entry.location.isSome():
      let loc = db[entry.location.get()]
      res["location"] = %*{
        "file": conf.toMsgFilename(loc.file),
        "line": $loc.line,
        "col": $loc.column
      }

    jfile.writeLine($res)

  close(jfile)

proc commandDoc3*(graph: ModuleGraph, ext: string) =
  ## Execute documentation generation command for module graph

  const str = """
type
  Enum = enum a, b

const s = {a, b}

type
  AddfFragment* = object
    case kind: Enum
      of s:
        f: int

"""

  # debug(graph.parseString(str), onlyStructureTReprConf + trfIndexVisisted)
  # debug(graph.compileString(str), onlyStructureTReprConf + trfIndexVisisted)
  # discard graph.compileString(str)

  # return

  var db = setupDocPasses(graph)
  compileProject(graph)
  unparseComments(db, graph.config)

  echo "Compiled documentation generator project"

  graph.config.writeFlatDump(db)
  echo "wrote list of tags to the /tmp/res_dump"

  if false:
    graph.config.writeJsonLines(db)
    echo "wrote json to the /tmp/jtags"

  let outSql = graph.config.projectFull.changeFileExt("sqlite")
  block:
    graph.config.writeSqlite(db, outSql)
    echo "wrote sqlite db to ", outSql.string

  block:
    var newConf = ConfigRef()
    var newDb = DocDb()
    readSqlite(newConf, newDb, outSql)
    echo "read sqlite from ", outSql.string

    let outSql2 = graph.config.projectFull.changeFileExt("sqlite2")
    graph.config.writeSqlite(newDb, outSql2)
    echo "wrote sqlite database back ", outSql2.string

  block:
    let outTags = graph.config.projectFull.changeFileExt("etags")
    graph.config.writeEtags(db, outTags)
    echo "wrote etags to the ", outTags.string
