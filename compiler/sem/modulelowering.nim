## Implements the lowering of modules into procedures and objects. The AST
## of a module is separated into *declarative* and *imperative* nodes:
## both are gathered into separate statement lists, with the *imperative*
## one then being transformed into a procedure (the module's ``init``
## procedure).
##
## For integration with the ``passes`` interface, a simple adapter pass is
## provided. It sets up and populates a ``ModuleList`` instance, which can
## then be retrieved from the ``ModuleGraph`` via ``takeModuleList`` after
## all passes have run.
##
## The collection part is somewhat similar to the rodfile-based IC backend, but
## instead of reading the modules' content from the rodfiles, it's collected
## via the pass interface.

import
  compiler/ast/[
    ast,
    ast_idgen,
    ast_types,
    ast_query,
    lineinfos,
    idents
  ],
  compiler/front/[
    options
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    passes
  ],
  compiler/utils/[
    containers,
    idioms
  ]

from compiler/mir/injecthooks import getOp

type
  ModuleStructs* = object
    globals*: seq[PSym]
      ## all top-level globals part of the module

    nestedGlobals*: seq[PSym]
      ## all globals defined at the module *level* but not in the outermost
      ## *scope*. Ideally, these would be locals instead

    threadvars*: seq[PSym]
      ## all thread-local variables part of the module

  Module* = object
    ## Represents the contents of a fully analysed module, intended for use by
    ## the compiler backend.
    sym*: PSym          ## module symbol
    idgen*: IdGenerator ## the ID generator associated with the module

    decls*: PNode
      ## all declarative statements (type, routine, and constant
      ## definitions)
    structs*: ModuleStructs
      ## the contents of the module's structs

    # the module-bound procedures. Conceptually, these operate on the module's
    # structs
    dataInit*: PSym
      ## the procedure responsible for initializing data associated with the
      ## module. This is data that must be initialized at run-time but does
      ## not depend on user-defined globals
    init*: PSym
      ## the procedure responsible for initializing the module's globals
    destructor*: PSym
      ## the procedure responsible for de-initializing the module's
      ## globals
    threadDestructor*: PSym
      ## the procedure responsible for de-initializing the module's
      ## thread-local variables

    # XXX: the design around the pre-init and post-destructor procedure is
    #      likely not final yet. At the moment, we set them up here so that
    #      the backend processing has a symbol to attach code to
    preInit*: PSym
      ## the procedure for initializing the module's lifted globals
    postDestructor*: PSym
      ## the procedure for destroying the module's lifted globals
    threadPostDestructor*: PSym
      ## the procedure for destroying the module's lifted threadvars
    dynlibInit*: PSym
      ## the procedure for loading the dynamic libraries, procedure, and
      ## variables associated with the module
    # finalizing dynamic libs is left to the OS

  ModuleList* = object
    modules*: SeqMap[FileIndex, Module]
    modulesClosed*: seq[FileIndex]
      ## stores the modules in the order they were closed. The first closed
      ## module comes first, then the next, etc.

    systemPos, mainPos: FileIndex

  ModuleListBackend = ref object of RootObj
    ## Adapter type required for storing a ``ModuleList`` in the
    ## ``ModuleGraph.backend`` field.
    modules: ModuleList

  CollectPassCtx = ref object of TPassContext
    ## Represents a module during the "collect" pass, and is populated as part
    ## of it. Turned into a ``Module`` instance once the module is closed.
    module: PSym

    decls: seq[PNode]      ## all declarative statements
    imperative: seq[PNode] ## all imperative statements

func isFilled*(m: Module): bool =
  # required so that ``Module`` is usable as the item type of a ``SeqMap``
  m.sym != nil

template `[]`*(list: ModuleList, i: FileIndex): Module =
  list.modules[i]

func systemModule*(modules: ModuleList): lent Module =
  modules[modules.systemPos]

func mainModule*(modules: ModuleList): lent Module =
  modules[modules.mainPos]

iterator closed*(modules: ModuleList): lent Module =
  ## Convenience iterator for returning all modules that need to be passed
  ## to code generation, in the order they were closed.
  for index in modules.modulesClosed.items:
    yield modules[index]

iterator rclosed*(modules: ModuleList): lent Module =
  ## Convenience iterator for returning all modules that need to be passed
  ## to code generation, in the *reverse* order they were closed.
  for i in countdown(modules.modulesClosed.high, 0):
    yield modules[modules.modulesClosed[i]]

proc takeModuleList*(graph: ModuleGraph): ModuleList =
  ## Moves the ``ModuleList`` set up by the collector pass out of the
  ## `graph.backend` field and returns it.
  result = move ModuleListBackend(graph.backend).modules
  graph.backend = nil

proc group(n: PNode, decl, imperative: var seq[PNode]) =
  case n.kind
  of nkCommentStmt, nkIncludeStmt, nkImportStmt, nkImportExceptStmt,
     nkExportStmt, nkExportExceptStmt, nkFromStmt, nkMixinStmt, nkBindStmt,
     nkStaticStmt:
    # declarative statements not relevant to the backends -- they're dropped
    discard
  of nkTemplateDef, nkMacroDef:
    # not relevant to the backend
    discard
  of nkEmpty, nkError:
    # errors were already reported earlier
    discard "drop errors and empty nodes"
  of nkStmtList:
    # flatten statement lists
    for it in n.items:
      group(it, decl, imperative)
  of nkTypeSection, nkConstSection, declarativeDefs:
    decl.add(n)
  else:
    # everything else is treated as imperative statements. Until it's clearer
    # what to do with them, this also includes declarative statements part
    # of nested scopes (those inside ``if``, ``block``, etc. statements)
    imperative.add(n)

proc createModuleOp(graph: ModuleGraph, idgen: IdGenerator, postfix: string,
                    module: PSym, body: PNode, options: TOptions): PSym =
  ## Creates the symbol for a module-bound operator. Note that this attachment
  ## is purely at the conceptional level at the moment.
  result = newSym(skProc, getIdent(graph.cache, module.name.s & postfix),
                  nextSymId idgen, module, module.info, options)
  # the procedure doesn't return anything and doesn't have parameters:
  result.typ = newProcType(module.info, nextTypeId idgen, module)
  # conservatively enable destructor injection:
  result.flags = {sfInjectDestructors}

  # also set up a proper definition AST:
  result.ast = newProcNode(nkProcDef, module.info, body,
                           params        = newTree(nkFormalParams, [graph.emptyNode]),
                           name          = newSymNode(result),
                           pattern       = graph.emptyNode,
                           genericParams = graph.emptyNode,
                           pragmas       = graph.emptyNode,
                           exceptions    = graph.emptyNode)

proc registerGlobals(stmts: seq[PNode], structs: var ModuleStructs) =
  ## Create an entry in `structs` for each global or threadvar defined at the
  ## the module level (within the module imperative body `stmts`).

  proc register(structs: var ModuleStructs, s: PSym, isTopLevel: bool) {.nimcall.} =
    if {sfCompileTime, sfImportc} * s.flags != {}:
      # don't register compile-time or imported globals with the module struct
      discard
    elif s.kind == skTemp:
      # HACK: semantic analysis sometimes produces temporaries (it does so for
      #       ``(a, b) = c``, for example) that are also globals. These aren't
      #       really globals, and we don't want to "lift" them into the module
      #       struct, so we ignore them here and pass them on to ``mirgen``
      discard
    elif sfPure in s.flags:
      # if it's a global or threadvar explicitly marked with ``.global``, then
      # we treat it like a *lifted global* and don't add it to the module
      # struct
      discard
    elif sfThread in s.flags:
      structs.threadvars.add s
    elif sfGlobal in s.flags:
      if isTopLevel:
        # a "proper" global
        structs.globals.add s
      else:
        # a global not defined in the outermost scope; those require
        # special handling, and thus use a separate list
        structs.nestedGlobals.add s
    else:
      discard

  proc scan(n: PNode, structs: var ModuleStructs) {.nimcall.} =
    ## Doesn't modify the AST, and only registers the globals for which
    ## definitions are found with `structs`. This is a work-around
    ## required for the globals not defined in the outermost *scope*;
    ## their definitions must be kept until after the ``injectdestructors``
    ## pass is done.
    # XXX: make ``scan`` obsolete as soon as possible
    template register(it: PNode) =
      if it.kind == nkSym:
        register(structs, it.sym, isTopLevel = false)

    case n.kind
    of nkIdentDefs, nkVarTuple:
      for it in names(n):
        register(it)

      # don't scan the type slot
      scan(n[^1], structs)
    of nkForStmt:
      # 'for' statements don't use identdefs...
      for it in names(n):
        case it.kind
        of nkSym:
          register(it)
        of nkVarTuple:
          # not a normal var tuple...
          for i in 0..<it.len-1:
            register(it[i])
        else:
          unreachable()

      scan(n[^2], structs)
      scan(n[^1], structs)
    of nkConv, nkCast, nkHiddenStdConv, nkHiddenSubConv:
      scan(n[1], structs)
    of nkWithoutSons, callableDefs, nkTypeSection, nkConstSection, nkConstDef:
      discard "ignore"
    else:
      for it in n.items:
        scan(it, structs)

  proc processVarSection(n: PNode, structs: var ModuleStructs) {.nimcall.} =
    ## Processes a var/let section appearing in a module's outermost scope.
    assert n.kind in {nkVarSection, nkLetSection}
    for it in n.items:
      case it.kind
      of nkCommentStmt:
        discard "ignore"
      of nkIdentDefs, nkVarTuple:
        var isCompileTime = false # crude detection for .compileTime globals
        for name in names(it):
          if name.kind == nkSym:
            register(structs, name.sym, true)
            isCompileTime = isCompileTime or sfCompileTime in name.sym.flags

        if not isCompileTime:
          # we still need to scan the right-hand side
          scan(it[^1], structs)

      else:
        unreachable()

  proc process(n: PNode, structs: var ModuleStructs) =
    case n.kind
    of nkVarSection, nkLetSection:
      processVarSection(n, structs)
    of nkStmtList:
      unreachable("top-level statement lists must not exist at this point")
    else:
      scan(n, structs)

  for it in stmts.items:
    process(it, structs)


proc genDestroy(graph: ModuleGraph, dest: PNode): PNode =
  ## Constructs and returns the AST representing a call to the
  ## ``=destroy`` hook for `dest`.
  let
    op = getOp(graph, dest.typ, attachedDestructor)
    addrExp = newTreeIT(nkHiddenAddr, dest.info, op.typ[1]): dest

  result = newTreeI(nkCall, dest.info, newSymNode(op), addrExp)

proc generateDestructor(graph: ModuleGraph, vars: openArray[PSym]): PNode =
  ## Generates the body for a module destructor (also referred to as the
  ## 'de-init' procedure). A destructor call for each entitiy in `vars` is
  ## emitted, in reverse order of appearance.
  result = newNode(nkStmtList)
  for i in countdown(vars.high, 0):
    let s = vars[i]
    if hasDestructor(s.typ):
      result.add genDestroy(graph, newSymNode(s))

  if result.len == 0:
    # collapse to an empty node (dead-code elimination treats procedures with
    # ``nkEmpty`` nodes as the body as empty)
    result = newNode(nkEmpty)

proc changeOwner(n: PNode, newOwner: PSym) =
  ## For all symbols defined in the AST `n`, changes the owner to
  ## `newOwner`.
  template change(it: PNode) =
    # make sure to not change the owner of non-pure globals. Pure globals (i.e.
    # those explicitly marked with the ``.global`` pragma) are reparented, so
    # that they're later treated as lifted globals. The cumbersome check is
    # required because a procedure symbol can also have both the ``sfGlobal``
    # *and* ``sfPure`` flag set
    # XXX: we also have to include ``skTemp`` in the set here, as temporaries
    #      would otherwise be extracted by the later called ``extractGlobals``,
    #      and that must not happen
    if it.kind == nkSym and (it.sym.kind notin {skVar, skLet, skForVar, skTemp} or
       {sfGlobal, sfPure} * it.sym.flags != {sfGlobal}):
      it.sym.owner = newOwner

  case n.kind
  of nkIdentDefs, nkVarTuple, nkConstDef:
    for it in names(n):
      change(it)

    # ignore the type slot here. If it contains definitions, so be
    # it -- changing their owner shouldn't cause any problems
    changeOwner(n[^1], newOwner)
  of routineDefs - nkIteratorDef:
    change(n[namePos])
  of nkLambdaKinds:
    change(n[namePos])
  of nkForStmt:
    # 'for' statements don't use identdefs...
    for it in names(n):
      case it.kind
      of nkSym:
        change(it)
      of nkVarTuple:
        # not a normal var tuple...
        for i in 0..<it.len-1:
          change(it[i])
      else:
        unreachable()

    changeOwner(n[^2], newOwner)
    changeOwner(n[^1], newOwner)
  of nkIteratorDef:
    # we special case iterator definitions here, and leave them as direct
    # descendants of the module. This is currently done for efficieny
    # reasons: without, a top-level closure iterator creating an instance of
    # another closure iterator would lead to the unnecessary allocation of
    # an empty environment that lives for the duration of the module-init
    # procedure.
    # XXX: ^^ this is limitation of the lambda-lifting pass
    discard
  of nkWithoutSons, nkTypeSection:
    discard "ignore"
  of nkWithSons - entityDefs - nkTypeSection:
    for it in n.items:
      changeOwner(it, newOwner)

proc setupModule*(graph: ModuleGraph, idgen: IdGenerator, m: PSym,
                  decls, imperative: seq[PNode]): Module =
  ## Creates a ``Module`` instance from `decls` and `imperative`. The module
  ## structs are populated with the initial items (top-level globals defined
  ## in the outermost scope, and threadvars) and the module-bound operators
  ## are set up.
  result = Module(sym: m, idgen: idgen)

  result.decls =
    if decls.len == 0: newNodeI(nkEmpty, m.info)
    else:              newTreeI(nkStmtList, m.info, decls)

  # gather the top-level globals and threadvars:
  registerGlobals(imperative, result.structs)

  # turn the list into a node:
  let imperative =
    if imperative.len == 0: newNodeI(nkEmpty, m.info)
    else:                   newTreeI(nkStmtList, m.info, imperative)

  # FIXME: ``when nimvm`` statements aren't collapsed at this point, and
  #        ``extractGlobals`` doesn't consider them. This means that the
  #        definitions from both branches are registered with the module
  #        struct

  var options = graph.config.options
  if sfSystemModule in m.flags:
    # don't generate stack traces for code in the ``system`` module
    options.excl optStackTrace

  # now that we have the code that makes up the user-defined module
  # initialization, we wrap it into a procedure:
  result.init = createModuleOp(graph, idgen, "", m, imperative, options)
  result.init.flags = m.flags * {sfInjectDestructors} # inherit the flag
  # we also need to make sure that the owner of all entities defined inside
  # the body is adjusted:
  changeOwner(result.init.ast[bodyPos], result.init)

  # create a procedure for the data-init operator already. The selected backend
  # is then responsible for filling it with content
  result.dataInit = createModuleOp(graph, idgen, "DatInit", m, newNode(nkEmpty), options)

  # setup the module struct clean-up operator:
  result.destructor =
    createModuleOp(graph, idgen, "Deinit", m,
                   generateDestructor(graph, result.structs.globals),
                   options)

  # setup the per-thread module struct clean-up operator:
  result.threadDestructor =
    createModuleOp(graph, idgen, "ThreadDeinit", m,
                   generateDestructor(graph, result.structs.threadvars),
                   options)

  result.preInit = createModuleOp(graph, idgen, "PreInit", m, newNode(nkEmpty), options)
  result.postDestructor = createModuleOp(graph, idgen, "PostDeinit", m, newNode(nkEmpty), options)
  result.threadPostDestructor =
    createModuleOp(graph, idgen, "ThreadPostDeinit", m, newNode(nkEmpty),
                   options)
  result.dynlibInit = createModuleOp(graph, idgen, "DynlibInit", m, newNode(nkEmpty), options)

# Below is the `passes` interface implementation

proc myOpen(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext =
  result = CollectPassCtx(idgen: idgen, module: module)

proc myProcess(b: PPassContext, n: PNode): PNode =
  result = n

  let c = CollectPassCtx(b)
  group(n, c.decls, c.imperative)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  result = myProcess(b, n)
  if graph.config.errorCounter != 0:
    # a user error occured somewhere earlier. Code generation is now disabled
    # anyway, so skip the processing
    return

  if graph.backend == nil:
    graph.backend = ModuleListBackend()

  template list: ModuleList = ModuleListBackend(graph.backend).modules

  let
    c = CollectPassCtx(b)
    pos = c.module.position.FileIndex

  list.modules[pos] = setupModule(graph, c.idgen, c.module, c.decls,
                                  c.imperative)
  list.modulesClosed.add(pos)

  # remember the positions of important modules:
  if sfSystemModule in c.module.flags:
    list.systemPos = pos
  elif sfMainModule in c.module.flags:
    list.mainPos = pos

const collectPass* = makePass(myOpen, myProcess, myClose)
