#
#
#           The Nim Compiler
#        (c) Copyright 2021 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Dead code elimination (=DCE) for IC.
##
## For the lifetime-tracking hooks there exists a phase-ordering problem: we
## only know the set of used hooks after the MIR transformations are applied,
## but those are part of the code generation process and DCE is required to
## have happened prior.
##
## The problem is currently solved by conservatively marking the types of all
## alive location definitions (locals, globals, constants, etc.) plus the
## parameter and return types of alive routines as alive (and with them, their
## attached operators). This tries to catch all types of which
## ``injectdestructors`` could inject hooks.

import
  std/[
    intsets,
    tables
  ],
  compiler/ast/[
    ast,
    lineinfos
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    options
  ]

import packed_ast, ic, bitabs

type
  AliveSyms* = seq[IntSet]
  AliveContext* = object ## Purpose is to fill the 'alive' field.
    stack: seq[(int, TOptions, NodePos)] ## A stack for marking symbols as alive.
    decoder: PackedDecoder ## We need a PackedDecoder for module ID address translations.
    thisModule: int  ## The module we're currently analysing for DCE.
    alive: AliveSyms ## The final result of our computation.
    options: TOptions
    compilerProcs: Table[string, (int, int32)]

    graph: ModuleGraph ## only used for lookup of type-bound operators

proc isExportedToC(c: var AliveContext; g: PackedModuleGraph; symId: int32): bool =
  ## "Exported to C" procs are special (these are marked with '.exportc') because these
  ## must not be optimized away!
  let symPtr = unsafeAddr g[c.thisModule].fromDisk.syms[symId]
  let flags = symPtr.flags
  # due to a bug/limitation in the lambda lifting, unused inner procs
  # are not transformed correctly; issue (#411). However, the whole purpose here
  # is to eliminate unused procs. So there is no special logic required for this case.
  if sfCompileTime notin flags:
    if ({sfExportc, sfCompilerProc} * flags != {}) or
        (symPtr.kind == skMethod):
      result = true
      # XXX: This used to be a condition to:
      #  (sfExportc in prc.flags and exfExportLib in prc.extFlags) or
    if sfCompilerProc in flags:
      c.compilerProcs[g[c.thisModule].fromDisk.strings[symPtr.name]] = (c.thisModule, symId)

template isNotGeneric(n: NodePos): bool = ithSon(tree, n, genericParamsPos).kind == nkEmpty

proc aliveType(c: var AliveContext, g: PackedModuleGraph, module: int, typ: PackedItemId)

proc aliveRoutine(c: var AliveContext, g: PackedModuleGraph, module: int, item: int32) =
  ## Marks all types referenced directly in the routine's header as alive, which
  ## marks all type-bound operators attached to them as alive.
  let m = unsafeAddr g[module].fromDisk

  # don't scan magics that have non-concrete parameters:
  if m.syms[item].magic in {mEcho}:
    # XXX: no hooks should be lifted for these parameters in the first
    #      place...
    return

  # check the parameter types and the return type for lifetime hooks:
  let
    id = translateId(m.syms[item].typ, g, module, c.decoder.config)
    typ = unsafeAddr g[id.module].fromDisk.types[id.item]

  for it in typ.types.items:
    aliveType(c, g, module, it)

  if typ.callConv == ccClosure:
    # we also need to check the hidden parameter (if one exists):
    let
      body = m.syms[item].ast
      params = ithSon(m.bodies, NodePos body, paramsPos)
    aliveType(c, g, module, m.bodies[lastSon(m.bodies, params).int].typeId)

proc followLater(c: var AliveContext; g: PackedModuleGraph; module: int; item: int32) =
  ## Marks a symbol 'item' as used and later in 'followNow' the symbol's body will
  ## be analysed.
  if not c.alive[module].containsOrIncl(item):
    var body = g[module].fromDisk.syms[item].ast
    if body != emptyNodeId:
      let opt = g[module].fromDisk.syms[item].options
      if g[module].fromDisk.syms[item].kind in routineKinds:
        aliveRoutine(c, g, module, item)
        body = NodeId ithSon(g[module].fromDisk.bodies, NodePos body, bodyPos)
      c.stack.add((module, opt, NodePos(body)))

    # XXX: only perform this for *definitions*
    if g[module].fromDisk.syms[item].kind in {skLet, skVar, skConst, skForVar}:
      aliveType(c, g, module, g[module].fromDisk.syms[item].typ)

    when false:
      let nid = g[module].fromDisk.syms[item].name
      if nid != LitId(0):
        let name = g[module].fromDisk.strings[nid]
        if name in ["nimFrame", "callDepthLimitReached"]:
          echo "I was called! ", name, " body exists: ", body != emptyNodeId, " ", module, " ", item

proc loadSkippedType(t: PackedItemId; c: AliveContext; g: PackedModuleGraph;
                     toSkip: set[TTypeKind]): (TTypeKind, ItemId) =
  ## Returns the item ID and kind of `t` after skipping all types in the
  ## `toSkip` set.
  template kind(t: ItemId): TTypeKind =
    g[t.module].fromDisk.types[t.item].kind

  var
    t2 = translateId(t, g, c.thisModule, c.decoder.config)
    k = t2.kind

  while k in toSkip:
    t2 = translateId(g[t2.module].fromDisk.types[t2.item].types[^1], g,
                     t2.module, c.decoder.config)
    k = t2.kind

  result = (k, t2)

proc aliveType(c: var AliveContext, g: PackedModuleGraph, module: int, typ: PackedItemId) =
  ## Marks the type as alive, which currently means marking all non-trivial
  ## attached operators as alive.
  if typ == nilItemId:
    return

  let orig = c.thisModule
  c.thisModule = module
  # get the underlying concrete type:
  let (_, itemId) = loadSkippedType(typ, c, g, skipForHooks)
  c.thisModule = orig

  template load(op: TTypeAttachedOp) =
    c.graph.attachedOps[op].withValue(itemId, value):
      let m = moduleIndex(c.decoder, g, value.id.module, value.id.packed)
      followLater(c, g, m, value.id.packed.item)

  if tfHasAsgn in g[itemId.module].fromDisk.types[itemId.item].flags:
    # the type has a non-trivial destructor; mark the lifetime hooks as
    # alive
    for op in attachedDestructor..attachedTrace:
      load(op)

  # deepcopy can be present and non-trivial when the ``tfHasAsgn`` flag is not
  # present
  load(attachedDeepCopy)

proc requestCompilerProc(c: var AliveContext; g: PackedModuleGraph; name: string) =
  let (module, item) = c.compilerProcs[name]
  followLater(c, g, module, item)

proc loadTypeKind(t: PackedItemId; c: AliveContext; g: PackedModuleGraph; toSkip: set[TTypeKind]): TTypeKind =
  result = loadSkippedType(t, c, g, toSkip)[0]

proc rangeCheckAnalysis(c: var AliveContext; g: PackedModuleGraph; tree: PackedTree; n: NodePos) =
  ## Replicates the logic of `ccgexprs.genRangeChck`.
  ## XXX Refactor so that the duplicated logic is avoided. However, for now it's not clear
  ## the approach has enough merit.
  var dest = loadTypeKind(n.typ, c, g, abstractVar)
  if optRangeCheck notin c.options or dest in {tyUInt..tyUInt64}:
    discard "no need to generate a check because it was disabled"
  else:
    let n0t = loadTypeKind(n.firstSon.typ, c, g, {})
    if n0t in {tyUInt, tyUInt64}:
      c.requestCompilerProc(g, "raiseRangeErrorNoArgs")
    else:
      let raiser =
        case loadTypeKind(n.typ, c, g, abstractVarRange)
        of tyUInt..tyUInt64, tyChar: "raiseRangeErrorU"
        of tyFloat..tyFloat64: "raiseRangeErrorF"
        else: "raiseRangeErrorI"
      c.requestCompilerProc(g, raiser)

proc aliveCode(c: var AliveContext; g: PackedModuleGraph; tree: PackedTree; n: NodePos) =
  ## Marks the symbols we encounter when we traverse the AST at `tree[n]` as alive, unless
  ## it is purely in a declarative context (type section etc.).
  case n.kind
  of nkWithoutSons - nkSym:
    discard "ignore non-sym atoms"
  of nkSym:
    # This symbol is alive and everything its body references.
    followLater(c, g, c.thisModule, n.operand)
  of nkModuleRef:
    let (n1, n2) = sons2(tree, n)
    assert n1.kind == nkInt32Lit
    assert n2.kind == nkInt32Lit
    let m = n1.litId
    let item = n2.operand
    let otherModule = toFileIndexCached(c.decoder, g, c.thisModule, m).int
    followLater(c, g, otherModule, item)
  of nkMacroDef, nkTemplateDef, nkTypeSection, nkTypeOfExpr,
     nkIncludeStmt, nkImportStmt, nkImportExceptStmt,
     nkExportStmt, nkExportExceptStmt, nkFromStmt, nkStaticStmt:
    discard
  of nkVarSection, nkLetSection, nkConstSection:
    # XXX ignore the defining local variable name?
    for son in sonsReadonly(tree, n):
      aliveCode(c, g, tree, son)
  of nkChckRangeF, nkChckRange64, nkChckRange:
    rangeCheckAnalysis(c, g, tree, n)
  of nkProcDef, nkConverterDef, nkMethodDef, nkFuncDef, nkIteratorDef:
    if n.firstSon.kind == nkSym and isNotGeneric(n):
      let item = n.firstSon.operand
      if isExportedToC(c, g, item):
        # This symbol is alive and everything its body references.
        followLater(c, g, c.thisModule, item)
  else:
    for son in sonsReadonly(tree, n):
      aliveCode(c, g, tree, son)

proc followNow(c: var AliveContext; g: PackedModuleGraph) =
  ## Mark all entries in the stack. Marking can add more entries
  ## to the stack but eventually we have looked at every alive symbol.
  while c.stack.len > 0:
    let (modId, opt, ast) = c.stack.pop()
    c.thisModule = modId
    c.options = opt
    aliveCode(c, g, g[modId].fromDisk.bodies, ast)

proc computeAliveSyms*(g: PackedModuleGraph; graph: ModuleGraph,
                       conf: ConfigRef): AliveSyms =
  ## Entry point for our DCE algorithm.
  var c = AliveContext(stack: @[], decoder: PackedDecoder(config: conf),
                       thisModule: -1, alive: newSeq[IntSet](g.len),
                       options: conf.options, graph: graph)
  for i in countdown(high(g), 0):
    if g[i].status != undefined:
      c.thisModule = i
      for p in allNodes(g[i].fromDisk.topLevel):
        aliveCode(c, g, g[i].fromDisk.topLevel, p)

  followNow(c, g)
  result = move(c.alive)

proc isAlive*(a: AliveSyms; module: int, item: int32): bool =
  ## Backends use this to query if a symbol is `alive` which means
  ## we need to produce (C/JS/etc) code for it.
  result = a[module].contains(item)

