#
#
#           The Nim Compiler
#        (c) Copyright 2020 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Module that contains code to replay global VM state changes and pragma
## state like ``{.compile: "foo.c".}``. For IC (= Incremental compilation)
## support.

import
  compiler/ast/[
    ast,
    trees,
  ],
  compiler/modules/[
    modulegraphs,
  ],
  compiler/utils/[
    btrees,
    pathutils,
  ],
  compiler/front/[
    msgs,
    options,
  ],
  compiler/backend/[
    extccomp,
    cgmeth
  ]

from compiler/ast/reports_sem import reportStr
from compiler/ast/report_enums import ReportKind

import std/tables

import packed_ast, ic, bitabs

proc replayStateChanges*(module: PSym; g: ModuleGraph) =
  let list = module.ast
  assert list != nil
  assert list.kind == nkStmtList
  for n in list:
    assert n.kind == nkReplayAction
    # Fortunately only a tiny subset of the available pragmas need to
    # be replayed here. This is always a subset of ``pragmas.stmtPragmas``.
    if n.len >= 2:
      internalAssert g.config, n[0].kind == nkStrLit and n[1].kind == nkStrLit
      case n[0].strVal
      of "hint": localReport(g.config, n.info, reportStr(
        rsemUserHint, n[1].strVal))

      of "warning": localReport(g.config, n.info, reportStr(
        rsemUserWarning, n[1].strVal))

      of "error": localReport(g.config, n.info, reportStr(
        rsemUserError, n[1].strVal))

      of "compile":
        internalAssert g.config, n.len == 4 and n[2].kind == nkStrLit
        let cname = AbsoluteFile n[1].strVal
        var cf = Cfile(nimname: splitFile(cname).name, cname: cname,
                       obj: AbsoluteFile n[2].strVal,
                       flags: {CfileFlag.External},
                       customArgs: n[3].strVal)
        extccomp.addExternalFileToCompile(g.config, cf)
      of "link":
        extccomp.addExternalFileToLink(g.config, AbsoluteFile n[1].strVal)
      of "passl":
        extccomp.addLinkOption(g.config, n[1].strVal)
      of "passc":
        extccomp.addCompileOption(g.config, n[1].strVal)
      of "localpassc":
        extccomp.addLocalCompileOption(
          g.config, n[1].strVal, toFullPathConsiderDirty(g.config, module.info.fileIndex))
      of "cppdefine":
        options.cppDefine(g.config, n[1].strVal)
      of "inc":
        let destKey = n[1].strVal
        let by = n[2].intVal
        let v = getOrDefault(g.cacheCounters, destKey)
        g.cacheCounters[destKey] = v+by
      of "put":
        let destKey = n[1].strVal
        let key = n[2].strVal
        let val = n[3]
        if not contains(g.cacheTables, destKey):
          g.cacheTables[destKey] = initBTree[string, PNode]()
        if not contains(g.cacheTables[destKey], key):
          g.cacheTables[destKey].add(key, val)
        else:
          internalError(g.config, n.info, "key already exists: " & key)
      of "incl":
        let destKey = n[1].strVal
        let val = n[2]
        if not contains(g.cacheSeqs, destKey):
          g.cacheSeqs[destKey] = newTree(nkStmtList, val)
        else:
          block search:
            for existing in g.cacheSeqs[destKey]:
              if exprStructuralEquivalent(existing, val, strictSymEquality = true):
                # comment equality DOES matter here
                break search
            g.cacheSeqs[destKey].add val
      of "add":
        let destKey = n[1].strVal
        let val = n[2]
        if not contains(g.cacheSeqs, destKey):
          g.cacheSeqs[destKey] = newTree(nkStmtList, val)
        else:
          g.cacheSeqs[destKey].add val
      else:
        internalAssert g.config, false

proc replayBackendRoutines*(g: ModuleGraph, module: int) =
  ## Applies changes to whole-program (``ModuleGraph``) state the module
  ## `module` has. Only changes relevant to the backend part of the compiler
  ## are included here.
  for it in mitems(g.packed[module].fromDisk.enumToStringProcs):
    let key = translateId(it[0], g.packed, module, g.config)
    g.enumToStringProcs[key] =
      LazySym(id: FullId(module: module, packed: it[1]), sym: nil)

  for it in mitems(g.packed[module].fromDisk.attachedOps):
    let
      key = translateId(it[1], g.packed, module, g.config)
      symId = FullId(module: module, packed: it[2])
    g.attachedOps[it[0]][key] = LazySym(id: symId, sym: nil)

proc replayGenericCacheInformation*(g: ModuleGraph; module: int) =
  ## We remember the generic instantiations a module performed
  ## in order to to avoid the code bloat that generic code tends
  ## to imply. This is cheaper than deduplication of identical
  ## generic instantiations. However, deduplication is more
  ## powerful and general and I hope to implement it soon too
  ## (famous last words).
  assert g.packed[module].status == loaded
  for it in g.packed[module].fromDisk.typeInstCache:
    let key = translateId(it[0], g.packed, module, g.config)
    g.typeInstCache.mgetOrPut(key, @[]).add LazyType(id: FullId(module: module, packed: it[1]), typ: nil)

  for it in mitems(g.packed[module].fromDisk.procInstCache):
    let key = translateId(it.key, g.packed, module, g.config)
    var concreteTypes = newSeq[FullId](it.concreteTypes.len)
    for i in 0..high(it.concreteTypes):
      concreteTypes[i] = FullId(module: module, packed: it.concreteTypes[i])

    g.procInstCache.mgetOrPut(key, @[]).add LazyInstantiation(
      module: module, sym: FullId(module: module, packed: it.sym),
      concreteTypes: concreteTypes, inst: nil)

  for it in mitems(g.packed[module].fromDisk.methodsPerType):
    let key = translateId(it[0], g.packed, module, g.config)
    let col = it[1]
    let symId = FullId(module: module, packed: it[2])
    g.methodsPerType.mgetOrPut(key, @[]).add (col, LazySym(id: symId, sym: nil))

  for it in mitems(g.packed[module].fromDisk.enumToStringProcs):
    let key = translateId(it[0], g.packed, module, g.config)
    g.enumToStringProcs[key] =
      LazySym(id: FullId(module: module, packed: it[1]), sym: nil)

  replayBackendRoutines(g, module)

  for it in mitems(g.packed[module].fromDisk.methods):
    let sym = loadSymFromId(g.config, g.cache, g.packed, module,
                            PackedItemId(module: LitId(0), item: it))
    methodDef(g, g.idgen, sym)

  when false:
    # not used anymore:
    for it in mitems(g.packed[module].fromDisk.compilerProcs):
      let symId = FullId(module: module, packed: PackedItemId(module: LitId(0), item: it[1]))
      g.lazyCompilerprocs[g.packed[module].fromDisk.sh.strings[it[0]]] = symId

  for it in mitems(g.packed[module].fromDisk.converters):
    let symId = FullId(module: module, packed: PackedItemId(module: LitId(0), item: it))
    g.ifaces[module].converters.add LazySym(id: symId, sym: nil)

  for it in mitems(g.packed[module].fromDisk.trmacros):
    let symId = FullId(module: module, packed: PackedItemId(module: LitId(0), item: it))
    g.ifaces[module].patterns.add LazySym(id: symId, sym: nil)

  for it in mitems(g.packed[module].fromDisk.pureEnums):
    let symId = FullId(module: module, packed: PackedItemId(module: LitId(0), item: it))
    g.ifaces[module].pureEnums.add LazySym(id: symId, sym: nil)

proc replayLibs*(g: ModuleGraph, module: int) =
  ## Loads the packed library information for `module` into `g`.
  # XXX: libs are not really replayed state changes...
  if module >= g.libs.len:
    g.libs.setLen(module + 1)

  g.libs[module] = loadLibs(g.config, g.cache, g.packed, module)
