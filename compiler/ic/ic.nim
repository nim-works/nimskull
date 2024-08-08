#
#
#           The Nim Compiler
#        (c) Copyright 2020 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import std/[hashes, tables, intsets, sha1]

import
  compiler/ic/[
    packed_ast,
    bitabs,
    rodfiles
  ],
  compiler/ast/[
    ast,
    idents,
    lineinfos,
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/utils/[
    ropes,
    pathutils
  ]

# TODO: at least switch to internalAssert/Error or better have its own
#       diag/event/telemetry types
from compiler/ast/reports_internal import InternalReport
from compiler/ast/report_enums import ReportKind

from std/os import removeFile, isAbsolute

type
  PackedConfig* = object
    backend: TBackend
    selectedGC: TGCMode
    cCompiler: TSystemCC
    options: TOptions
    globalOptions: TGlobalOptions

  ModuleBackendFlag* = enum
    HasDatInitProc
    HasModuleInitProc

  PackedModule* = object ## the parts of a PackedEncoder that are part of the .rod file
    definedSymbols: string
    moduleFlags: TSymFlags
    includes*: seq[(LitId, string)] ## first entry is the module filename itself
    imports: seq[LitId] ## the modules this module depends on
    toReplay*: PackedTree ## pragmas and VM specific state to replay.
    topLevel*: PackedTree  ## top level statements
    bodies*: PackedTree ## other trees. Referenced from typ.n and sym.ast by their position.
    #producedGenerics*: Table[GenericKey, SymId]
    exports*: seq[(LitId, int32)]
    hidden*: seq[(LitId, int32)]
    reexports*: seq[(LitId, PackedItemId)]
    compilerProcs*: seq[(LitId, int32)]
    converters*, methods*, trmacros*, pureEnums*: seq[int32]
    macroUsages*: seq[(PackedItemId, PackedLineInfo)]

    typeInstCache*: seq[(PackedItemId, PackedItemId)]
    procInstCache*: seq[PackedInstantiation]
    attachedOps*: seq[(TTypeAttachedOp, PackedItemId, PackedItemId)]
    methodsPerType*: seq[(PackedItemId, int, PackedItemId)]
    enumToStringProcs*: seq[(PackedItemId, PackedItemId)]
    libs*: seq[PackedLib]

    emittedTypeInfo*: seq[string]
    backendFlags*: set[ModuleBackendFlag]

    syms*: seq[PackedSym]
    types*: seq[PackedType]
    strings*: BiTable[string] ## we could share these between modules.
    numbers*: BiTable[BiggestInt] ## we also store floats in here so
                                  ## that we can assure that every bit is kept

    cfg: PackedConfig

  PackedEncoder* = object
    #m*: PackedModule
    thisModule*: int32
    lastFile*: FileIndex ## remember the last lookup entry.
    lastLit*: LitId
    filenames*: Table[FileIndex, LitId]
    pendingTypes*: seq[PType]
    pendingSyms*: seq[PSym]
    typeMarker*: IntSet #Table[ItemId, TypeId]  # ItemId.item -> TypeId
    symMarker*: IntSet #Table[ItemId, SymId]    # ItemId.item -> SymId
    config*: ConfigRef

proc toString*(tree: PackedTree; n: NodePos; m: PackedModule; nesting: int;
               result: var string) =
  let pos = n.int
  if result.len > 0 and result[^1] notin {' ', '\n'}:
    result.add ' '

  result.add $tree[pos].kind
  case tree.nodes[pos].kind
  of nkError, nkEmpty, nkNilLit, nkType, nkCommentStmt: discard
  of nkIdent, nkStrLiterals:
    result.add " "
    result.add m.strings[LitId tree.nodes[pos].operand]
  of nkSym:
    result.add " "
    result.add m.strings[m.syms[tree.nodes[pos].operand].name]
  of directIntLit:
    result.add " "
    result.addInt tree.nodes[pos].operand
  of externSIntLit:
    result.add " "
    result.addInt m.numbers[LitId tree.nodes[pos].operand]
  of externUIntLit:
    result.add " "
    result.addInt cast[uint64](m.numbers[LitId tree.nodes[pos].operand])
  of nkFloatLiterals:
    result.add " "
    result.addFloat cast[BiggestFloat](m.numbers[LitId tree.nodes[pos].operand])
  of nkWithSons:
    result.add "(\n"
    for i in 1..(nesting+1)*2: result.add ' '
    for child in sonsReadonly(tree, n):
      toString(tree, child, m, nesting + 1, result)
    result.add "\n"
    for i in 1..nesting*2: result.add ' '
    result.add ")"
    #for i in 1..nesting*2: result.add ' '

proc toString*(tree: PackedTree; n: NodePos; m: PackedModule): string =
  result = ""
  toString(tree, n, m, 0, result)

proc debug*(tree: PackedTree; m: PackedModule) =
  stdout.write toString(tree, NodePos 0, m)

proc isActive*(e: PackedEncoder): bool = e.config != nil
proc disable(e: var PackedEncoder) = e.config = nil

template primConfigFields(fn: untyped) {.dirty.} =
  fn backend
  fn selectedGC
  fn cCompiler
  fn options
  fn globalOptions

proc definedSymbolsAsString(config: ConfigRef): string =
  result = newStringOfCap(200)
  result.add "config"
  for d in definedSymbolNames(config):
    result.add ' '
    result.add d

proc rememberConfig(c: var PackedEncoder; m: var PackedModule; config: ConfigRef; pc: PackedConfig) =
  m.definedSymbols = definedSymbolsAsString(config)
  #template rem(x) =
  #  c.m.cfg.x = config.x
  #primConfigFields rem
  m.cfg = pc

const
  debugConfigDiff = defined(debugConfigDiff)

when debugConfigDiff:
  import hashes, tables, intsets, sha1, strutils, sets

proc configIdentical(m: PackedModule; config: ConfigRef): bool =
  result = m.definedSymbols == definedSymbolsAsString(config)
  when debugConfigDiff:
    if not result:
      var wordsA = m.definedSymbols.split(Whitespace).toHashSet()
      var wordsB = definedSymbolsAsString(config).split(Whitespace).toHashSet()
      for c in wordsA - wordsB:
        echo "in A but not in B ", c
      for c in wordsB - wordsA:
        echo "in B but not in A ", c
  template eq(x) =
    result = result and m.cfg.x == config.x
    when debugConfigDiff:
      if m.cfg.x != config.x:
        echo "B ", m.cfg.x, " ", config.x
  primConfigFields eq

proc rememberStartupConfig*(dest: var PackedConfig, config: ConfigRef) =
  template rem(x) =
    dest.x = config.x
  primConfigFields rem
  dest.globalOptions.excl optForceFullMake

proc hashFileCached(conf: ConfigRef; fileIdx: FileIndex): string =
  result = msgs.getHash(conf, fileIdx)
  if result.len == 0:
    let fullpath = msgs.toFullPath(conf, fileIdx)
    result = $secureHashFile(fullpath)
    msgs.setHash(conf, fileIdx, result)

proc toLitId(x: string; m: var PackedModule): LitId {.inline.} =
  ## store a string as a literal
  result = getOrIncl(m.strings, x)

proc toLitId(x: FileIndex; c: var PackedEncoder; m: var PackedModule): LitId =
  ## store a file index as a literal
  if x == c.lastFile:
    result = c.lastLit
  else:
    result = c.filenames.getOrDefault(x)
    if result == LitId(0):
      let p = msgs.toFullPath(c.config, x)
      result = toLitId(p, m)
      c.filenames[x] = result
    c.lastFile = x
    c.lastLit = result
  assert result != LitId(0)

proc toFileIndex*(x: LitId; m: PackedModule; config: ConfigRef): FileIndex =
  result = options.fileInfoIdx(config, AbsoluteFile m.strings[x])

proc includesIdentical(m: var PackedModule; config: ConfigRef): bool =
  for it in mitems(m.includes):
    if hashFileCached(config, toFileIndex(it[0], m, config)) != it[1]:
      return false
  result = true

proc initEncoder*(c: var PackedEncoder; m: var PackedModule; moduleSym: PSym; config: ConfigRef; pc: PackedConfig) =
  ## setup a context for serializing to packed ast
  c.thisModule = moduleSym.itemId.module
  c.config = config
  m.moduleFlags = moduleSym.flags
  m.bodies = newTreeFrom(m.topLevel)
  m.toReplay = newTreeFrom(m.topLevel)

  c.lastFile = FileIndex(-10)

  let thisNimFile = FileIndex c.thisModule
  var h = msgs.getHash(config, thisNimFile)
  if h.len == 0:
    let fullpath = msgs.toFullPath(config, thisNimFile)
    if isAbsolute(fullpath):
      # For NimScript compiler API support the main Nim file might be from a stream.
      h = $secureHashFile(fullpath)
      msgs.setHash(config, thisNimFile, h)
  m.includes.add((toLitId(thisNimFile, c, m), h)) # the module itself

  rememberConfig(c, m, config, pc)

proc addIncludeFileDep*(c: var PackedEncoder; m: var PackedModule; f: FileIndex) =
  m.includes.add((toLitId(f, c, m), hashFileCached(c.config, f)))

proc addImportFileDep*(c: var PackedEncoder; m: var PackedModule; f: FileIndex) =
  m.imports.add toLitId(f, c, m)

proc addHidden*(c: var PackedEncoder; m: var PackedModule; s: PSym) =
  let nameId = getOrIncl(m.strings, s.name.s)
  m.hidden.add((nameId, s.itemId.item))

proc addExported*(c: var PackedEncoder; m: var PackedModule; s: PSym) =
  let nameId = getOrIncl(m.strings, s.name.s)
  m.exports.add((nameId, s.itemId.item))

proc addConverter*(c: var PackedEncoder; m: var PackedModule; s: PSym) =
  assert c.thisModule == s.itemId.module
  m.converters.add(s.itemId.item)

proc addTrmacro*(c: var PackedEncoder; m: var PackedModule; s: PSym) =
  m.trmacros.add(s.itemId.item)

proc addPureEnum*(c: var PackedEncoder; m: var PackedModule; s: PSym) =
  assert s.kind == skType
  m.pureEnums.add(s.itemId.item)

proc addMethod*(c: var PackedEncoder; m: var PackedModule; s: PSym) =
  m.methods.add s.itemId.item

proc addReexport*(c: var PackedEncoder; m: var PackedModule; s: PSym) =
  let nameId = getOrIncl(m.strings, s.name.s)
  m.reexports.add((nameId, PackedItemId(module: toLitId(s.itemId.module.FileIndex, c, m),
                                        item: s.itemId.item)))

proc addCompilerProc*(c: var PackedEncoder; m: var PackedModule; s: PSym) =
  let nameId = getOrIncl(m.strings, s.name.s)
  m.compilerProcs.add((nameId, s.itemId.item))

proc toPackedNode*(n: PNode; ir: var PackedTree; c: var PackedEncoder; m: var PackedModule)
proc storeSym*(s: PSym; c: var PackedEncoder; m: var PackedModule): PackedItemId
proc storeType(t: PType; c: var PackedEncoder; m: var PackedModule): PackedItemId

proc flush(c: var PackedEncoder; m: var PackedModule) =
  ## serialize any pending types or symbols from the context
  while true:
    if c.pendingTypes.len > 0:
      discard storeType(c.pendingTypes.pop, c, m)
    elif c.pendingSyms.len > 0:
      discard storeSym(c.pendingSyms.pop, c, m)
    else:
      break

proc toPackedInfo(x: TLineInfo; c: var PackedEncoder; m: var PackedModule): PackedLineInfo =
  PackedLineInfo(line: x.line, col: x.col, file: toLitId(x.fileIndex, c, m))

proc safeItemId(s: PSym; c: var PackedEncoder; m: var PackedModule): PackedItemId {.inline.} =
  ## given a symbol, produce an ItemId with the correct properties
  ## for local or remote symbols, packing the symbol as necessary
  if s == nil or s.kind == skPackage:
    result = nilItemId
  #elif s.itemId.module == c.thisModule:
  #  result = PackedItemId(module: LitId(0), item: s.itemId.item)
  else:
    assert int(s.itemId.module) >= 0
    result = PackedItemId(module: toLitId(s.itemId.module.FileIndex, c, m),
                          item: s.itemId.item)

proc addMissing(c: var PackedEncoder; p: PSym) =
  ## consider queuing a symbol for later addition to the packed tree
  if p != nil and p.itemId.module == c.thisModule:
    if p.itemId.item notin c.symMarker:
      if not (sfForward in p.flags and p.kind in routineKinds):
        c.pendingSyms.add p

proc addMissing(c: var PackedEncoder; p: PType) =
  ## consider queuing a type for later addition to the packed tree
  if p != nil and p.uniqueId.module == c.thisModule:
    if p.uniqueId.item notin c.typeMarker:
      c.pendingTypes.add p

template storeNode(dest, src, field) =
  var nodeId: NodeId
  if src.field != nil:
    nodeId = getNodeId(m.bodies)
    toPackedNode(src.field, m.bodies, c, m)
  else:
    nodeId = emptyNodeId
  dest.field = nodeId

proc storeTypeLater(t: PType; c: var PackedEncoder; m: var PackedModule): PackedItemId =
  # We store multiple different trees in m.bodies. For this to work out, we
  # cannot immediately store types/syms. We enqueue them instead to ensure
  # we only write one tree into m.bodies after the other.
  if t.isNil: return nilItemId

  assert t.uniqueId.module >= 0
  assert t.uniqueId.item > 0
  result = PackedItemId(module: toLitId(t.uniqueId.module.FileIndex, c, m), item: t.uniqueId.item)
  if t.uniqueId.module == c.thisModule:
    # the type belongs to this module, so serialize it here, eventually.
    addMissing(c, t)

proc storeSymLater(s: PSym; c: var PackedEncoder; m: var PackedModule): PackedItemId =
  if s.isNil: return nilItemId
  assert s.itemId.module >= 0
  assert s.itemId.module >= 0
  result = PackedItemId(module: toLitId(s.itemId.module.FileIndex, c, m), item: s.itemId.item)
  if s.itemId.module == c.thisModule:
    # the sym belongs to this module, so serialize it here, eventually.
    addMissing(c, s)

proc storeType(t: PType; c: var PackedEncoder; m: var PackedModule): PackedItemId =
  ## serialize a ptype
  if t.isNil: return nilItemId

  assert t.uniqueId.module >= 0
  assert t.uniqueId.item > 0
  result = PackedItemId(module: toLitId(t.uniqueId.module.FileIndex, c, m), item: t.uniqueId.item)

  if t.uniqueId.module == c.thisModule and not c.typeMarker.containsOrIncl(t.uniqueId.item):
    if t.uniqueId.item >= m.types.len:
      setLen m.types, t.uniqueId.item+1

    var p = PackedType(kind: t.kind, flags: t.flags, callConv: t.callConv,
      size: t.size, align: t.align, nonUniqueId: t.itemId.item,
      paddingAtEnd: t.paddingAtEnd, lockLevel: t.lockLevel)
    storeNode(p, t, n)
    p.typeInst = t.typeInst.storeType(c, m)
    for kid in items t.sons:
      p.types.add kid.storeType(c, m)
    c.addMissing t.sym
    p.sym = t.sym.safeItemId(c, m)
    c.addMissing t.owner
    p.owner = t.owner.safeItemId(c, m)

    # fill the reserved slot, nothing else:
    m.types[t.uniqueId.item] = p

proc toPackedLib(l: TLib; c: var PackedEncoder; m: var PackedModule): PackedLib =
  result.kind = l.kind
  result.isOverriden = l.isOverriden
  if l.kind == libDynamic:
    result.name = storeSymLater(l.name, c, m)
  storeNode(result, l, path)

proc storeSym*(s: PSym; c: var PackedEncoder; m: var PackedModule): PackedItemId =
  ## serialize a psym
  if s.isNil: return nilItemId

  assert s.itemId.module >= 0
  result = PackedItemId(module: toLitId(s.itemId.module.FileIndex, c, m), item: s.itemId.item)

  if s.itemId.module == c.thisModule and not c.symMarker.containsOrIncl(s.itemId.item):
    if s.itemId.item >= m.syms.len:
      setLen m.syms, s.itemId.item+1

    assert sfForward notin s.flags

    var p = PackedSym(kind: s.kind, flags: s.flags, info: s.info.toPackedInfo(c, m), magic: s.magic,
      position: s.position, offset: s.offset, options: s.options,
      name: s.name.s.toLitId(m))

    storeNode(p, s, ast)
    storeNode(p, s, constraint)

    if s.kind in {skLet, skVar, skField, skForVar}:
      c.addMissing s.guard
      p.guard = s.guard.safeItemId(c, m)
      p.bitsize = s.bitsize
      p.alignment = s.alignment

    p.externalName = toLitId(s.extname, m)
    p.extFlags = s.extFlags
    c.addMissing s.typ
    p.typ = s.typ.storeType(c, m)
    c.addMissing s.owner
    p.owner = s.owner.safeItemId(c, m)
    if s.annex.index != 0:
      p.annex = (toLitId(s.annex.module.FileIndex, c, m), s.annex.index)

    # fill the reserved slot, nothing else:
    m.syms[s.itemId.item] = p

proc addModuleRef(n: PNode; ir: var PackedTree; c: var PackedEncoder; m: var PackedModule) =
  ## add a remote symbol reference to the tree
  let info = n.info.toPackedInfo(c, m)
  ir.nodes.add PackedNode(kind: nkModuleRef, operand: 3.int32, # spans 3 nodes in total
                          typeId: storeTypeLater(n.typ, c, m), info: info)
  ir.nodes.add PackedNode(kind: nkInt32Lit, info: info,
                          operand: toLitId(n.sym.itemId.module.FileIndex, c, m).int32)
  ir.nodes.add PackedNode(kind: nkInt32Lit, info: info,
                          operand: n.sym.itemId.item)

proc toPackedNode*(n: PNode; ir: var PackedTree; c: var PackedEncoder; m: var PackedModule) =
  ## serialize a node into the tree
  if n == nil:
    ir.nodes.add PackedNode(kind: nkNilRodNode, flags: {}, operand: 1)
    return
  let info = toPackedInfo(n.info, c, m)
  case n.kind
  of nkError, nkEmpty, nkNilLit, nkType, nkCommentStmt:
    ir.nodes.add PackedNode(kind: n.kind, flags: n.flags, operand: 0,
                            typeId: storeTypeLater(n.typ, c, m), info: info)
  of nkIdent:
    ir.nodes.add PackedNode(kind: n.kind, flags: n.flags,
                            operand: int32 getOrIncl(m.strings, n.ident.s),
                            typeId: storeTypeLater(n.typ, c, m), info: info)
  of nkSym:
    if n.sym.itemId.module == c.thisModule:
      # it is a symbol that belongs to the module we're currently
      # packing:
      let id = n.sym.storeSymLater(c, m).item
      ir.nodes.add PackedNode(kind: nkSym, flags: n.flags, operand: id,
                              typeId: storeTypeLater(n.typ, c, m), info: info)
    else:
      # store it as an external module reference:
      addModuleRef(n, ir, c, m)
  of directIntLit:
    ir.nodes.add PackedNode(kind: n.kind, flags: n.flags,
                            operand: int32(n.intVal),
                            typeId: storeTypeLater(n.typ, c, m), info: info)
  of externIntLit:
    ir.nodes.add PackedNode(kind: n.kind, flags: n.flags,
                            operand: int32 getOrIncl(m.numbers, n.intVal),
                            typeId: storeTypeLater(n.typ, c, m), info: info)
  of nkStrLiterals:
    ir.nodes.add PackedNode(kind: n.kind, flags: n.flags,
                            operand: int32 getOrIncl(m.strings, n.strVal),
                            typeId: storeTypeLater(n.typ, c, m), info: info)
  of nkFloatLiterals:
    ir.nodes.add PackedNode(kind: n.kind, flags: n.flags,
                            operand: int32 getOrIncl(m.numbers, cast[BiggestInt](n.floatVal)),
                            typeId: storeTypeLater(n.typ, c, m), info: info)
  of nkWithSons:
    let patchPos = ir.prepare(n.kind, n.flags,
                              storeTypeLater(n.typ, c, m), info)
    for i in 0..<n.len:
      toPackedNode(n[i], ir, c, m)
    ir.patch patchPos

proc storeTypeInst*(c: var PackedEncoder; m: var PackedModule; s: PSym; inst: PType) =
  m.typeInstCache.add (storeSymLater(s, c, m), storeTypeLater(inst, c, m))

proc addPragmaComputation*(c: var PackedEncoder; m: var PackedModule; n: PNode) =
  toPackedNode(n, m.toReplay, c, m)

proc toPackedProcDef(n: PNode; ir: var PackedTree; c: var PackedEncoder; m: var PackedModule) =
  let info = toPackedInfo(n.info, c, m)
  let patchPos = ir.prepare(n.kind, n.flags,
                            storeTypeLater(n.typ, c, m), info)
  for i in 0..<n.len:
    if i != bodyPos:
      toPackedNode(n[i], ir, c, m)
    else:
      # do not serialize the body of the proc, it's unnecessary since
      # n[0].sym.ast has the sem'checked variant of it which is what
      # everybody should use instead.
      ir.nodes.add PackedNode(kind: nkEmpty, flags: {}, operand: 0,
                              typeId: nilItemId, info: info)
  ir.patch patchPos

proc toPackedNodeIgnoreProcDefs(n: PNode, encoder: var PackedEncoder; m: var PackedModule) =
  case n.kind
  of routineDefs:
    toPackedProcDef(n, m.topLevel, encoder, m)
    when false:
      # we serialize n[namePos].sym instead
      if n[namePos].kind == nkSym:
        let s = n[namePos].sym
        discard storeSym(s, encoder, m)
        if s.flags * {sfExportc, sfCompilerProc, sfCompileTime} == {sfExportc}:
          m.exportCProcs.add(s.itemId.item)
      else:
        toPackedNode(n, m.topLevel, encoder, m)
  of nkStmtList, nkStmtListExpr:
    for it in n:
      toPackedNodeIgnoreProcDefs(it, encoder, m)
  of nkImportStmt, nkImportExceptStmt, nkExportStmt, nkExportExceptStmt,
     nkFromStmt, nkIncludeStmt:
    discard "nothing to do"
  else:
    toPackedNode(n, m.topLevel, encoder, m)

proc toPackedNodeTopLevel*(n: PNode, encoder: var PackedEncoder; m: var PackedModule) =
  toPackedNodeIgnoreProcDefs(n, encoder, m)
  flush encoder, m

proc toPackedGeneratedProcDef*(s: PSym, encoder: var PackedEncoder; m: var PackedModule) =
  ## Generic procs and generated `=hook`'s need explicit top-level entries so
  ## that the code generator can work without having to special case these. These
  ## entries will also be useful for other tools and are the cleanest design
  ## I can come up with.
  assert s.kind in routineKinds
  toPackedProcDef(s.ast, m.topLevel, encoder, m)
  #flush encoder, m

proc storeInstantiation*(c: var PackedEncoder; m: var PackedModule; s: PSym; i: PInstantiation) =
  var t = newSeq[PackedItemId](i.concreteTypes.len)
  for j in 0..high(i.concreteTypes):
    t[j] = storeTypeLater(i.concreteTypes[j], c, m)
  m.procInstCache.add PackedInstantiation(key: storeSymLater(s, c, m),
                                          sym: storeSymLater(i.sym, c, m),
                                          concreteTypes: t)
  toPackedGeneratedProcDef(i.sym, c, m)

proc storeExpansion*(c: var PackedEncoder; m: var PackedModule; info: TLineInfo; s: PSym) =
  toPackedNode(newSymNode(s, info), m.bodies, c, m)

proc storeAttachedOp*(c: var PackedEncoder; m: var PackedModule, kind: TTypeAttachedOp, t: PType, s: PSym) =
  ## Records a type-bound operator attachment action to module `m`.
  m.attachedOps.add((kind, storeTypeLater(t, c, m), storeSymLater(s, c, m)))

proc storeLib*(c: var PackedEncoder, m: var PackedModule, lib: TLib) =
  m.libs.add toPackedLib(lib, c, m)
  flush c, m

proc loadError(err: RodFileError; filename: AbsoluteFile; config: ConfigRef;) =
  case err
  of cannotOpen:
    config.localReport InternalReport(
      kind: rintWarnCannotOpenFile, file: filename.string)

  of includeFileChanged:
    config.localReport InternalReport(
      kind: rintWarnFileChanged, file: filename.string)
  else:
    config.localReport InternalReport(
      kind: rintCannotOpenFile, file: filename.string, msg: $err)

proc loadRodFile*(filename: AbsoluteFile; m: var PackedModule; config: ConfigRef;
                  ignoreConfig = false): RodFileError =
  var f = rodfiles.open(filename.string)
  f.loadHeader()
  f.loadSection configSection

  f.loadPrim m.definedSymbols
  f.loadPrim m.moduleFlags
  f.loadPrim m.cfg

  if f.err == ok and not configIdentical(m, config) and not ignoreConfig:
    f.err = configMismatch

  template loadSeqSection(section, data) {.dirty.} =
    f.loadSection section
    f.loadSeq data

  template loadTabSection(section, data) {.dirty.} =
    f.loadSection section
    f.load data

  loadTabSection stringsSection, m.strings

  loadSeqSection checkSumsSection, m.includes
  if not includesIdentical(m, config):
    f.err = includeFileChanged

  loadSeqSection depsSection, m.imports

  loadTabSection numbersSection, m.numbers

  loadSeqSection exportsSection, m.exports
  loadSeqSection hiddenSection, m.hidden
  loadSeqSection reexportsSection, m.reexports

  loadSeqSection compilerProcsSection, m.compilerProcs

  loadSeqSection trmacrosSection, m.trmacros

  loadSeqSection convertersSection, m.converters
  loadSeqSection methodsSection, m.methods
  loadSeqSection pureEnumsSection, m.pureEnums
  loadSeqSection macroUsagesSection, m.macroUsages

  loadSeqSection toReplaySection, m.toReplay.nodes
  loadSeqSection topLevelSection, m.topLevel.nodes
  loadSeqSection bodiesSection, m.bodies.nodes
  loadSeqSection symsSection, m.syms
  loadSeqSection typesSection, m.types

  loadSeqSection typeInstCacheSection, m.typeInstCache
  loadSeqSection procInstCacheSection, m.procInstCache
  loadSeqSection attachedOpsSection, m.attachedOps
  loadSeqSection methodsPerTypeSection, m.methodsPerType
  loadSeqSection enumToStringProcsSection, m.enumToStringProcs
  loadSeqSection libsSection, m.libs
  loadSeqSection typeInfoSection, m.emittedTypeInfo

  f.loadSection backendFlagsSection
  f.loadPrim m.backendFlags

  close(f)
  result = f.err

# -------------------------------------------------------------------------

proc storeError(err: RodFileError; filename: AbsoluteFile) =
  echo "Error: ", $err, "; couldn't write to ", filename.string
  removeFile(filename.string)

proc saveRodFile*(filename: AbsoluteFile; encoder: var PackedEncoder; m: var PackedModule) =
  flush encoder, m
  #rememberConfig(encoder, encoder.config)

  var f = rodfiles.create(filename.string)
  f.storeHeader()
  f.storeSection configSection
  f.storePrim m.definedSymbols
  f.storePrim m.moduleFlags
  f.storePrim m.cfg

  template storeSeqSection(section, data) {.dirty.} =
    f.storeSection section
    f.storeSeq data

  template storeTabSection(section, data) {.dirty.} =
    f.storeSection section
    f.store data

  storeTabSection stringsSection, m.strings

  storeSeqSection checkSumsSection, m.includes

  storeSeqSection depsSection, m.imports

  storeTabSection numbersSection, m.numbers

  storeSeqSection exportsSection, m.exports
  storeSeqSection hiddenSection, m.hidden
  storeSeqSection reexportsSection, m.reexports

  storeSeqSection compilerProcsSection, m.compilerProcs

  storeSeqSection trmacrosSection, m.trmacros
  storeSeqSection convertersSection, m.converters
  storeSeqSection methodsSection, m.methods
  storeSeqSection pureEnumsSection, m.pureEnums
  storeSeqSection macroUsagesSection, m.macroUsages

  storeSeqSection toReplaySection, m.toReplay.nodes
  storeSeqSection topLevelSection, m.topLevel.nodes

  storeSeqSection bodiesSection, m.bodies.nodes
  storeSeqSection symsSection, m.syms

  storeSeqSection typesSection, m.types

  storeSeqSection typeInstCacheSection, m.typeInstCache
  storeSeqSection procInstCacheSection, m.procInstCache
  storeSeqSection attachedOpsSection, m.attachedOps
  storeSeqSection methodsPerTypeSection, m.methodsPerType
  storeSeqSection enumToStringProcsSection, m.enumToStringProcs
  storeSeqSection libsSection, m.libs
  storeSeqSection typeInfoSection, m.emittedTypeInfo

  f.storeSection backendFlagsSection
  f.storePrim m.backendFlags

  close(f)
  encoder.disable()
  if f.err != ok:
    storeError(f.err, filename)

  when false:
    # basic loader testing:
    var m2: PackedModule
    discard loadRodFile(filename, m2, encoder.config)
    echo "loaded ", filename.string

# ----------------------------------------------------------------------------

type
  PackedDecoder* = object
    lastModule: int
    lastLit: LitId
    lastFile: FileIndex # remember the last lookup entry.
    config*: ConfigRef
    cache*: IdentCache

type
  ModuleStatus* = enum
    undefined,
    storing,  # state is strictly for stress-testing purposes
    loading,
    loaded,
    outdated,
    stored    # store is complete, no further additions possible

  LoadedModule* = object
    status*: ModuleStatus
    symsInit, typesInit, loadedButAliveSetChanged*: bool
    fromDisk*: PackedModule
    syms: seq[PSym] # indexed by itemId
    types: seq[PType]
    module*: PSym # the one true module symbol.
    iface, ifaceHidden: Table[PIdent, seq[PackedItemId]]
      # PackedItemId so that it works with reexported symbols too
      # ifaceHidden includes private symbols

  PackedModuleGraph* = seq[LoadedModule] # indexed by FileIndex

proc loadType(c: var PackedDecoder; g: var PackedModuleGraph; thisModule: int; t: PackedItemId): PType
proc loadSym(c: var PackedDecoder; g: var PackedModuleGraph; thisModule: int; s: PackedItemId): PSym

proc toFileIndexCached*(c: var PackedDecoder; g: PackedModuleGraph; thisModule: int; f: LitId): FileIndex =
  if c.lastLit == f and c.lastModule == thisModule:
    result = c.lastFile
  else:
    result = toFileIndex(f, g[thisModule].fromDisk, c.config)
    c.lastModule = thisModule
    c.lastLit = f
    c.lastFile = result

proc translateLineInfo(c: var PackedDecoder; g: var PackedModuleGraph; thisModule: int;
                       x: PackedLineInfo): TLineInfo =
  assert g[thisModule].status in {loaded, storing, stored}
  result = TLineInfo(line: x.line, col: x.col,
            fileIndex: toFileIndexCached(c, g, thisModule, x.file))

proc loadNodes*(c: var PackedDecoder; g: var PackedModuleGraph; thisModule: int;
                tree: PackedTree; n: NodePos): PNode =
  let k = n.kind
  if k == nkNilRodNode:
    return nil
  when false:
    echo "loading node ", c.config $ translateLineInfo(c, g, thisModule, n.info)
  result = newNodeIT(k, translateLineInfo(c, g, thisModule, n.info),
    loadType(c, g, thisModule, n.typ))
  result.flags = n.flags

  case k
  of nkError, nkEmpty, nkNilLit, nkType, nkCommentStmt:
    discard
  of nkIdent:
    result.ident = getIdent(c.cache, g[thisModule].fromDisk.strings[n.litId])
  of nkSym:
    result.sym = loadSym(c, g, thisModule, PackedItemId(module: LitId(0), item: tree.nodes[n.int].operand))
  of directIntLit:
    result.intVal = tree.nodes[n.int].operand
  of externIntLit:
    result.intVal = g[thisModule].fromDisk.numbers[n.litId]
  of nkStrLiterals:
    result.strVal = g[thisModule].fromDisk.strings[n.litId]
  of nkFloatLiterals:
    result.floatVal = cast[BiggestFloat](g[thisModule].fromDisk.numbers[n.litId])
  of nkModuleRef:
    let (n1, n2) = sons2(tree, n)
    assert n1.kind == nkInt32Lit
    assert n2.kind == nkInt32Lit
    transitionNoneToSym(result)
    result.sym = loadSym(c, g, thisModule, PackedItemId(module: n1.litId, item: tree.nodes[n2.int].operand))
  of nkWithSons - nkModuleRef:
    for n0 in sonsReadonly(tree, n):
      result.addAllowNil loadNodes(c, g, thisModule, tree, n0)

proc initPackedDecoder*(config: ConfigRef; cache: IdentCache): PackedDecoder =
  result = PackedDecoder(
    lastModule: int32(-1),
    lastLit: LitId(0),
    lastFile: FileIndex(-1),
    config: config,
    cache: cache)

proc loadProcHeader(c: var PackedDecoder; g: var PackedModuleGraph; thisModule: int;
                    tree: PackedTree; n: NodePos): PNode =
  # do not load the body of the proc. This will be done later in
  # getProcBody, if required.
  let k = n.kind
  result = newNodeIT(k, translateLineInfo(c, g, thisModule, n.info),
    loadType(c, g, thisModule, n.typ))
  result.flags = n.flags
  assert k in {nkProcDef, nkMethodDef, nkIteratorDef, nkFuncDef, nkConverterDef, nkLambda}
  var i = 0
  for n0 in sonsReadonly(tree, n):
    if i != bodyPos:
      result.add loadNodes(c, g, thisModule, tree, n0)
    else:
      result.addAllowNil nil
    inc i

proc loadProcBody(c: var PackedDecoder; g: var PackedModuleGraph; thisModule: int;
                  tree: PackedTree; n: NodePos): PNode =
  var i = 0
  for n0 in sonsReadonly(tree, n):
    if i == bodyPos:
      result = loadNodes(c, g, thisModule, tree, n0)
    inc i

proc moduleIndex(c: var PackedDecoder; g: PackedModuleGraph; thisModule: int;
                 module: LitId): int32 {.inline.} =
  ## Returns the module ID for the stored ``FileIndex`` identified by `module`.
  result = if module == LitId(0): thisModule.int32
           else: toFileIndexCached(c, g, thisModule, module).int32

proc moduleIndex*(c: var PackedDecoder; g: PackedModuleGraph; thisModule: int;
                  s: PackedItemId): int32 {.inline.} =
  result = moduleIndex(c, g, thisModule, s.module)

proc symHeaderFromPacked(c: var PackedDecoder; g: var PackedModuleGraph;
                         s: PackedSym; si, item: int32): PSym =
  result = PSym(itemId: ItemId(module: si, item: item),
    kind: s.kind, magic: s.magic, flags: s.flags,
    info: translateLineInfo(c, g, si, s.info),
    options: s.options,
    position: if s.kind in {skForVar, skVar, skLet, skTemp}: 0 else: s.position,
    offset: if s.kind in routineKinds: defaultOffset else: s.offset,
    name: getIdent(c.cache, g[si].fromDisk.strings[s.name])
  )

template loadAstBody(p, field) =
  if p.field != emptyNodeId:
    result.field = loadNodes(c, g, si, g[si].fromDisk.bodies, NodePos p.field)

template loadAstBodyLazy(p, field) =
  if p.field != emptyNodeId:
    result.field = loadProcHeader(c, g, si, g[si].fromDisk.bodies, NodePos p.field)

proc loadLib(c: var PackedDecoder; g: var PackedModuleGraph;
             si: int; l: PackedLib): TLib =
  result = TLib(isOverriden: l.isOverriden, kind: l.kind)
  if l.kind == libDynamic:
    result.name = loadSym(c, g, si, l.name)

  loadAstBody(l, path)

proc symBodyFromPacked(c: var PackedDecoder; g: var PackedModuleGraph;
                       s: PackedSym; si, item: int32; result: PSym) =
  result.typ = loadType(c, g, si, s.typ)
  loadAstBody(s, constraint)
  if result.kind in {skProc, skFunc, skIterator, skConverter, skMethod}:
    loadAstBodyLazy(s, ast)
  else:
    loadAstBody(s, ast)

  if s.annex.index != 0:
    result.annex = LibId(module: moduleIndex(c, g, si, s.annex.module),
                        index: s.annex.index)

  if s.kind in {skLet, skVar, skField, skForVar}:
    result.guard = loadSym(c, g, si, s.guard)
    result.bitsize = s.bitsize
    result.alignment = s.alignment
  result.owner = loadSym(c, g, si, s.owner)
  let externalName = g[si].fromDisk.strings[s.externalName]
  if externalName != "":
    result.extname = rope externalName
  result.extFlags = s.extFlags

proc loadSym(c: var PackedDecoder; g: var PackedModuleGraph; thisModule: int; s: PackedItemId): PSym =
  if s == nilItemId:
    result = nil
  else:
    let si = moduleIndex(c, g, thisModule, s)
    assert g[si].status in {loaded, storing, stored}
    if not g[si].symsInit:
      g[si].symsInit = true
      setLen g[si].syms, g[si].fromDisk.syms.len

    if g[si].syms[s.item] == nil:
      if g[si].fromDisk.syms[s.item].kind != skModule:
        result = symHeaderFromPacked(c, g, g[si].fromDisk.syms[s.item], si, s.item)
        # store it here early on, so that recursions work properly:
        g[si].syms[s.item] = result
        symBodyFromPacked(c, g, g[si].fromDisk.syms[s.item], si, s.item, result)
      else:
        result = g[si].module
        assert result != nil

    else:
      result = g[si].syms[s.item]

proc typeHeaderFromPacked(c: var PackedDecoder; g: var PackedModuleGraph;
                          t: PackedType; si, item: int32): PType =
  result = PType(itemId: ItemId(module: si, item: t.nonUniqueId), kind: t.kind,
                flags: t.flags, size: t.size, align: t.align,
                paddingAtEnd: t.paddingAtEnd, lockLevel: t.lockLevel,
                uniqueId: ItemId(module: si, item: item),
                callConv: t.callConv)

proc typeBodyFromPacked(c: var PackedDecoder; g: var PackedModuleGraph;
                        t: PackedType; si, item: int32; result: PType) =
  result.sym = loadSym(c, g, si, t.sym)
  result.owner = loadSym(c, g, si, t.owner)
  when false:
    for op, item in pairs t.attachedOps:
      result.attachedOps[op] = loadSym(c, g, si, item)
  result.typeInst = loadType(c, g, si, t.typeInst)
  for son in items t.types:
    result.sons.add loadType(c, g, si, son)
  loadAstBody(t, n)
  when false:
    for gen, id in items t.methods:
      result.methods.add((gen, loadSym(c, g, si, id)))

proc loadType(c: var PackedDecoder; g: var PackedModuleGraph; thisModule: int; t: PackedItemId): PType =
  if t == nilItemId:
    result = nil
  else:
    let si = moduleIndex(c, g, thisModule, t)
    assert g[si].status in {loaded, storing, stored}
    assert t.item > 0

    if not g[si].typesInit:
      g[si].typesInit = true
      setLen g[si].types, g[si].fromDisk.types.len

    if g[si].types[t.item] == nil:
      result = typeHeaderFromPacked(c, g, g[si].fromDisk.types[t.item], si, t.item)
      # store it here early on, so that recursions work properly:
      g[si].types[t.item] = result
      typeBodyFromPacked(c, g, g[si].fromDisk.types[t.item], si, t.item, result)
    else:
      result = g[si].types[t.item]
    assert result.itemId.item > 0

proc newPackage(config: ConfigRef; cache: IdentCache; fileIdx: FileIndex): PSym =
  let
    filename = AbsoluteFile toFullPath(config, fileIdx)
    info = newLineInfo(fileIdx, 1, 1)
  let desc = getPkgDesc(config, filename.string)
  result = newSym(skPackage, getIdent(cache, desc.pkgName),
    ItemId(module: PackageModuleId, item: int32(fileIdx)), nil, info)

proc setupLookupTables(g: var PackedModuleGraph; conf: ConfigRef; cache: IdentCache;
                       fileIdx: FileIndex; m: var LoadedModule) =
  m.iface = initTable[PIdent, seq[PackedItemId]]()
  m.ifaceHidden = initTable[PIdent, seq[PackedItemId]]()
  template impl(iface, e) =
    let nameLit = e[0]
    let e2 =
      when e[1] is PackedItemId: e[1]
      else: PackedItemId(module: LitId(0), item: e[1])
    iface.mgetOrPut(cache.getIdent(m.fromDisk.strings[nameLit]), @[]).add(e2)

  for e in m.fromDisk.exports:
    m.iface.impl(e)
    m.ifaceHidden.impl(e)
  for e in m.fromDisk.reexports:
    m.iface.impl(e)
    m.ifaceHidden.impl(e)
  for e in m.fromDisk.hidden:
    m.ifaceHidden.impl(e)

  let filename = AbsoluteFile toFullPath(conf, fileIdx)
  # We cannot call ``newSym`` here, because we have to circumvent the ID
  # mechanism, which we do in order to assign each module a persistent ID.
  m.module = PSym(kind: skModule, itemId: ItemId(module: int32(fileIdx), item: 0'i32),
                  name: getIdent(cache, splitFile(filename).name),
                  info: newLineInfo(fileIdx, 1, 1),
                  position: int(fileIdx))
  m.module.owner = newPackage(conf, cache, fileIdx)
  m.module.flags = m.fromDisk.moduleFlags

proc loadToReplayNodes(g: var PackedModuleGraph; conf: ConfigRef; cache: IdentCache;
                       fileIdx: FileIndex; m: var LoadedModule) =
  m.module.ast = newNode(nkStmtList)
  if m.fromDisk.toReplay.len > 0:
    var decoder = PackedDecoder(
      lastModule: int32(-1),
      lastLit: LitId(0),
      lastFile: FileIndex(-1),
      config: conf,
      cache: cache)
    for p in allNodes(m.fromDisk.toReplay):
      m.module.ast.add loadNodes(decoder, g, int(fileIdx), m.fromDisk.toReplay, p)

proc needsRecompile(g: var PackedModuleGraph; conf: ConfigRef; cache: IdentCache;
                    fileIdx: FileIndex; cachedModules: var seq[FileIndex]): bool =
  # Does the file belong to the fileIdx need to be recompiled?
  let m = int(fileIdx)
  if m >= g.len:
    g.setLen(m+1)

  case g[m].status
  of undefined:
    g[m].status = loading
    let fullpath = msgs.toFullPath(conf, fileIdx)
    let rod = toRodFile(conf, AbsoluteFile fullpath)
    let err = loadRodFile(rod, g[m].fromDisk, conf)
    if err == ok:
      result = optForceFullMake in conf.globalOptions
      # check its dependencies:
      for dep in g[m].fromDisk.imports:
        let fid = toFileIndex(dep, g[m].fromDisk, conf)
        # Warning: we need to traverse the full graph, so
        # do **not use break here**!
        if needsRecompile(g, conf, cache, fid, cachedModules):
          result = true

      if not result:
        setupLookupTables(g, conf, cache, fileIdx, g[m])
        cachedModules.add fileIdx
        g[m].status = loaded
      else:
        g[m] = LoadedModule(status: outdated, module: g[m].module)
    else:
      loadError(err, rod, conf)
      g[m].status = outdated
      result = true
    when false: loadError(err, rod, conf)
  of loading, loaded:
    # For loading: Assume no recompile is required.
    result = false
  of outdated, storing, stored:
    result = true

proc moduleFromRodFile*(g: var PackedModuleGraph; conf: ConfigRef; cache: IdentCache;
                        fileIdx: FileIndex; cachedModules: var seq[FileIndex]): PSym =
  ## Returns 'nil' if the module needs to be recompiled.
  if needsRecompile(g, conf, cache, fileIdx, cachedModules):
    result = nil
  else:
    result = g[int fileIdx].module
    assert result != nil
    assert result.position == int(fileIdx)
  for m in cachedModules:
    loadToReplayNodes(g, conf, cache, m, g[int m])

template setupDecoder() {.dirty.} =
  var decoder = PackedDecoder(
    lastModule: int32(-1),
    lastLit: LitId(0),
    lastFile: FileIndex(-1),
    config: config,
    cache: cache)

proc loadProcBody*(config: ConfigRef, cache: IdentCache;
                   g: var PackedModuleGraph; s: PSym): PNode =
  let mId = s.itemId.module
  var decoder = PackedDecoder(
    lastModule: int32(-1),
    lastLit: LitId(0),
    lastFile: FileIndex(-1),
    config: config,
    cache: cache)
  let pos = g[mId].fromDisk.syms[s.itemId.item].ast
  assert pos != emptyNodeId
  result = loadProcBody(decoder, g, mId, g[mId].fromDisk.bodies, NodePos pos)

proc loadTypeFromId*(config: ConfigRef, cache: IdentCache;
                     g: var PackedModuleGraph; module: int; id: PackedItemId): PType =
  if id.module == LitId(0) and g[module].typesInit:
    # it's a type from `module`, but it doesn't have to be cached yet
    result = g[module].types[id.item]

  if result == nil:
    setupDecoder()
    result = loadType(decoder, g, module, id)

proc loadSymFromId*(config: ConfigRef, cache: IdentCache;
                    g: var PackedModuleGraph; module: int; id: PackedItemId): PSym =
  ## Loads the symbol with `id` from the context of the module `module`. The
  ## resulting symbol is cached if it wasn't already.
  if id.module == LitId(0) and g[module].symsInit:
    # it's a symbol from `module`, but it doesn't have to be cached yet
    result = g[module].syms[id.item]

  if result == nil:
    setupDecoder()
    result = loadSym(decoder, g, module, id)

proc loadLibs*(config: ConfigRef, cache: IdentCache,
               g: var PackedModuleGraph, module: int): seq[TLib] =
  setupDecoder()
  for it in g[module].fromDisk.libs.items:
    result.add(loadLib(decoder, g, module, it))

proc translateId*(id: PackedItemId; g: PackedModuleGraph; thisModule: int; config: ConfigRef): ItemId =
  if id.module == LitId(0):
    ItemId(module: thisModule.int32, item: id.item)
  else:
    ItemId(module: toFileIndex(id.module, g[thisModule].fromDisk, config).int32, item: id.item)

proc checkForHoles*(m: PackedModule; config: ConfigRef; moduleId: int)
    {.deprecated: "for debugging".} =
  var bugs = 0
  for i in 1 .. high(m.syms):
    if m.syms[i].kind == skUnknown:
      echo "EMPTY ID ", i, " module ", moduleId, " ", toFullPath(config, FileIndex(moduleId))
      inc bugs
  assert bugs == 0
  when false:
    var nones = 0
    for i in 1 .. high(m.types):
      inc nones, m.types[i].kind == tyNone
    assert nones < 1

proc simulateLoadedModule*(g: var PackedModuleGraph; conf: ConfigRef; cache: IdentCache;
                           moduleSym: PSym; m: PackedModule) =
  # For now only used for heavy debugging. In the future we could use this to reduce the
  # compiler's memory consumption.
  let idx = moduleSym.position
  assert g[idx].status in {storing}
  g[idx].status = loaded
  assert g[idx].module == moduleSym
  setupLookupTables(g, conf, cache, FileIndex(idx), g[idx])
  loadToReplayNodes(g, conf, cache, FileIndex(idx), g[idx])

# ---------------- symbol table handling ----------------

type
  RodIter* = object
    decoder: PackedDecoder
    values: seq[PackedItemId]
    i, module: int

template interfSelect(a: LoadedModule, importHidden: bool): auto =
  var ret = a.iface.addr
  if importHidden: ret = a.ifaceHidden.addr
  ret[]

proc initRodIter*(it: var RodIter; config: ConfigRef, cache: IdentCache;
                  g: var PackedModuleGraph; module: FileIndex;
                  name: PIdent, importHidden: bool): PSym =
  it.decoder = PackedDecoder(
    lastModule: int32(-1),
    lastLit: LitId(0),
    lastFile: FileIndex(-1),
    config: config,
    cache: cache)
  it.values = g[int module].interfSelect(importHidden).getOrDefault(name)
  it.i = 0
  it.module = int(module)
  if it.i < it.values.len:
    result = loadSym(it.decoder, g, int(module), it.values[it.i])
    inc it.i

proc initRodIterAllSyms*(it: var RodIter; config: ConfigRef, cache: IdentCache;
                         g: var PackedModuleGraph; module: FileIndex, importHidden: bool): PSym =
  it.decoder = PackedDecoder(
    lastModule: int32(-1),
    lastLit: LitId(0),
    lastFile: FileIndex(-1),
    config: config,
    cache: cache)
  it.values = @[]
  it.module = int(module)
  for v in g[int module].interfSelect(importHidden).values:
    it.values.add v
  it.i = 0
  if it.i < it.values.len:
    result = loadSym(it.decoder, g, int(module), it.values[it.i])
    inc it.i

proc nextRodIter*(it: var RodIter; g: var PackedModuleGraph): PSym =
  if it.i < it.values.len:
    result = loadSym(it.decoder, g, it.module, it.values[it.i])
    inc it.i

iterator interfaceSymbols*(config: ConfigRef, cache: IdentCache;
                           g: var PackedModuleGraph; module: FileIndex;
                           name: PIdent, importHidden: bool): PSym =
  setupDecoder()
  let values = g[int module].interfSelect(importHidden).getOrDefault(name)
  for pid in values:
    let s = loadSym(decoder, g, int(module), pid)
    assert s != nil
    yield s

proc interfaceSymbol*(config: ConfigRef, cache: IdentCache;
                      g: var PackedModuleGraph; module: FileIndex;
                      name: PIdent, importHidden: bool): PSym =
  setupDecoder()
  let values = g[int module].interfSelect(importHidden).getOrDefault(name)
  result = loadSym(decoder, g, int(module), values[0])

proc idgenFromLoadedModule*(m: LoadedModule): IdGenerator =
  IdGenerator(module: m.module.itemId.module, symId: int32 m.fromDisk.syms.len,
              typeId: int32 m.fromDisk.types.len)

proc searchForCompilerproc*(m: LoadedModule; name: string): int32 =
  # slow, linear search, but the results are cached:
  for it in items(m.fromDisk.compilerProcs):
    if m.fromDisk.strings[it[0]] == name:
      return it[1]
  return -1

# ------------------------- .rod file viewer ---------------------------------

proc rodViewer*(rodfile: AbsoluteFile; config: ConfigRef, cache: IdentCache) =
  var m: PackedModule
  let err = loadRodFile(rodfile, m, config, ignoreConfig=true)
  if err != ok:
    config.quitOrRaise "Error: could not load: " & $rodfile.string & " reason: " & $err

  when true:
    echo "exports:"
    for ex in m.exports:
      echo "  ", m.strings[ex[0]], " local ID: ", ex[1]
      assert ex[0] == m.syms[ex[1]].name
      # ex[1] int32

    echo "reexports:"
    for ex in m.reexports:
      echo "  ", m.strings[ex[0]]
    #  reexports*: seq[(LitId, PackedItemId)]

    echo "hidden: " & $m.hidden.len
    for ex in m.hidden:
      echo "  ", m.strings[ex[0]], " local ID: ", ex[1]

  echo "all symbols"
  for i in 0..high(m.syms):
    if m.syms[i].name != LitId(0):
      echo "  ", m.strings[m.syms[i].name], " local ID: ", i, " kind ", m.syms[i].kind
    else:
      echo "  <anon symbol?> local ID: ", i, " kind ", m.syms[i].kind

  echo "all lib objects"
  for it in m.libs.items:
    echo "  kind: ", it.kind, " name: ", it.name, " path: ", it.path

  echo "symbols: ", m.syms.len, " types: ", m.types.len,
    " top level nodes: ", m.topLevel.nodes.len, " other nodes: ", m.bodies.nodes.len,
    " strings: ", m.strings.len, " numbers: ", m.numbers.len
