#
#
#           The Nim Compiler
#        (c) Copyright 2021 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Entry point into the C code generator when rodfiles are used. Instead of
## invoking the code generator directly, it simply invokes the normal code
## generation orchestrator for the C backend.
##
## However, instead of leaving dead-code elimination (=DCE) to the
## orchestrator we compute the set of alive symbols here, through a prepass
## over the entire packed module graph. The code generator currently picks
## this up via the ``useAliveDataFromDce`` flag.

import
  std/[
    packedsets, algorithm
  ],
  compiler/ast/[
    ast,
    lineinfos
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/utils/[
    pathutils
  ],
  compiler/backend/[
    cgendata,
    cgen,
    extccomp
  ],
  compiler/ic/[
    packed_ast,
    ic,
    dce,
    replayer,
    rodfiles
  ],
  compiler/sem/[
    collectors
  ]

import compiler/backend/cbackend as cbackend2

proc unpackTree(g: ModuleGraph; thisModule: int;
                tree: PackedTree; n: NodePos): PNode =
  var decoder = initPackedDecoder(g.config, g.cache)
  result = loadNodes(decoder, g.packed, thisModule, tree, n)

proc setupBackendModule(g: BModuleList; m: var LoadedModule, alive: AliveSyms) =
  var bmod = cgen.newModule(g, m.module, g.config)
  bmod.idgen = idgenFromLoadedModule(m)

  bmod.flags.incl useAliveDataFromDce
  # XXX: we need to copy for now :(
  bmod.alive = alive[m.module.position]

proc addFileToLink(config: ConfigRef; m: PSym) {.used.} =
  # XXX: currently unused, but kept in case it is needed again
  let filename = AbsoluteFile toFullPath(config, m.position.FileIndex)
  let ext = ".nim.c"
  let cfile = changeFileExt(completeCfilePath(config, withPackageName(config, filename)), ext)
  let objFile = completeCfilePath(config, toObjFile(config, cfile))
  if fileExists(objFile):
    var cf = Cfile(nimname: m.name.s, cname: cfile,
                   obj: objFile,
                   flags: {CfileFlag.Cached})
    addFileToCompile(config, cf)

when defined(debugDce):
  import os, std/packedsets

proc storeAliveSymsImpl(asymFile: AbsoluteFile; s: seq[int32]) =
  var f = rodfiles.create(asymFile.string)
  f.storeHeader()
  f.storeSection aliveSymsSection
  f.storeSeq(s)
  close f

template prepare {.dirty.} =
  let asymFile = toRodFile(config, AbsoluteFile toFullPath(config, position.FileIndex), ".alivesyms")
  var s = newSeqOfCap[int32](alive[position].len)
  for a in items(alive[position]): s.add int32(a)
  sort(s)

proc storeAliveSyms(config: ConfigRef; position: int; alive: AliveSyms) =
  prepare()
  storeAliveSymsImpl(asymFile, s)

proc aliveSymsChanged(config: ConfigRef; position: int; alive: AliveSyms): bool =
  prepare()
  var f2 = rodfiles.open(asymFile.string)
  f2.loadHeader()
  f2.loadSection aliveSymsSection
  var oldData: seq[int32]
  f2.loadSeq(oldData)
  f2.close
  if f2.err == ok and oldData == s:
    result = false
  else:
    when defined(debugDce):
      let oldAsSet = toPackedSet[int32](oldData)
      let newAsSet = toPackedSet[int32](s)
      echo "set of live symbols changed ", asymFile.changeFileExt("rod"), " ", position, " ", f2.err
      echo "in old but not in new ", oldAsSet.difference(newAsSet), " number of entries in old ", oldAsSet.len
      echo "in new but not in old ", newAsSet.difference(oldAsSet), " number of entries in new ", newAsSet.len
      #if execShellCmd(getAppFilename() & " rod " & quoteShell(asymFile.changeFileExt("rod"))) != 0:
      #  echo "command failed"
    result = true
    storeAliveSymsImpl(asymFile, s)

proc storePackedModule(g: ModuleGraph, i: int; alive: AliveSyms) =
  # case statement here to enforce exhaustive checks.
  case g.packed[i].status
  of undefined, loaded:
    discard "nothing to do"
  of loading, stored:
    assert false
  of storing, outdated:
    storeAliveSyms(g.config, g.packed[i].module.position, alive)
    closeRodFile(g, g.packed[i].module)

proc generateCode*(g: ModuleGraph) =
  ## The single entry point, generate C(++) code for the entire
  ## Nim program aka `ModuleGraph`.
  resetForBackend(g)

  # First pass: replay the module-graph state changes that the backend needs to
  # know about (the alive analysis does too)
  # XXX: these state changes were already applied during semantic analysis,
  #      but ``resetForBackend`` (unnecessarily) throws them away again
  for i in 0..high(g.packed):
    replayBackendRoutines(g, i)

  var alive = computeAliveSyms(g.packed, g, g.config)

  when false:
    for i in 0..high(g.packed):
      echo i, " is of status ", g.packed[i].status, " ", toFullPath(g.config, FileIndex(i))

  # setup the module list and allocate space for all existing modules.
  # The slots for unchanged modules stay uninitialized.
  let backend = cgendata.newModuleList(g)
  backend.modules.setLen(g.packed.len)

  # Second pass: Setup all the backend modules for all the modules that have
  # changed:
  for i in 0..high(g.packed):
    # case statement here to enforce exhaustive checks.
    case g.packed[i].status
    of undefined:
      discard "nothing to do"
    of loading, stored:
      assert false
    of storing, outdated:
      setupBackendModule(backend, g.packed[i], alive)
    of loaded:
      # Even though this module didn't change, DCE might trigger a change.
      # Consider this case: Module A uses symbol S from B and B does not use
      # S itself. A is then edited not to use S either. Thus we have to
      # recompile B in order to remove S from the final result.
      if aliveSymsChanged(g.config, g.packed[i].module.position, alive):
        g.packed[i].loadedButAliveSetChanged = true

      # for now, we simply re-generate code for all modules, independent of
      # whether they've changed
      setupBackendModule(backend, g.packed[i], alive)

  # Third pass: Setup a ``ModuleList``. For simplicity, we simulate the
  # ``collectPass`` being invoked.
  const pass = collectPass
  for m in g.packed.items:
    if m.status == undefined:
      continue

    let
      pos = m.module.position
      c = pass.open(g, m.module, backend.modules[pos].idgen)
    for p in allNodes(m.fromDisk.topLevel):
      let n = unpackTree(g, pos, m.fromDisk.topLevel, p)
      discard pass.process(c, n)

    # XXX: the order in which the modules are closed is incorrect
    discard pass.close(g, c, g.emptyNode)

  var mlist = takeModuleList(g)
  # make at least sure that the main module comes last (the other modules are
  # closed in the wrong order):
  for i, pos in mlist.modulesClosed.pairs:
    if pos == g.config.projectMainIdx2:
      # move to the end:
      delete(mlist.modulesClosed, i)
      mlist.modulesClosed.add(pos)
      break

  # Fourth pass: Generate the code:
  cbackend2.generateCode(g, backend, mlist)
  g.backend = backend

  # Last pass: Write the packed modules to disk. This currently cannot happen
  # earlier, as the code generator still modifies their contents.
  for i in 0..high(g.packed):
    storePackedModule(g, i, alive)