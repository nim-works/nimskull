#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Functions for writing target's dependencies in various formats.

import
  std/[os, strutils, tables]

import
  compiler/utils/[pathutils],
  compiler/modules/[modulegraphs],
  compiler/ast/[lineinfos],
  compiler/front/[msgs, options]

proc writeDepsFile*(g: ModuleGraph) =
  let fname = g.config.nimcacheDir / RelativeFile(g.config.projectName & ".deps")
  let f = open(fname.string, fmWrite)
  for m in g.ifaces:
    if m.module != nil:
      f.writeLine(toFullPath(g.config, m.module.position.FileIndex))
  for k in g.inclToMod.keys:
    if g.getModule(k).isNil:  # don't repeat includes which are also modules
      f.writeLine(toFullPath(g.config, k))
  f.close()

func quoteFilepath(path: string): string =
  ## Quote paths for use in Unix Makefiles.
  return path.multiReplace((" ", "\\ "), ("#", "\\#"))

proc writeGccDepfile*(depfile: File, target: string, paths: openArray[string]) =
  ## Outputs one `make` rule containing target's file name, a colon, and the
  ## names of all specified paths. Spaces and hashes are escaped.
  depfile.write(target.quoteFilepath & ": \\" & '\n')
  for path in paths:
    if path.len == 0 or not fileExists(path):
      continue
    depfile.write('\t' & path.quoteFilepath & " \\" & '\n')
