#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a dependency file generator.

import
  compiler/ast/[
    ast
  ],
  compiler/modules/[
    modulepaths,
    modulegraphs
  ],
  compiler/front/[
    options
  ],
  compiler/utils/[
    ropes,
    pathutils
  ],
  compiler/sem/[
    passes
  ]

type
  TGen = object of PPassContext
    module: PSym
    config: ConfigRef
    graph: ModuleGraph
  PGen = ref TGen

  Backend = ref object of RootRef
    dotGraph: Rope

proc addDependencyAux(b: Backend; importing, imported: string) =
  b.dotGraph.addf("\"$1\" -> \"$2\";$n", [rope(importing), rope(imported)])
  # s1 -> s2_4[label="[0-9]"];

proc addDotDependency(c: PPassContext, n: PNode): PNode =
  result = n
  let g = PGen(c)
  let b = Backend(g.graph.backend)
  case n.kind
  of nkImportStmt:
    for i in 0..<n.len:
      var imported = getModuleName(g.config, n[i])
      addDependencyAux(b, g.module.name.s, imported)
  of nkFromStmt, nkImportExceptStmt:
    var imported = getModuleName(g.config, n[0])
    addDependencyAux(b, g.module.name.s, imported)
  of nkStmtList, nkBlockStmt, nkStmtListExpr, nkBlockExpr:
    for i in 0..<n.len: discard addDotDependency(c, n[i])
  else:
    discard

proc generateDot*(graph: ModuleGraph; project: AbsoluteFile) =
  let b = Backend(graph.backend)
  discard writeRope("digraph $1 {$n$2}$n" % [
      rope(project.splitFile.name), b.dotGraph],
            changeFileExt(project, "dot"))

when not defined(nimHasSinkInference):
  {.pragma: nosinks.}

proc myOpen(graph: ModuleGraph; module: PSym; idgen: IdGenerator): PPassContext {.nosinks.} =
  var g: PGen
  new(g)
  g.module = module
  g.config = graph.config
  g.graph = graph
  if graph.backend == nil:
    graph.backend = Backend(dotGraph: "")
  result = g

const gendependPass* = makePass(open = myOpen, process = addDotDependency)
