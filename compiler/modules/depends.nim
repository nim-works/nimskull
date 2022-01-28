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
  ast/[
    ast,
    lineinfos,
    renderer
  ],
  modules/[
    modulepaths,
    modulegraphs
  ],
  front/[
    options,
    msgs
  ],
  utils/[
    ropes,
    pathutils,
    astrepr
  ],
  sem/[
    passes
  ],
  std/[
    tables,
    sets,
    hashes,
    sequtils,
    strutils
  ]

import std/options as std_options

func hash(ps: PSym): Hash =
  hash(ps.itemId.module) !& hash(ps.itemId.item)

type
  TGen = object of PPassContext
    module: PSym
    config: ConfigRef
    graph: ModuleGraph
  PGen = ref TGen

  DependData = ref object of RootRef
    maybeImports, maybeIncludes: Table[FileIndex, seq[
      tuple[context: seq[PNode], target: PNode]]]

    foundImports: Table[PSym, seq[PSym]]

    dotGraph: Rope

proc addDependencyAux(b: DependData; importing, imported: string) =
  b.dotGraph.addf("\"$1\" -> \"$2\";$n", [rope(importing), rope(imported)])

proc genDependOpenPreSem(graph: ModuleGraph; module: PSym; idgen: IdGenerator): PPassContext {.nosinks.} =
  discard

proc addPreSemDotDependency(c: PPassContext, n: PNode): PNode =
  result = n
  let g = PGen(c)
  let b = DependData(g.graph.backend)

  var context: seq[PNode]

  let file = n.info.fileIndex

  proc addImport(n: PNode) =
    b.maybeImports.mgetOrPut(file, @[]).add (context, n)

  proc addInclude(n: PNode) =
    b.maybeIncludes.mgetOrPut(file, @[]).add (context, n)


  proc aux(n: PNode) =
    case n.kind:
      of nkImportStmt:
        let file = n.info.fileIndex
        for sub in n:
          addImport(sub)

      of nkFromStmt, nkImportExceptStmt:
        addImport(n[0])

      of nkIncludeStmt:
        for sub in n:
          addInclude(sub)

      of nkStmtList, nkBlockStmt, nkStmtListExpr, nkBlockExpr:
        for sub in n:
          aux(sub)

      of nkWhenStmt:
        for branch in n:
          if branch.kind == nkElse:
            aux(branch[0])

          else:
            context.add branch[0].copyTree()
            aux(branch[1])
            discard context.pop

      else:
        discard

  aux(n)

proc addPostSemDotDependency(c: PPassContext, n: PNode): PNode =
  result = n
  let g = PGen(c)
  let b = DependData(g.graph.backend)
  case n.kind:
    of nkImportStmt:
      for target in n:
        b.foundImports.mgetOrPut(g.module, @[]).add target.sym

    of nkFromStmt, nkImportExceptStmt:
      b.foundImports.mgetOrPut(g.module, @[]).add n[0].sym

    of nkStmtList, nkBlockStmt, nkStmtListExpr, nkBlockExpr:
      for target in n:
        discard addPostSemDotDependency(c, target)

    else:
      discard

proc targetIndex(conf: ConfigRef, source: FileIndex, target: PNode): Option[FileIndex] =
  let modulename = getModuleName(conf, target)
  let sourceFull = toFullPath(conf, source)
  let fullPath = findModule(conf, modulename, sourceFull)
  if not fullPath.isEmpty():
    result = some fileInfoIdx(conf, fullPath)

  # else:
  #   echo sourceFull, " cannot import "
  #   debug target



func quoteGraphviz*(str: string): string =
  result.add '"'
  for idx, ch in pairs(str):
    if ch in {'\'', '"'}:

      result.add "\\"

    elif ch in {'\\'} and (
      # Do not escape `\l` and `\r`
      idx < str.high and str[idx + 1] notin {'l', 'r'}):

      result.add "\\"

    result.add ch

  result.add '"'

proc generateDot*(graph: ModuleGraph; project: AbsoluteFile) =
  let b = DependData(graph.backend)

  var confirmedImports: Table[(FileIndex, FileIndex), TLineInfo]

  let conf = graph.config
  for sym, imports in b.foundImports:
    let s = sym.info.fileIndex
    for target in imports:
      let t = target.info.fileIndex
      if s != t:
        # HACK why do we get cyclic imports everywhere?
        confirmedImports[(s, t)] = target.info


  var possibleImports: Table[(FileIndex, FileIndex), (seq[PNode], TLineInfo)]
  var notFoundImports: seq[(FileIndex, PNode, seq[PNode])]

  for source, imports in b.maybeImports:
    for (context, target) in imports:
      let idx = conf.targetIndex(source, target)
      if idx.isNone():
        notFoundImports.add((source, target, context))

      elif (source, idx.get()) in confirmedImports:
        discard

      else:
        possibleImports[(source, idx.get())] = (context, target.info)

  var possibleIncludes: Table[(FileIndex, FileIndex), (seq[PNode], TLineInfo)]
  var notFoundIncludes: seq[(FileIndex, PNode, seq[PNode])]

  for source, imports in b.maybeIncludes:
    for (context, target) in imports:
      let idx = conf.targetIndex(source, target)
      if idx.isNone():
        notFoundIncludes.add((source, target, context))

      else:
        possibleIncludes[(source, idx.get())] = (context, target.info)

  proc file(idx: FileIndex): string =
    toFilenameOption(conf, idx, foCanonical)

  proc getNode(idx: FileIndex): string =
    "_" & $idx.int

  var res = """
digraph main {
  rankdir=LR;
  node[shape=rect, fontname=Consolas];
  edge[fontname=Consolas];
"""

  var seen: HashSet[FileIndex]
  proc declNode(idx: FileIndex) =
    if idx notin seen:
      res.addf(
        "  $#[label=\"$#\"];\n",
        idx.getNode(),
        idx.file()
      )

  for pair, _ in possibleIncludes:
    declNode(pair[0])
    declNode(pair[1])

  for pair, _ in possibleImports:
    declNode(pair[0])
    declNode(pair[1])

  for (ffrom, fto) in confirmedImports.keys:
    declNode(ffrom)
    declNode(fto)

  var tempNode = 0
  proc condInclude(
      pair: (FileIndex, FileIndex),
      context: (seq[PNode], TLineInfo),
      label: string
    ) =

    var text = label & " in $#:$#" % [$context[1].line, $context[1].col]

    if 0 < context[0].len:
      text.add " if ("
      text.add mapIt(context[0], $it).join(" and ")
      text.add ")"

    res.addf("""
  $1[label=$2, shape=plain];
  $3:e -> $1:w[style=dashed, arrowhead=none];
  $1:e -> $4:w[style=dashed];
""",
      $tempNode,
      quoteGraphviz(text),
      pair[0].getNode(),
      pair[1].getNode(),
    )

    inc tempNode


  for pair, context in possibleImports:
    condInclude(pair, context, "import")

  for pair, context in possibleIncludes:
    condInclude(pair, context, "include")

  for pair, info in confirmedImports:
    condInclude(pair, (@[], info), "import")


  res.add "}"


  changeFileExt(project, "dot").string.writeFile(res)

proc genDependOpen(graph: ModuleGraph; module: PSym; idgen: IdGenerator): PPassContext =
  var g: PGen
  new(g)
  g.module = module
  g.config = graph.config
  g.graph = graph
  if graph.backend == nil:
    graph.backend = DependData(dotGraph: nil)
  result = g

const genDependPostSemPass* = makePass(
  open = genDependOpen,
  process = addPostSemDotDependency
)

const genDependPreSemPass* = makePass(
  open = genDependOpen,
  process = addPreSemDotDependency
)
