import
  sem/[
    passes,
    sem
  ],
  front/[
    msgs,
    options,
    scriptconfig
  ],
  modules/[
    modulegraphs,
    modules
  ],
  utils/[
    pathutils,
    astrepr
  ],
  ast/[
    ast,
    idents,
    llstream,
    parser
  ]

proc parseString*(
  graph: ModuleGraph, s: string; filename: string = ""; line: int = 0): PNode =
  parseString(s, graph.cache, graph.config, filename, line)


proc compileString*(graph: ModuleGraph, text: string): PNode =
  let moduleName: string = "/tmp/compileStringModuleName.nim"
  var
    idx {.global.}: FileIndex
    res {.global.}: PNode

  idx = graph.config.fileInfoIdx(AbsoluteFile moduleName)
  res = nkStmtList.newTree()
  registerPass(graph, semPass)
  registerPass(
    graph, makePass(
      TPassOpen(
        proc(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext {.nimcall.} =
          return PPassContext()
      ),
      TPassProcess(
        proc(c: PPassContext, n: PNode): PNode {.nimcall.} =
          if n.info.fileIndex.uint32 == idx.uint32:
            debug n, onlyStructureTReprConf + trfIndexVisisted
            res.add n
          result = n
      ),
      TPassClose(
        proc(graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.} =
          discard
      )
    )
  )

  var m = graph.makeModule(moduleName)
  graph.vm = setupVM(m, graph.cache, moduleName, graph, graph.idgen)
  graph.compileSystemModule()
  discard graph.processModule(m, graph.idgen, llStreamOpen(text))
  return res
