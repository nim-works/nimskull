import std/os

{.define(nimcore).}

import
  compiler / ast / [
    ast,
    astalgo,
    idents,
    lineinfos,
    parser,
    report_enums, # legacy reports stupidity
  ],
  compiler / front / options,
  compiler / modules / modulegraphs,
  compiler / sem / [
    passes,
    sem,
  ],
  compiler / utils / [ astrepr, pathutils, ]

# legacy reports stupidity
from compiler/ast/reports import Report, location, kind

import std/options as std_options # due to legacy reports stupidity

import spec
import boring
import mutate

template semcheck(body: untyped) {.dirty.} =
  ## perform the complete setup and compilation process
  cache = newIdentCache()
  config = newConfigRef(uhoh)
  graph = newModuleGraph(cache, config)
  graph.loadConfig(filename)

  # perform boring setup of the config using the cache
  if not setup(cache, config, graph):
    echo "crashing due to error during setup"
    quit 1

  config.verbosity = compVerbosityMin   # reduce spam

  # create a new module graph
  #graph = newModuleGraph(cache, config)

  body
  registerPass graph, semPass           # perform semcheck
  compile graph                         # run the compile
  inc counter

proc calculateScore(config: ConfigRef; n: PNode): int =
  when defined(dustFewerLines):
    result = config.linesCompiled
  else:
    result = size(n)

proc dust*(filename: AbsoluteFile) =
  var
    graph: ModuleGraph
    cache: IdentCache
    config: ConfigRef
    errorKind: ReportKind
    best: PNode
    counter = 0
    score: int
    remains: Remains
    rendered: string

  proc uhoh(config: ConfigRef, rep: Report): TErrorHandling =
    ## capture the first error
    if config.severity(rep) == rsevError:
      if std_options.unsafeGet(rep.location).fileIndex == config.projectMainIdx:
        if errorKind == repNone:
          errorKind = rep.kind
        elif errorKind == rep.kind:
          config.structuredReportHook = nil

  # in the first pass, we add the program to our cache
  semcheck:
    # basically, just taking advantage of cache and config values...
    best = toPNode(parseString(readFile(filename.string),
                       cache = cache, config = config, line = 0,
                       filename = filename.string))
    score = size(best)
    remains.add best
    assert len(remains) > 0

  # if the semcheck passes, we have nothing to do
  if config.errorCounter == 0:
    echo "error: " & filename.string & " passes the semcheck"
    quit 1

  # otherwise, we have an interesting error message to pursue
  echo "interesting: ", errorKind,
       " first of ", config.errorCounter, " errors"

  # make note of the expected number of errors
  let expected = config.errorCounter

  while len(remains) > 0:
    echo rendered
    echo "remaining: ", len(remains), " best: ", score
    let node = pop(remains)

    semcheck:
      try:
        writeFile(filename.string, $node)
      except IndexError:
        echo "cheating to get around rendering bug"
        continue

    # extra errors are a problem
    if config.errorCounter > expected:
      echo "(unexpected errors)"
    # if we didn't unhook the errors,
    # it means we didn't find the error we were looking for
    elif config.structuredReportHook != nil:
      echo "(uninteresting errors)"
    # i guess this node is a viable reproduction
    else:
      let z = calculateScore(config, node)
      if z < score:
        echo "(new high score)"
        best = node
        score = z
        rendered = $best
      for mutant in mutations(node):
        remains.add mutant

  if not best.isNil:
    debug best
    echo "=== minimal after ", counter, "/", remains.count, " semchecks; scored ", score
    echo best
    writeFile(filename.string, $best)

when isMainModule:
  if paramCount() > 0:
    dust paramStr(paramCount()).AbsoluteFile
  else:
    echo "supply a source file to inspect"