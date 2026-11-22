## The main dust driver, orchestrating the AST reduction process.


{.define(nimcore).}

import
  std/[
    os
  ],
  compiler/ast/[
    ast,
    idents,
    lineinfos,
    parser,
    report_enums, # legacy reports stupidity
  ],
  compiler/front/options,
  compiler/modules/modulegraphs,
  compiler/sem/[
    passes,
    sem,
  ],
  compiler/utils/[astrepr,],
  std/options as std_options, # due to legacy reports stupidity
  spec,
  boring,
  mutate

# legacy reports stupidity
from compiler/ast/reports import Report, location, kind
from compiler/front/cli_reporter import reportFull, legacyReportBridge
from compiler/front/msgs import defaultDiagHandler


template semcheck(body: untyped) {.dirty.} =
  ## First performs the complete setup, then runs `body`, and finally runs
  ## sem-checking on the graph.
  cache = newIdentCache()
  config = newConfigRef(uhoh)
  config.diagHandler = msgs.defaultDiagHandler
  config.astDiagToLegacyReport = cli_reporter.legacyReportBridge
  graph = newModuleGraph(cache, config)

  # perform boring setup of the config and graph (command line parsing, config
  # file loading, etc.)
  if not setup(cache, config, graph, args):
    return ErrorCode.setupError

  # make sure there's a main module provided
  if not wantMainModule(config):
    return ErrorCode.fileNotProvided

  config.verbosity = compVerbosityMin   # reduce spam

  body
  registerPass graph, semPass           # perform semcheck
  compile graph                         # run the compile
  inc counter


proc calculateScore(config: ConfigRef; n: PNode): int =
  when defined(dustFewerLines):
    result = config.linesCompiled
  else:
    result = size(n)


proc dustReportHook(conf: ConfigRef, report: Report): TErrorHandling =
  # uncomment below to see all errors+
  # if conf.severity(report) >= rsevError:
  #   echo conf.reportFull(report)
  doDefault


proc dust*(args: openArray[string]): ErrorCode =
  var
    graph: ModuleGraph
    cache: IdentCache
    config: ConfigRef
    errorKind: ReportKind
    best: PNode
    counter = 0
    score: int

  result = ErrorCode.success

  proc uhoh(config: ConfigRef, rep: Report): TErrorHandling =
    ## capture the first error
    if config.severity(rep) == rsevError:
      if std_options.isSome(rep.location) and
         std_options.unsafeGet(rep.location).fileIndex == config.projectMainIdx:
        if errorKind == repNone:
          errorKind = rep.kind
        elif errorKind == rep.kind:
          config.structuredReportHook = dustReportHook
    # uncomment below to see all errors+
    # if config.severity(rep) >= rsevError:
    #   echo config.reportFull(rep)

  # in the first pass, we add the program to our cache
  semcheck:
    # basically, just taking advantage of cache and config values...
    best = toPNode(parseString(readFile(config.projectFull.string),
                       cache = cache, config = config, line = 0,
                       filename = config.projectFull.string))

  # if the semcheck passes, we have nothing to do
  if config.errorCounter == 0:
    return ErrorCode.noError

  # otherwise, we have an interesting error message to pursue
  echo "interesting: ", errorKind,
       " first of ", config.errorCounter, " errors"

  # make note of the expected number of errors
  let expected = config.errorCounter

  while true:
    echo best
    echo "----- current score: ", calculateScore(config, best)

    var found = PNode nil
    var iter = initMutator(best)
    # go over all mutations, committing the ones that reproduce the error
    while (let node = iter.get(); node != nil):
      semcheck:
        writeFile(config.projectFull.string, $node)

      # extra errors are a problem
      if config.errorCounter > expected:
        echo "(unexpected errors)"
        iter.next()
      # if we didn't unhook the errors,
      # it means we didn't find the error we were looking for
      elif config.structuredReportHook != dustReportHook:
        echo "(uninteresting errors)"
        iter.next()
      else:
        # found a viable tree
        iter.keep()
        found = node
    
    if found.isNil:
      # none of the candidates reproduces the property; we're done
      break

    # note: it's possible, and valid, for the new best to have the same score
    # as the previous best. This is because some reductions (e.g., replacing
    # an `nkIntLit` with a `nkEmpty`) don't change the number of nodes
    best = found

  if not best.isNil:
    debug best
    score = calculateScore(config, best)
    echo "=== minimal after ", counter, " semchecks; scored ", score
    echo best
    writeFile(config.projectFull.string, $best)


when isMainModule:
  let args = getExecArgs()
  let code = dust(args)
  case code
  of ErrorCode.setupError:
    echo "crashing due to error during setup"
  of ErrorCode.noError:
    echo "error: module passes the semcheck"
  of ErrorCode.fileNotProvided:
    echo "supply a source file to inspect"
  of ErrorCode.compilerError:
    unreachable "This should only be detectable when the CLI abnormally exits"
  of ErrorCode.success:
    echo "success"
  quit ord(code)
