import std/os

{.define(nimcore).}

import
  compiler / ast / [
    ast,
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
from compiler / ast / reports import Report, location, kind
from compiler / front / cli_reporter import reportFull, legacyReportBridge
from compiler / front / msgs import defaultDiagHandler

import std/options as std_options # due to legacy reports stupidity
import std/[sets, algorithm]

import spec
import hashing
import boring
import mutate

type
  ErrorCode* = enum
    success = 0
    fileNotProvided = 1
    setupError = 2
    noError = 3

template semcheck(body: untyped) {.dirty.} =
  ## perform the complete setup and compilation process
  cache = newIdentCache()
  config = newConfigRef(uhoh)
  config.diagHandler = msgs.defaultDiagHandler
  config.astDiagToLegacyReport = cli_reporter.legacyReportBridge
  graph = newModuleGraph(cache, config)

  # perform boring setup of the config and graph (command line parsing, config
  # file loading, etc.)
  if not setup(cache, config, graph, args):
    return ErrorCode.setupError

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

proc dust*(filename: AbsoluteFile): ErrorCode =
  var
    graph: ModuleGraph
    cache: IdentCache
    config: ConfigRef
    errorKind: ReportKind
    best: PNode
    counter = 0
    score: int
    remains: Remains

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
    best = toPNode(parseString(readFile(filename.string),
                       cache = cache, config = config, line = 0,
                       filename = filename.string))

  # if the semcheck passes, we have nothing to do
  if config.errorCounter == 0:
    return ErrorCode.noError

  # otherwise, we have an interesting error message to pursue
  echo "interesting: ", errorKind,
       " first of ", config.errorCounter, " errors"

  # make note of the expected number of errors
  let expected = config.errorCounter
  var seen: HashSet[SigHash]

  while true:
    var remains: seq[(SigHash, PNode)]
    # gather all possible, not-yet-tried mutations:
    for mutant in mutations(best):
      let hash = hashNode(mutant)
      if hash notin seen:
        remains.add (hash, mutant)
  
    if remains.len == 0:
      # there are none; we're done
      break
    # sort by their score. The one with the lowest score has to come first
    sort(remains, proc(a, b: auto): int =
      calculateScore(config, a[1]) - calculateScore(config, b[1]))

    echo best
    echo "----- current score: ", calculateScore(config, best)

    var found = PNode nil
    # try all candidates, starting with the smallest one
    for (hash, node) in remains.items:
      seen.incl(hash)

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
      elif config.structuredReportHook != dustReportHook:
        echo "(uninteresting errors)"
      # i guess this node is a viable reproduction
      else:
        # found a viable tree
        found = node
        break
    
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
    echo "=== minimal after ", counter, "/", remains.count, " semchecks; scored ", score
    echo best
    writeFile(filename.string, $best)

when isMainModule:
  from std/strutils import strip

  if paramCount() > 0 and paramStr(paramCount()).strip() != "":
    let
      file = paramStr(paramCount())
      absFile = toAbsolute(file, AbsoluteDir(getCurrentDir()))
      code = dust(absFile)
    case code
    of ErrorCode.setupError:
      echo "crashing due to error during setup"
    of ErrorCode.noError:
      echo "error: " & file & " passes the semcheck"
    of ErrorCode.fileNotProvided:
      echo "args: ", commandLineParams()
      echo "supply a source file to inspect"
    of ErrorCode.success:
      echo "success: " & file
    quit ord(code)
  else:
    echo "supply a source file to inspect"
    quit ord(ErrorCode.fileNotProvided)
