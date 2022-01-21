discard """
  description: '''CLI and configuration file testing'''
  joinable: false
"""

## Unit tests for command line and configuration file processing. Tests are
## separated into three stages, mirroring number of steps that are done by
## compiler to process the configuration.
##
## 1. Read only CLI interaface
## 2. Read CLI interface and configuration values
## 3. Full processing of the CLI flags - integration tests for the
##    `nim.handleCmdLine()` logic


import
  std/[
    strutils,
    os,
    sequtils
  ],
  ast/[
    reports,
    idents
  ],
  modules/[
    modulegraphs
  ],
  front/[
    options,
    commands,
    cli_reporter,
    cmdlinehelper,
    nimconf
  ]

var reported: seq[Report]

proc hook(conf: ConfigRef, report: Report): TErrorHandling =
  reported.add report
  return doNothing

proc getReports(): seq[Report] =
  result = reported
  reported = @[]

proc firstPass*(args: seq[string]): ConfigRef =
  ## Create config ref object and run fist CLI pass of on the configuration
  result = newConfigRef(hook)
  processCmdLine(passCmd1, args.join(" "), result)

proc cfgPass*(file: string, args: seq[string]): ConfigRef =
  doAssert fileExists(file), $file

  let prog = NimProg(
    supportsStdinFile: true,
    processCmdLine: processCmdLine
  )

  result = newConfigRef(hook)
  prog.processCmdLineAndProjectPath(
    result, join(args & @[file], " "))

  var cache = newIdentCache()
  var graph = newModuleGraph(cache, result)
  loadConfigs(DefaultConfig, cache, result, graph.idgen)

proc assertInter[T](inters: set[T], want: set[T] = {}) =
  doAssert inters == want, $want

block fist_pass_tests:
  block:
    let conf = firstPass(@["compile", "--hint=all:off"])
    assertInter(repHintKinds * conf.notes)

  block:
    let conf = firstPass(@["compile", "--hint=all:off", "--hint=MsgOrigin:on"])
    assertInter(repHintKinds * conf.notes, {rintMsgOrigin})

  block:
    let conf = firstPass(@[
      "compile",
      "--hint=all:off",
      "--hint=MsgOrigin:on",
      "--hint=all:off",
      "--hint=MsgOrigin:on"
    ])

    assertInter(repHintKinds * conf.notes, {rintMsgOrigin})

const dir = currentSourcePath().parentDir()

template assertEq[T](a, b: T) =
  doAssert a == b, $a & " != " & $b

block first_and_cfg_pass:
  const
    parent = dir / "cfg_processing/parent_directory/project_directory"
    file = parent / "project_file.nim"
    confread = {rdbgFinishedConfRead, rdbgStartingConfRead}

  proc getTraces(): tuple[reads, trace: seq[DebugReport]] =
    for r in getReports():
      case r.kind:
        of rdbgStartingConfRead:
          result.reads.add r.debugReport

        of rdbgCfgTrace:
          result.trace.add r.debugReport

        else:
          discard



  block:
    var conf = cfgPass(file, @["compile"])

    assertEq(conf.projectName, "project_file")
    assertEq(conf.projectFull.string, file)
    assertEq(conf.projectPath.string, parent)

    let (reads, trace) = getTraces()

    conf.filenameOption = foCanonical

    let cfgFiles = reads.mapIt(it.filename).filterIt(
      # Parent configuration file read is not disabled, so filtering out
      # any unwanted interference such as `nimskull/nim.cfg`,
      # `tests/config.nims`
      "cfg_processing" in it
    )

    assertEq(cfgFiles, @[
      dir / "cfg_processing/nim.cfg",
      dir / "cfg_processing/parent_directory/nim.cfg",
      dir / "cfg_processing/parent_directory/project_directory/nim.cfg",
      dir / "cfg_processing/parent_directory/project_directory/project_file.nim.cfg"
    ])

    assertEq(trace.mapIt(it.str), @[
      "parent+2 config",
      "parent+1 config",
      "default project configuration file",
      "project-specific configuration file"
    ])

  block:
    var conf = cfgPass(file, @["compile"])
