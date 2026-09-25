discard """
description: ""
joinable: false
"""

import
  compiler/ast/[
    reports
  ],
  compiler/front/[
    options,
    commands,
    cli_reporter,
    msgs
  ],
  std/[
    unittest,
    strtabs,
    strutils,
    os,
    algorithm,
    sequtils
  ]


var reported: seq[Report]

proc hook(conf: ConfigRef, report: Report): TErrorHandling =
  reported.add report
  return doNothing

proc getReports(): seq[Report] =
  result = reported
  reported = @[]


proc parse(args: openArray[string], pass: TCmdLinePass = passCmd1): tuple[reports: seq[Report], conf: CurrentConf] =
  var tmp = newConfigRef(hook)
  tmp.diagHandler = msgs.defaultDiagHandler
  tmp.astDiagToLegacyReport = cli_reporter.legacyReportBridge
  processCmdLine(pass, args, tmp)
  result.reports = getReports()
  result.conf = tmp.active

suite "Basic command parsing":
  test "Valid commands":
    let conf = parse(["compile", "file.nim"]).conf

    check conf.projectName == "file.nim"
    check conf.cmd == cmdCompileToC

  test "Backend resetting":
    let conf = parse(["c", "--backend:c", "--backend:js", "file.nim"]).conf
    check conf.projectName == "file.nim"
    check conf.backend == backendJs

  test "Hint configuration":
    let conf = parse(["--hint=all:off", "--hint=Processing:on"]).conf
    check(conf.noteSets[cnCurrent] * repHintKinds == {rsemProcessing})

  test "Warning configuration":
    let conf = parse(["--warning=all:off", "--warning=UnusedImport:on"]).conf
    check(conf.noteSets[cnCurrent] * repWarningKinds == {rsemUnusedImport})

# import hmisc/other/hpprint
