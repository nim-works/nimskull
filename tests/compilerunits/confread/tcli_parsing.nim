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
    commands
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


proc parse(cmd: string, pass: TCmdLinePass = passCmd1): tuple[reports: seq[Report], conf: CurrentConf] =
  var tmp = newConfigRef(hook)
  processCmdLine(pass, cmd, tmp)
  result.reports = getReports()
  result.conf = tmp.active

proc parse(cmds: seq[string], pass: TCmdLinePass = passCmd1): auto =
  parse(cmds.join(" "), pass)

suite "Basic command parsing":
  test "Valid commands":
    let conf = parse("compile file.nim").conf

    check conf.projectName == "file.nim"
    check conf.cmd == cmdCompileToC

  test "Backend resetting":
    let conf = parse("c --backend:c --backend:js file.nim").conf
    check conf.projectName == "file.nim"
    check conf.backend == backendJs

  test "Hint configuration":
    let conf = parse("--hint=all:off --hint=Processing:on").conf
    check(conf.noteSets[cnCurrent] * repHintKinds == {rsemProcessing})

  test "Warning configuration":
    let conf = parse("--warning=all:off --warning=UnusedImport:on").conf
    check(conf.noteSets[cnCurrent] * repWarningKinds == {rsemUnusedImport})

# import hmisc/other/hpprint

let csd = currentSourcePath().parentDir()

suite "Path options specification":
  test "Inferring lazy paths for packages":
    let conf = parse(
      "--nimblePath=$#/nimbleDir/simplePkgs" % csd,
      passCmd2).conf

    let lazy = conf.lazyPaths.mapIt(it.string).sorted()
    let pkg = csd / "nimbleDir/simplePkgs"
    let expect = sorted(@[pkg, pkg / "pkgA-0.0.1", pkg / "pkgB-0.0.2"])
    check lazy[0] == expect[0]
    check lazy[1] == expect[1]
    check lazy[2] == expect[2]

    # pprint(conf, ignore = matchField("noteSets"))
