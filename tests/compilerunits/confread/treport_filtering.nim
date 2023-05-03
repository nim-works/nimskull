discard """
  description: '''CLI and configuration file testing'''
  joinable: false
"""

## xxx: this test will likely be entirely dropped along with legacy reports
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
  compiler/ast/[
    reports,
    idents
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    options,
    commands,
    cli_reporter,
    cmdlinehelper,
    nimconf
  ]

# xxx: all the `reports` bits are legacy
from compiler/ast/reports_debug import DebugReport


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
  result.astDiagToLegacyReport = cli_reporter.legacyReportBridge
  processCmdLine(passCmd1, args.join(" "), result)

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
