## module with CLI legacy reports definitions
##
## TODO: even though it's legacy, it's highly unlike that this should be
##       included in the ast package

import
  compiler/ast/[
    reports_base,
  ]

type
  CmdReport* = object of ReportBase
    cmd*: string
    msg*: string
    code*: int
    case kind*: ReportKind
      of rcmdFailedExecution:
        exitOut*, exitErr*: string
      else:
        discard


func severity*(report: CmdReport): ReportSeverity =
  case CmdReportKind(report.kind):
    of rcmdHintKinds: rsevHint
    of rcmdWarningKinds: rsevWarning
    of rcmdErrorKinds: rsevError