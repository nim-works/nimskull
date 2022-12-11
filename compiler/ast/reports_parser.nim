## module with parser legacy reports definitions

import
  compiler/ast/[
    ast_types,
    reports_base,
  ]

export reports_base.ReportKind

type
  ParserReport* = object of ReportBase
    msg*: string
    found*: string
    case kind*: ReportKind
      of rparIdentExpected .. rparUnexpectedToken:
        expected*: seq[string]

      of rparInvalidFilter:
        node*: PNode

      else:
        discard

func severity*(parser: ParserReport): ReportSeverity =
  case ParserReportKind(parser.kind):
    of rparHintKinds: rsevHint
    of rparWarningKinds: rsevWarning
    of rparErrorKinds: rsevError