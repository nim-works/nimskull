## module with lexer legacy reports definitions

import
  compiler/ast/[
    reports_base,
    report_enums,
  ]

type
  LexerReport* = object of ReportBase
    msg*: string
    case kind*: ReportKind
      of rlexLinterReport:
        wanted*: string
        got*: string
      else:
        discard

func severity*(rep: LexerReport): ReportSeverity =
  case LexerReportKind(rep.kind):
    of rlexHintKinds: rsevHint
    of rlexErrorKinds: rsevError
    of rlexWarningKinds: rsevWarning
