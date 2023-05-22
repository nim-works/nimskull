## module with lexer legacy reports definitions
## this is all trash legacy code

import
  compiler/ast/[
    reports_base,
    report_enums,
  ]

type
  LexerReport* = object of ReportBase
    msg*: string
    kind*: ReportKind

func severity*(rep: LexerReport): ReportSeverity =
  case LexerReportKind(rep.kind):
    of rlexHintKinds: rsevHint
    of rlexErrorKinds: rsevError
    of rlexWarningKinds: rsevWarning
