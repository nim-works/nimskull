## module with parser legacy reports definitions

import
  compiler/ast/[
    ast_types,
    reports_base,
    report_enums,
  ]

from compiler/ast/lineinfos import TLineInfo

type
  ParserReport* = object of ReportBase
    msg*: string
    found*: string
    case kind*: ReportKind
      of rparInvalidIndentationWithForgotEqualSignHint:
        eqInfo*: TLineInfo

      of rparIdentExpected .. rparMissingToken:
        expected*: seq[string]

      of rparUnexpectedToken:
        expectedKind*: string

      of rparAsmStmtExpectsStrLit:
        discard

      of rparInvalidFilter:
        node*: PNode

      else:
        discard

func severity*(parser: ParserReport): ReportSeverity =
  case ParserReportKind(parser.kind):
    of rparHintKinds: rsevHint
    of rparWarningKinds: rsevWarning
    of rparErrorKinds: rsevError