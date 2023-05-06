## module with external legacy reports definitions
##
## TODO: even though it's legacy external reports should not be in the ast
##       package.

import
  compiler/ast/[
    reports_base,
    report_enums,
  ]

type
  ExternalReport* = object of ReportBase
    ## Report about external environment reads, passed configuration
    ## options etc.
    msg*: string
    kind*: ReportKind

func severity*(report: ExternalReport): ReportSeverity =
  case ExternalReportKind(report.kind):
    of rextErrorKinds: rsevError
    of rextHintKinds: rsevHint