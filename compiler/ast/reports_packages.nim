## module with package legacy reports definitions

import
  compiler/ast/[
    reports_base,
    report_enums,
  ]

type
  PackageReport* = object of ReportBase
    case kind*: ReportKind
      of rpkgDuplicateAliasForPackageDependencies:
        parentPackage*: string
        alias*: string
        packages*: seq[string]
      else:
        discard

func severity*(report: PackageReport): ReportSeverity =
  case PackageReportKind(report.kind):
    of rpkgErrorKinds: rsevError
    of rpkgWarningKinds: rsevWarning
    of rpkgHintKinds: rsevHint
