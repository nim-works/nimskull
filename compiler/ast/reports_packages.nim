## Provides the definitions for legacy package reports.

import
  compiler/ast/[
    reports_base,
    report_enums,
  ]

type
  PackageReport* = object of ReportBase
    msg*: string
    case kind*: ReportKind
      of rpkgDuplicateAliasForPackageDependencies:
        parentPackage*: string
        package*: string
        alias*: string
      of rpkgSrcDirNotRelativeToPackageDir, rpkgEntrypointNotRelativeToSrcDir:
        subject*: string
        target*: string
      else:
        discard

func severity*(report: PackageReport): ReportSeverity =
  case PackageReportKind(report.kind):
    of rpkgErrorKinds: rsevError
    of rpkgWarningKinds: rsevWarning
    of rpkgHintKinds: rsevHint
