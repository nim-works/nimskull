## module with internal legacy reports definitions
##
## TODO: even though it's legacy internal reports should not be in the ast
##       package.
## TODO: This module needs to be broken and those parts placed correctly:
##       1. Internal assertion/error related bits
##       2. Internal state/user input types
##       3. Help/CLI messages, etc
##       4. General report cruft


import
  compiler/ast/[
    report_enums,
    reports_base,
  ]


type
  InternalReport* = object of ReportBase
    ## Report generated for the internal compiler workings
    msg*: string
    case kind*: ReportKind
      of rintStackTrace:
        trace*: seq[StackTraceEntry] ## Generated stack trace entries

      of rintAssert:
        expression*: string

      of rintCannotOpenFile .. rintWarnFileChanged:
        file*: string

      of rintListWarnings, rintListHints:
        enabledOptions*: set[ReportKind]

      else:
        discard

func severity*(report: InternalReport): ReportSeverity =
  case InternalReportKind(report.kind)
  of rintFatalKinds:    rsevFatal
  of rintHintKinds:     rsevHint
  of rintWarningKinds:  rsevWarning
  of rintErrorKinds:    rsevError
  of rintDataPassKinds: rsevTrace
