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

    case kind*: ReportKind
      of rextInvalidHint .. rextInvalidPath, rextCfgArgDeprecatedNoop:
        cmdlineSwitch*: string ## Switch in processing
        cmdlineProvided*: string ## Value passed to the command-line
        cmdlineAllowed*: seq[string] ## Allowed command-line values
        cmdlineError*: string ## Textual description of the cmdline failure

      of rextUnknownCCompiler:
        knownCompilers*: seq[string]
        passedCompiler*: string

      of rextInvalidPackageName:
        packageName*: string

      of rextPath:
        packagePath*: string

      else:
        discard

func severity*(report: ExternalReport): ReportSeverity =
  case ExternalReportKind(report.kind):
    of rextErrorKinds: rsevError
    of rextWarningKinds: rsevWarning
    of rextHintKinds: rsevHint