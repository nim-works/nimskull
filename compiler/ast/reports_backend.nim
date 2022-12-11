## module with backend legacy reports definitions
##
## TODO: even though it's legacy backend reports should not be in the ast
##       package.

import
  compiler/ast/[
    reports_base,
  ]

type
  BackendReport* = object of ReportBase
    msg*: string
    usedCompiler*: string
    case kind*: ReportKind
      of rbackCannotWriteScript,
         rbackProducedAssembly,
         rbackCannotWriteMappingFile:
        filename*: string

      of rbackTargetNotSupported:
        requestedTarget*: string

      of rbackJsonScriptMismatch:
        jsonScriptParams*: tuple[
          outputCurrent, output, jsonFile: string
        ]

      of rbackVmFileWriteFailed:
        outFilename*: string
        failureMsg*: string ## string rep of the ``RodFileError``, so that
                            ## ``rodfiles`` doesn't need to be imported here
      else:
        discard

func severity*(report: BackendReport): ReportSeverity =
  case BackendReportKind(report.kind):
    of rbackErrorKinds: rsevError
    of rbackHintKinds: rsevHint
    of rbackWarningKinds: rsevWarning