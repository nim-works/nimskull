## module with Debug legacy reports definitions
##
## TODO: even though it's legacy we shouldn't group "debug" reports like this,
##       that's what severity or some other categorization is for. Instead,
##       break these up into additional report types within reports_sem/vm/etc

import
  compiler/ast/[
    lineinfos,
    reports_base,
    report_enums,
  ],
  compiler/front/[
    in_options,
  ],
  compiler/vm/vm_enums

type
  DebugReport* = object of DebugReportBase
    case kind*: ReportKind
      of rdbgOptionsPush, rdbgOptionsPop:
        optionsNow*: TOptions

      of rdbgVmExecTraceFull:
        vmgenExecFull*: tuple[
          pc: int,
          opc: TOpcode,
          info: TLineInfo,
          ra, rb, rc: TRegisterKind
        ]

      of rdbgVmExecTraceMinimal:
        vmgenExecMinimal*: tuple[
          info: TLineInfo,
          opc: TOpcode
        ]

      else:
        discard

func severity*(report: DebugReport): ReportSeverity =
  rsevDebug
