## module with Debug legacy reports definitions
##
## TODO: even though it's legacy we shouldn't group "debug" reports like this,
##       that's what severity or some other categorization is for. Instead,
##       break these up into additional report types within reports_sem/vm/etc

import
  compiler/ast/[
    ast_types,
    lineinfos,
    reports_base,
    report_enums,
  ],
  compiler/front/[
    in_options,
  ],
  compiler/vm/vm_enums

type
  DebugVmCodeEntry* = object
    isTarget*: bool
    info*: TLineInfo
    pc*: int
    idx*: int
    case opc*: TOpcode:
      of opcConv, opcCast:
        types*: tuple[tfrom, tto: PType]
      of opcLdConst, opcAsgnConst:
        ast*: PNode
      else:
        discard
    ra*: int
    rb*: int
    rc*: int

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

      of rdbgVmCodeListing:
        vmgenListing*: tuple[
          sym: PSym,
          ast: PNode,
          entries: seq[DebugVmCodeEntry]
        ]

      else:
        discard

func severity*(report: DebugReport): ReportSeverity =
  rsevDebug
