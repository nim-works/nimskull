## module with VM legacy reports definitions

import
  compiler/ast/[
    ast_types,
    reports_base_sem,
    report_enums,
    lineinfos,
  ],
  compiler/utils/[
    int128,
  ]


type
  VMReport* = object of SemishReportBase
    ast*: PNode
    typ*: PType
    str*: string
    sym*: PSym
    trace*: ref VMReport  ## for storing an `rvmStackTrace`, not ideal but
                          ## reports are legacy and to be removed
    case kind*: ReportKind
      of rvmStackTrace:
        currentExceptionA*, currentExceptionB*: PNode
        stacktrace*: seq[tuple[sym: PSym, location: TLineInfo]]
        skipped*: int

      of rvmCannotCast, rvmIllegalConvFromXToY:
        actualType*, formalType*: PType

      of rvmIndexError:
        indexSpec*: tuple[usedIdx, minIdx, maxIdx: Int128]

      of rvmQuit:
        exitCode*: BiggestInt

      else:
        discard


func severity*(vm: VMReport): ReportSeverity =
  case VMReportKind(vm.kind):
  of rvmStackTrace: rsevTrace
  else: rsevError
