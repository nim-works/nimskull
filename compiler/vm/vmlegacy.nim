## module containing legacy reporting bridging code

# xxx: All the VMReport stuff needs to go, it should just be the VM or VM Gen
#      defined/provided diagnostics/etc that we shouldn't much with. The code
#      below is a temproary bridge to work around this until fixed.

import compiler/vm/vmdef

import std/options as std_options

from compiler/front/options import ConfigRef, TErrorHandling
from compiler/ast/lineinfos import TLineInfo, InstantiationInfo

from compiler/ast/reports import Report, ReportKind, toReportLineInfo
from compiler/ast/reports_vm import VMReport
from compiler/ast/reports_debug import DebugReport
from compiler/ast/reports_sem import SemTypeMismatch
from compiler/ast/report_enums import ReportCategory
from compiler/front/msgs import handleReport, localReport

func vmGenDiagToLegacyReportKind*(diag: VmGenDiagKind): ReportKind {.inline.} =
  case diag
  of vmGenDiagMissingImportcCompleteStruct: rvmMissingImportcCompleteStruct
  of vmGenDiagTooManyRegistersRequired: rvmTooManyRegistersRequired
  of vmGenDiagCannotFindBreakTarget: rvmCannotFindBreakTarget
  of vmGenDiagNotUnused: rvmNotUnused
  of vmGenDiagNotAFieldSymbol: rvmNotAFieldSymbol
  of vmGenDiagTooLargeOffset: rvmTooLargetOffset
  of vmGenDiagCannotGenerateCode: rvmCannotGenerateCode
  of vmGenDiagCannotCast: rvmCannotCast
  of vmGenDiagBadExpandToAst: rvmBadExpandToAst
  of vmGenDiagCannotEvaluateAtComptime: rvmCannotEvaluateAtComptime
  of vmGenDiagCannotImportc: rvmCannotImportc
  of vmGenDiagInvalidObjectConstructor: rvmInvalidObjectConstructor
  of vmGenDiagNoClosureIterators: rvmNoClosureIterators
  of vmGenDiagCannotCallMethod: rvmCannotCallMethod
  of vmGenDiagNotAField: rvmNotAField

func vmGenDiagToLegacyVmReport*(diag: VmGenDiag): VMReport {.inline.} =
  let kind = diag.kind.vmGenDiagToLegacyReportKind()
  result =
    case kind
    of rvmCannotCast:
      VMReport(
        ast: diag.ast,
        typ: diag.typ,
        str: diag.msg,
        sym: diag.sym,
        location: std_options.some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: kind,
        typeMismatch:
          @[SemTypeMismatch(actualType: diag.typeMismatch.actualType,
                            formalType: diag.typeMismatch.formalType)])
    else:
      VMReport(
        ast: diag.ast,
        typ: diag.typ,
        str: diag.msg,
        sym: diag.sym,
        kind: kind,
        location: std_options.some diag.location,
        reportInst: diag.instLoc.toReportLineInfo)

func vmGenDiagToLegacyReport*(diag: VmGenDiag): Report {.inline.} =
  result = Report(category: repVM, vmReport: vmGenDiagToLegacyVmReport(diag))

proc handleReport*(
    conf: ConfigRef,
    diag: VmGenDiag,
    reportFrom: InstantiationInfo,
    eh: TErrorHandling = doNothing
  ) {.inline.} =
  # REFACTOR: this is a temporary bridge into existing reporting
  let rep = diag.vmGenDiagToLegacyReport()

  handleReport(conf, rep, reportFrom, eh)


func vmEventToLegacyReportKind*(evt: VmEventKind): ReportKind {.inline.} =
  case evt
  of vmEvtOpcParseExpectedExpression: rvmOpcParseExpectedExpression
  of vmEvtUserError: rvmUserError
  of vmEvtUnhandledException: rvmUnhandledException
  of vmEvtCannotCast: rvmCannotCast
  of vmEvtCallingNonRoutine: rvmCallingNonRoutine
  of vmEvtCannotModifyTypechecked: rvmCannotModifyTypechecked
  of vmEvtNilAccess: rvmNilAccess
  of vmEvtAccessOutOfBounds: rvmAccessOutOfBounds
  of vmEvtAccessTypeMismatch: rvmAccessTypeMismatch
  of vmEvtAccessNoLocation: rvmAccessNoLocation
  of vmEvtErrInternal: rvmErrInternal
  of vmEvtIndexError: rvmIndexError
  of vmEvtOutOfRange: rvmOutOfRange
  of vmEvtOverOrUnderflow: rvmOverOrUnderflow
  of vmEvtDivisionByConstZero: rvmDivisionByConstZero
  of vmEvtArgNodeNotASymbol: rvmNodeNotASymbol
  of vmEvtNodeNotASymbol: rvmNodeNotASymbol
  of vmEvtNodeNotAProcSymbol: rvmNodeNotAProcSymbol
  of vmEvtIllegalConv: rvmIllegalConv
  of vmEvtMissingCacheKey: rvmMissingCacheKey
  of vmEvtCacheKeyAlreadyExists: rvmCacheKeyAlreadyExists
  of vmEvtFieldNotFound: rvmFieldNotFound
  of vmEvtNotAField: rvmNotAField
  of vmEvtFieldUnavailable: rvmFieldInavailable
  of vmEvtCannotSetChild: rvmCannotSetChild
  of vmEvtCannotAddChild: rvmCannotAddChild
  of vmEvtCannotGetChild: rvmCannotGetChild
  of vmEvtNoType: rvmNoType
  of vmEvtUnsupportedNonNil: rvmUnsupportedNonNil
  of vmEvtTooManyIterations: rvmTooManyIterations
  of vmEvtQuit: rvmQuit

func vmEventToLegacyVmReport*(
    evt: VmEvent,
    location: Option[TLineInfo] = std_options.none[TLineInfo]()
  ): VMReport {.inline.} =
  let kind = evt.kind.vmEventToLegacyReportKind()
  result =
    case kind
    of rvmCannotCast:
      VMReport(
        kind: kind,
        location: location,
        reportInst: evt.instLoc.toReportLineInfo,
        typeMismatch:
          @[SemTypeMismatch(actualType: evt.typeMismatch.actualType,
                            formalType: evt.typeMismatch.formalType)])
    of rvmIndexError:
      VMReport(
        location: location,
        reportInst: evt.instLoc.toReportLineInfo,
        kind: kind,
        indexSpec: evt.indexSpec)
    of rvmQuit:
      VMReport(
        location: location,
        reportInst: evt.instLoc.toReportLineInfo,
        kind: kind,
        exitCode: evt.exitCode)
    of rvmCannotSetChild, rvmCannotAddChild, rvmCannotGetChild,
        rvmUnhandledException, rvmNoType, rvmNodeNotASymbol:
      case evt.kind
      of vmEvtArgNodeNotASymbol:
        VMReport(
          location: some evt.argAst.info,
          reportInst: evt.instLoc.toReportLineInfo,
          kind: kind,
          str: evt.callName & "()",
          ast: evt.argAst)
      else:
        VMReport(
          location: location,
          reportInst: evt.instLoc.toReportLineInfo,
          kind: kind,
          ast: evt.ast)
    of rvmUserError:
      VMReport(
        kind: kind,
        str: evt.errMsg,
        location: location,
        reportInst: evt.instLoc.toReportLineInfo)
    of rvmErrInternal, rvmNilAccess, rvmIllegalConv, rvmFieldInavailable,
        rvmFieldNotFound, rvmCacheKeyAlreadyExists, rvmMissingCacheKey:
      VMReport(
        kind: kind,
        str: evt.msg,
        location: location,
        reportInst: evt.instLoc.toReportLineInfo)
    of rvmUnsupportedNonNil:
      VMReport(
        kind: kind,
        typ: evt.typ,
        location: location,
        reportInst: evt.instLoc.toReportLineInfo)
    of rvmNotAField:
      VMReport(
        kind: kind,
        sym: evt.sym,
        location: location,
        reportInst: evt.instLoc.toReportLineInfo)
    else:
      VMReport(
        kind: kind,
        location: location,
        reportInst: evt.instLoc.toReportLineInfo)

func vmEventToLegacyReport*(evt: VmEvent): Report {.inline.} =
  Report(category: repVM, vmReport: vmEventToLegacyVmReport(evt))

proc handleReport*(
    conf: ConfigRef,
    evt: VmEvent,
    reportFrom: InstantiationInfo,
    eh: TErrorHandling = doNothing
  ) {.inline.} =
  # REFACTOR: this is a temporary bridge into existing reporting
  let rep = evt.vmEventToLegacyReport()

  handleReport(conf, rep, reportFrom, eh)

proc legacyReportsVmTracer*(c: TCtx, t: VmExecTrace) =
  case t.kind
  of vmTraceMin:
    c.config.localReport(DebugReport(
        kind: rdbgVmExecTraceMinimal,
        vmgenExecMinimal: (
          info: c.debug[t.pc],
          opc: c.code[t.pc].opcode
      )))
  of vmTraceFull:
    c.config.localReport(DebugReport(
        kind: rdbgVmExecTraceFull,
        vmgenExecFull: (
          pc: t.pc,
          opc: c.code[t.pc].opcode,
          info: c.debug[t.pc],
          ra: t.ra,
          rb: t.rb,
          rc: t.rc
      )))