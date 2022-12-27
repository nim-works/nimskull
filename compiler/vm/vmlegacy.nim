## module containing legacy reporting bridging code

# xxx: All the VMReport stuff needs to go, it should just be the VM or VM Gen
#      defined/provided diagnostics/etc that we shouldn't much with. The code
#      below is a temproary bridge to work around this until fixed.

import compiler/vm/vmdef

import std/options as std_options

from compiler/ast/ast_types import TMagic, SemTypeMismatch
from compiler/ast/lineinfos import TLineInfo, InstantiationInfo

from compiler/ast/reports import Report, ReportKind, toReportLineInfo
from compiler/ast/reports_vm import VMReport
from compiler/ast/reports_debug import DebugReport
from compiler/ast/report_enums import ReportCategory
from compiler/front/msgs import localReport

func vmGenDiagToLegacyReportKind(diag: VmGenDiagKind): ReportKind {.inline.} =
  case diag
  of vmGenDiagMissingImportcCompleteStruct: rvmMissingImportcCompleteStruct
  of vmGenDiagTooManyRegistersRequired: rvmTooManyRegistersRequired
  of vmGenDiagCannotFindBreakTarget: rvmCannotFindBreakTarget
  of vmGenDiagNotUnused: rvmNotUnused
  of vmGenDiagNotAFieldSymbol: rvmNotAFieldSymbol
  of vmGenDiagTooLargeOffset: rvmTooLargetOffset
  of vmGenDiagCannotGenerateCode: rvmCannotGenerateCode
  of vmGenDiagCodeGenUnhandledMagic: rvmCannotGenerateCode
  of vmGenDiagCodeGenGenericInNonMacro: rvmCannotGenerateCode
  of vmGenDiagCodeGenUnexpectedSym: rvmCannotGenerateCode
  of vmGenDiagCannotCast: rvmCannotCast
  of vmGenDiagBadExpandToAstArgRequired: rvmBadExpandToAst
  of vmGenDiagBadExpandToAstCallExprRequired: rvmBadExpandToAst
  of vmGenDiagCannotEvaluateAtComptime: rvmCannotEvaluateAtComptime
  of vmGenDiagCannotImportc: rvmCannotImportc
  of vmGenDiagInvalidObjectConstructor: rvmInvalidObjectConstructor
  of vmGenDiagNoClosureIterators: rvmNoClosureIterators
  of vmGenDiagCannotCallMethod: rvmCannotCallMethod

template magicToString(m: TMagic): string =
  case m
  of mSizeOf:   "sizeOf"
  of mAlignOf:  "align"
  of mOffsetOf: "offset"
  else:         $m

func vmGenDiagToLegacyVmReport*(diag: VmGenDiag): VMReport {.inline.} =
  let kind = diag.kind.vmGenDiagToLegacyReportKind()
  result =
    case diag.kind
    of vmGenDiagCannotCast:
      VMReport(
        location: std_options.some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rvmCannotCast,
        typeMismatch:
          @[SemTypeMismatch(actualType: diag.typeMismatch.actualType,
                            formalType: diag.typeMismatch.formalType)])
    of vmGenDiagCodeGenUnhandledMagic,
        vmGenDiagMissingImportcCompleteStruct:
      VMReport(
        str: magicToString(diag.magic),
        kind: kind,
        location: std_options.some diag.location,
        reportInst: diag.instLoc.toReportLineInfo)
    of vmGenDiagCodeGenGenericInNonMacro,
        vmGenDiagCodeGenUnexpectedSym,
        vmGenDiagNoClosureIterators,
        vmGenDiagCannotImportc,
        vmGenDiagCannotCallMethod,
        vmGenDiagTooLargeOffset:
      VMReport(
        str: case diag.kind
              of vmGenDiagCodeGenGenericInNonMacro:
                "Attempt to generate VM code for generic parameter in non-macro proc"
              of vmGenDiagCodeGenUnexpectedSym:
                "Unexpected symbol for VM code - " & $diag.sym.kind
              else:
                "",
        sym: diag.sym,
        location: std_options.some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: kind)
    of vmGenDiagBadExpandToAstArgRequired:
      VMReport(
        str: "expandToAst requires 1 argument",
        kind: kind,
        location: std_options.some diag.location,
        reportInst: diag.instLoc.toReportLineInfo)
    of vmGenDiagBadExpandToAstCallExprRequired:
      VMReport(
        str: "expandToAst requires a call expression",
        kind: kind,
        location: std_options.some diag.location,
        reportInst: diag.instLoc.toReportLineInfo)
    of vmGenDiagNotUnused,
        vmGenDiagNotAFieldSymbol,
        vmGenDiagCannotGenerateCode,
        vmGenDiagCannotEvaluateAtComptime,
        vmGenDiagInvalidObjectConstructor:
      VMReport(
        ast: diag.ast,
        kind: kind,
        location: std_options.some diag.location,
        reportInst: diag.instLoc.toReportLineInfo)
    else:
      VMReport(
        kind: kind,
        location: std_options.some diag.location,
        reportInst: diag.instLoc.toReportLineInfo)

func vmGenDiagToLegacyReport*(diag: VmGenDiag): Report {.inline.} =
  result = Report(category: repVM, vmReport: vmGenDiagToLegacyVmReport(diag))

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