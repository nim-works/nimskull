## This module provides type definitions for all structured report entries
## that compiler can provide.
##
## Note that this module specifically does not import anything else from
## the compiler - by design it is supposed to be available in every other
## module (because almost any phase of the compiler can generate reports
## one way or another). By design report should contain as much information
## as possible and *never* be used for any conditional logic in the
## compiler - it is a final form of the output that can only be printed to
## the output (either via user-provided report hook implementation, or
## using one of the built-in ones)
##
## Not using compiler-specific types also allows this report to be easily
## reused by external tooling - custom error pretty-printers, test runners
## and so on.
## 
## Debug Defines:
## `compilerDebugCompilerReportStatistics`: output stats of counts for various
##                                          report kinds

import std/[options]

import
  compiler/ast/[
    ast_types, 
    lineinfos,
    reports_base,
    report_enums,
    reports_lexer,
    reports_parser,
    reports_sem,
    reports_vm,
    reports_debug,
    reports_backend,
    reports_internal,
    reports_external,
    reports_cmd,
  ],
  compiler/utils/[
    idioms,
  ]

from compiler/utils/int128 import toInt128
from compiler/ast/reports_base_sem import ReportContext

export
  options.some,
  options.none,
  options.Option,
  int128.toInt128,
  ReportLineInfo

# from compiler/front/in_options import TOption, TOptions

# Importing and reexporting enums and 'external' reports in order to avoid
# needlessly cluttering the import lists of all modules that have to report
# something (and that would be almost all modules)
export report_enums

type
  ReportTypes* =
    LexerReport    |
    ParserReport   |
    SemReport      |
    VMReport       |
    CmdReport      |
    TraceSemReport |
    DebugReport    |
    InternalReport |
    BackendReport  |
    ExternalReport

  Report* = object
    ## Toplevel wrapper type for the compiler report
    case category*: ReportCategory
      of repLexer:
        lexReport*: LexerReport

      of repParser:
        parserReport*: ParserReport

      of repSem:
        semReport*: SemReport

      of repVM:
        vmReport*: VMReport

      of repCmd:
        cmdReport*: CmdReport

      of repDbgTrace:
        dbgTraceReport*: TraceSemReport

      of repDebug:
        debugReport*: DebugReport

      of repInternal:
        internalReport*: InternalReport

      of repBackend:
        backendReport*: BackendReport

      of repExternal:
        externalReport*: ExternalReport

static:
  when defined(compilerDebugCompilerReportStatistics):
    # TODO: some else fix this, but without adding imports, good luck
    echo(
      "Nimskull compiler outputs ",
      ord(high(ReportKind) + 1),
      " different kinds of diagnostics")

    echo "size of ReportBase     ", sizeof(ReportBase)
    echo "size of LexerReport    ", sizeof(LexerReport)
    echo "size of ParserReport   ", sizeof(ParserReport)
    echo "size of SemReport      ", sizeof(SemReport)
    echo "size of CmdReport      ", sizeof(CmdReport)
    echo "size of DbgTraceReport ", sizeof(TraceSemReport)
    echo "size of DebugReport    ", sizeof(DebugReport)
    echo "size of InternalReport ", sizeof(InternalReport)
    echo "size of BackendReport  ", sizeof(BackendReport)
    echo "size of ExternalReport ", sizeof(ExternalReport)
    echo "size of Report         ", sizeof(Report)
    echo "sem reports      = ", len(repSemKinds)
    echo "lexer reports    = ", len(repLexerKinds)
    echo "parser reports   = ", len(repParserKinds)
    echo "internal reports = ", len(repInternalKinds)

let reportEmpty* = Report(
  category: repInternal,
  internalReport: InternalReport(kind: repNone))

template eachCategory*(report: Report, field: untyped): untyped =
  case report.category:
    of repLexer:    report.lexReport.field
    of repParser:   report.parserReport.field
    of repCmd:      report.cmdReport.field
    of repVM:       report.vmReport.field
    of repSem:      report.semReport.field
    of repDbgTrace: report.dbgTraceReport.field
    of repDebug:    report.debugReport.field
    of repInternal: report.internalReport.field
    of repBackend:  report.backendReport.field
    of repExternal: report.externalReport.field

func kind*(report: Report): ReportKind =
  eachCategory(report, kind)

func location*(report: Report): Option[TLineInfo] =
  eachCategory(report, location)

func reportInst*(report: Report): ReportLineInfo =
  eachCategory(report, reportInst)

func reportFrom*(report: Report): ReportLineInfo =
  eachCategory(report, reportFrom)

func context*(report: Report): seq[ReportContext] =
  case report.category
  of repSem:
    result = report.semReport.context
  of repVM:
    result = report.vmReport.context
  else:
    unreachable "context get on category: " & $report.category

func `context=`*(report: var Report, context: seq[ReportContext]) =
  case report.category
  of repSem: report.semReport.context = context
  of repVM:  report.vmReport.context = context
  else: unreachable "context get on category: " & $report.category

func `reportFrom=`*(report: var Report, loc: ReportLineInfo) =
  case report.category
  of repLexer:    report.lexReport.reportFrom = loc
  of repParser:   report.parserReport.reportFrom = loc
  of repCmd:      report.cmdReport.reportFrom = loc
  of repSem:      report.semReport.reportFrom = loc
  of repVM:       report.vmReport.reportFrom = loc
  of repDebug:    report.debugReport.reportFrom = loc
  of repDbgTrace: report.dbgTraceReport.reportFrom = loc
  of repInternal: report.internalReport.reportFrom = loc
  of repBackend:  report.backendReport.reportFrom = loc
  of repExternal: report.externalReport.reportFrom = loc

func category*(kind: ReportKind): ReportCategory =
  case kind
  of repDbgTraceKinds: result = repDbgTrace
  of repDebugKinds:    result = repDebug
  of repInternalKinds: result = repInternal
  of repExternalKinds: result = repExternal
  of repCmdKinds:      result = repCmd
  of repLexerKinds:    result = repLexer
  of repParserKinds:   result = repParser
  of repSemKinds:      result = repSem
  of repBackendKinds:  result = repBackend
  of repVMKinds:       result = repVM
  of repNone: assert false, "'none' report does not have category"

func severity*(
    report: ReportTypes,
    asError: ReportKinds,
    asWarning: ReportKinds = default(ReportKinds)
  ): ReportSeverity =
  if report.kind in asError:
    rsevError
  elif report.kind in asWarning:
    rsevWarning
  else:
    severity(report)

func severity*(
    report: Report,
    asError: ReportKinds = default(ReportKinds),
    asWarning: ReportKinds = default(ReportKinds)
  ): ReportSeverity =
  ## Return report severity accounting for 'asError' and 'asWarning'
  ## mapping sets.

  if report.kind in asError: rsevError
  elif report.kind in asWarning: rsevWarning
  else:
    case report.category:
      of repLexer:    report.lexReport.severity()
      of repParser:   report.parserReport.severity()
      of repSem:      report.semReport.severity()
      of repCmd:      report.cmdReport.severity()
      of repVM:       report.vmReport.severity()
      of repInternal: report.internalReport.severity()
      of repBackend:  report.backendReport.severity()
      of repDebug:    report.debugReport.severity()
      of repDbgTrace: report.dbgTraceReport.severity()
      of repExternal: report.externalReport.severity()

func toReportLineInfo*(iinfo: InstantiationInfo): ReportLineInfo =
  ReportLineInfo(file: iinfo[0], line: uint16(iinfo[1]), col: int16(iinfo[2]))

template calledFromInfo*(): ReportLineInfo =
  {.line.}:
    let e = getStackTraceEntries()[^2]
    ReportLineInfo(file: $e.filename, line: e.line.uint16)

func isValid*(point: ReportLineInfo): bool =
  point.file.len > 0 and point.file != "???"

template reportHere*[R: ReportTypes](report: R): R =
  block:
    var tmp = report
    tmp.reportInsta = toReportLineInfo(
      instantiationInfo(fullPaths = true))

    tmp

func wrap*(rep: sink LexerReport): Report =
  assert rep.kind in repLexerKinds, $rep.kind
  Report(category: repLexer, lexReport: rep)

func wrap*(rep: sink ParserReport): Report =
  assert rep.kind in repParserKinds, $rep.kind
  Report(category: repParser, parserReport: rep)

func wrap*(rep: sink VMReport): Report =
  assert rep.kind in repVMKinds, $rep.kind
  Report(category: repVM, vmReport: rep)

func wrap*(rep: sink SemReport): Report =
  assert rep.kind in repSemKinds, $rep.kind
  Report(category: repSem, semReport: rep)

func wrap*(rep: sink BackendReport): Report =
  assert rep.kind in repBackendKinds, $rep.kind
  Report(category: repBackend, backendReport: rep)

func wrap*(rep: sink CmdReport): Report =
  assert rep.kind in repCmdKinds, $rep.kind
  Report(category: repCmd, cmdReport: rep)

func wrap*(rep: sink TraceSemReport): Report =
  assert rep.kind in repDbgTraceKinds, $rep.kind
  Report(category: repDbgTrace, dbgTraceReport: rep)

func wrap*(rep: sink DebugReport): Report =
  assert rep.kind in repDebugKinds, $rep.kind
  Report(category: repDebug, debugreport: rep)

func wrap*(rep: sink InternalReport): Report =
  assert rep.kind in repInternalKinds, $rep.kind
  Report(category: repInternal, internalReport: rep)

func wrap*(rep: sink ExternalReport): Report =
  assert rep.kind in repExternalKinds, $rep.kind
  Report(category: repExternal, externalReport: rep)

func wrap*[R: ReportTypes](rep: sink R, iinfo: InstantiationInfo): Report =
  var tmp = rep
  tmp.reportInst = toReportLineInfo(iinfo)
  return wrap(tmp)

func wrap*[R: ReportTypes](
    rep: sink R, iinfo: ReportLineInfo, point: TLineInfo): Report =
  var tmp = rep
  tmp.reportInst = iinfo
  tmp.location = some point
  return wrap(tmp)

func wrap*[R: ReportTypes](
    rep: sink R, iinfo: InstantiationInfo, point: TLineInfo): Report =
  wrap(rep, toReportLineInfo(iinfo), point)

func wrap*[R: ReportTypes](iinfo: InstantiationInfo, rep: sink R): Report =
  wrap(rep, iinfo)

template wrap*(rep: ReportTypes): Report =
  wrap(rep, toReportLineInfo(instLoc()))

func `$`*(point: ReportLineInfo): string =
  point.file & "(" & $point.line & ", " & $point.col & ")"

func actualType*(r: SemReport | VMReport): PType = r.typeMismatch[0].actualType
func formalType*(r: SemReport | VMReport): PType = r.typeMismatch[0].formalType
func formalTypeKind*(r: SemReport): set[TTypeKind] = r.typeMismatch[0].formalTypeKind
func symstr*(r: SemReport | VMReport): string = r.sym.name.s
