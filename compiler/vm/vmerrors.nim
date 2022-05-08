## This module contains the definitions for error reporting inside
## VM execution.
##
## To raise a `VmError` use `raiseVmError`. To override the source
## location where the error is reported to have happened, use the overload
## that takes an additional `TLineInfo`.
##
## `raiseVmError` must not be used outside of VM execution (`rawExecute`)
## nor inside VM code-generation (`vmgen`)

import
  compiler/ast/[
    reports
  ],
  compiler/front/[
    msgs
  ]


type VmError* = object of CatchableError
  report*: SemReport


func raiseVmError*(
  report: sink SemReport;
  inst:   InstantiationInfo = instLoc()
  ) {.noinline, noreturn.} =
  ## Raises a `VmError`. If the report has location information already,
  ## it's reset to none.
  report.location = none(TLineInfo)
  report.reportInst = toReportLineInfo(inst)
  raise (ref VmError)(report: report)


func raiseVmError*(
  report:   sink SemReport;
  location: TLineInfo,
  inst:     InstantiationInfo = instLoc()
  ) {.noinline, noreturn.} =
  ## Raises a `VmError`. If the report has location information already,
  ## it's replaced with `location`.
  report.location = some(location)
  report.reportInst = toReportLineInfo(inst)
  raise (ref VmError)(report: report)