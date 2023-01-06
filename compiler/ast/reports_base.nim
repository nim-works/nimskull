## module with base legacy reports definitions

import
  compiler/ast/[
    lineinfos,
  ]

import std/[options]

type
  ReportLineInfo* = object
    ## Location expressed in terms of a single point in the file
    file*: string
    line*: uint16
    col*: int16

  # TODO: the need to inherit should be the first clue that things are wrong,
  #       thanks to that all the data type declaration dependencies are still
  #       stuck being inverted. Rip this stuff out.

  ReportBase* = object of RootObj
    location*: Option[TLineInfo] ## Location associated with report. Some
    ## reports do not have any locations associated with them (most (but
    ## not all, due to `gorge`) of the external command executions, sem
    ## tracing etc). Some reports might have additional associated location
    ## information (view type sealing reasons) - those are handled on the
    ## per-report-kind basis.

    reportInst*: ReportLineInfo ## Information about instantiation location
    ## of the reports - present for all reports in order to track their
    ## origin withing the compiler.

    reportFrom*: ReportLineInfo ## Information about submit location of the
    ## report. Contains information about the place where report was
    ## /submitted/ to the system - sometimes a report is created, modified
    ## to add new information, and only then put into the pipeline.
  
  DebugReportBase* = object of ReportBase
    ## Inherit various debugging related reports from this object