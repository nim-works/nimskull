## module with base sem-like reports definitions

import compiler/ast/[
    ast_types,
    reports_base,
  ]

export reports_base

type
  SemishReportBase* = object of ReportBase
    ## Base for Sem and VM reports which may require additional contextual
    ## data. Created as part of a refactor to extract diagnostic data out of
    ## `reports` and put it back into the appropriate modules. The name is
    ## temporary and ideally we no longer will need inheritance.
    ## 
    ## For refactor info: https://github.com/nim-works/nimskull/issues/443
    context*: seq[ReportContext]

  ReportContextKind* = enum
    sckInstantiationOf
    sckInstantiationFrom

  ReportContext* = object
    location*: TLineInfo ## Report context instantiation
    case kind*: ReportContextKind
      of sckInstantiationOf:
        entry*: PSym ## Instantiated entry symbol
      of sckInstantiationFrom:
        discard

  SemTypeMismatch* = object
    formalTypeKind*: set[TTypeKind]
    actualType*, formalType*: PType