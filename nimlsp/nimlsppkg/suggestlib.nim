import std/[strformat, strutils]
import messageenums
import nimsuggest
export Suggest
export IdeCmd
export NimSuggest
export initNimSuggest
import
  compiler/ast/[
    ast,
    report_enums
  ]

export ReportKind

proc stopNimSuggest*(nimsuggest: NimSuggest): int = 42

proc `$`*(suggest: Suggest): string =
  &"""(section: {suggest.section}, symKind: {suggest.symkind.TSymKind
  }, qualifiedPath: {suggest.qualifiedPath.join(".")}, forth: {suggest.forth
  }, filePath: {suggest.filePath}, line: {suggest.line}, column: {suggest.column
  }, doc: {suggest.doc}, quality: {suggest.quality}, prefix: {suggest.prefix})"""

template ellipsis(s: string): string =
  if s.len > 0: "..." else: ""

proc logFormat*(sug: Suggest): string =
  &"""(section: {sug.section}, symKind: {TSymKind(sug.symkind)},
  qualifiedPath: {sug.qualifiedPath.join(".")}, forth: {sug.forth},
  filePath: {sug.filePath.ellipsis()}, line: {sug.line}, column: {sug.column},
  doc: {sug.doc.ellipsis()}, quality: {sug.quality}, prefix: {sug.prefix})"""

func collapseByIdentifier*(suggest: Suggest): string =
  ## Function to create an identifier that can be used to remove duplicates in a list
  fmt"{suggest.qualifiedPath[^1]}__{suggest.symKind.TSymKind}"

func nimSymToLSPKind*(suggest: Suggest): CompletionItemKind =
  case suggest.symKind.TSymKind:
  of skConst: CompletionItemKind.Value
  of skEnumField: CompletionItemKind.Enum
  of skForVar: CompletionItemKind.Variable
  of skIterator: CompletionItemKind.Keyword
  of skLabel: CompletionItemKind.Keyword
  of skLet: CompletionItemKind.Value
  of skMacro: CompletionItemKind.Snippet
  of skMethod: CompletionItemKind.Method
  of skParam: CompletionItemKind.Variable
  of skProc: CompletionItemKind.Function
  of skResult: CompletionItemKind.Value
  of skTemplate: CompletionItemKind.Snippet
  of skType: CompletionItemKind.Class
  of skVar: CompletionItemKind.Field
  of skFunc: CompletionItemKind.Function
  else: CompletionItemKind.Property

func nimSymToLSPKind*(suggest: byte): SymbolKind =
  case TSymKind(suggest):
  of skConst: SymbolKind.Constant
  of skEnumField: SymbolKind.EnumMember
  of skIterator: SymbolKind.Function
  of skConverter: SymbolKind.Function
  of skLet: SymbolKind.Variable
  of skMacro: SymbolKind.Function
  of skMethod: SymbolKind.Method
  of skProc: SymbolKind.Function
  of skTemplate: SymbolKind.Function
  of skType: SymbolKind.Class
  of skVar: SymbolKind.Variable
  of skFunc: SymbolKind.Function
  of skLabel: SymbolKind.Key
  else: SymbolKind.Field

func nimSymDetails*(suggest: Suggest): string =
  case suggest.symKind.TSymKind:
  of skConst: fmt"""const {suggest.qualifiedPath.join(".")}: {suggest.forth}"""
  of skEnumField: "enum " & suggest.forth
  of skForVar: "for var of " & suggest.forth
  of skIterator: suggest.forth
  of skLabel: "label"
  of skLet: "let of " & suggest.forth
  of skMacro: "macro"
  of skMethod: suggest.forth
  of skParam: "param"
  of skProc: suggest.forth
  of skResult: "result"
  of skTemplate: suggest.forth
  of skType: "type " & suggest.qualifiedPath.join(".")
  of skVar: "var of " & suggest.forth
  else: suggest.forth

func symKindToString*(suggest: byte): string =
  case TSymKind(suggest)
    of skConst: discard
    of skEnumField: discard
    of skIterator: result = "iterator"
    of skConverter: result = "converter"
    of skLet: result = "let"
    of skMacro: result = "macro"
    of skMethod: result = "method"
    of skProc: result = "proc"
    of skTemplate: result = "template"
    of skType: discard
    of skVar: result = "var"
    of skFunc: result = "func"
    of skLabel: result = "label"
    of skUnknown: result = "unkown"
    else: result = ""

template createFullCommand(command: untyped) =
  proc command*(nimsuggest: NimSuggest, file: string, dirtyfile = "",
            line: int, col: int): seq[Suggest] =
    nimsuggest.runCmd(`ide command`, AbsoluteFile file, AbsoluteFile dirtyfile, line, col)

template createFileOnlyCommand(command: untyped) =
  proc command*(nimsuggest: NimSuggest, file: string, dirtyfile = ""): seq[Suggest] =
    nimsuggest.runCmd(`ide command`, AbsoluteFile file, AbsoluteFile dirtyfile, 0, 0)

createFullCommand(sug)
createFullCommand(con)
createFullCommand(def)
createFullCommand(use)
createFullCommand(dus)
createFileOnlyCommand(chk)
createFileOnlyCommand(highlight)
createFileOnlyCommand(outline)
createFileOnlyCommand(known)

proc `mod`*(nimsuggest: NimSuggest, file: string, dirtyfile = ""): seq[Suggest] =
  nimsuggest.runCmd(ideMod, AbsoluteFile file, AbsoluteFile dirtyfile, 0, 0)

proc fetchCachedReports*(nimsuggest: NimSuggest, file: string): seq[Suggest] =
  nimsuggest.fetchCachedReports(AbsoluteFile file)

when isMainModule:
  import os, sequtils, algorithm
  const PkgDir = currentSourcePath.parentDir.parentDir
  var graph = initNimSuggest(PkgDir / "nimlsp.nim", nimPath = PkgDir.parentDir)
  var files = toSeq(walkDirRec(PkgDir)).filterIt(it.endsWith(".nim")).sorted()
  for f in files:
    echo "outline:" & f
    let syms = graph.outline(f, f)
    echo "outline: symbols(" & $syms.len & ")"
