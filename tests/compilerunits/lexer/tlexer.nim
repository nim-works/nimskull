## Execute tests for the lexer

import
  std/[
    os,
    strutils,
    options
  ],
  compiler/ast/[
    lexer
  ],
  experimental/[
    sexp
  ]

type
  WantedToken = object
    kind: TokenKind
    text: string
    slice: Option[Slice[int]]
    line: Option[int]
    column: Option[int]

  Spec = object
    title: string
    testCode: string
    expected: seq[WantedToken]


proc parseWantedToken(sexp: SexpNode): WantedToken =
  let kind = "tk" & (sexp[0] as string)
  result.kind = parseEnum[TokenKind](kind)
  if 1 < len(sexp):
    result.text = sexp[1] as string

  let line = sexp.getField("line")
  if not line.isNil():
    result.line = some(line as int)

  let slice = sexp.getField("slice")
  if not slice.isNil():
    result.slice = some((slice[0] as int) .. (slice[1] as int))

  let column = sexp.getField("column")
  if not column.isNil():
    result.column = some(column as int)

proc splitLexerSpec(spec: string): Spec =
  type WhichPart = enum Title, InputText, ExpectedOut
  var part = Title
  for line in spec.splitLines():
    case part:
      of Title:
        if line.startsWith("==="):
          if 0 < result.title.len():
            part = InputText

        else:
          result.title = line

      of InputText:
        if line.startsWith("---"):
          part = ExpectedOut

        else:
          result.testCode &= line & "\n"

      of ExpectedOut:
        if 0 < line.len:
          result.expected.add parseWantedToken(parseSexp(line))


  
for file in walkDir(currentSourcePath().parentDir()):
  if file.path.endsWith(".txt"):
    let spec = readFile(file.path).splitLexerSpec()
    echo spec
