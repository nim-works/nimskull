discard """
  cmd: "nim check $options $file"
  action: reject
  nimout: '''
tinvalidasmstmt.nim(14, 3) Error: empty 'asm' statement
tinvalidasmstmt.nim(19, 22) Error: illformed AST: asm ha
tinvalidasmstmt.nim(30, 9) Error: invalid pragma: invalid
tinvalidasmstmt.nim(31, 16) Error: invalid pragma: invalid("a")
tinvalidasmstmt.nim(32, 17) Error: invalid pragma: subschar(1)
'''
"""

proc emptyAsmStmt =
  asm ""
emptyAsmStmt()

import macros
macro defA =
  result = newNimNode(nnkAsmStmt)
  result.add(
    newEmptyNode(),
    ident "ha"
  )

proc invalidAST =
  defA
invalidAST()

proc invalidPragma =
  asm {.invalid.} ""
  asm {.invalid("a").} ""
  asm {.subschar(1).} "" # subschar('`') would be valid for example

invalidPragma()
