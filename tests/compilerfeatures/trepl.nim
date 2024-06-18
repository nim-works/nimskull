discard """
  description: "Ensure that the basic REPL functionality works"
  targets: native
"""

import std/[streams, os, osproc]

const Compiler = getCurrentCompilerExe()

let
  repl = startProcess("bin/nim", args=["secret"], options={poStdErrToStdOut})
  output = repl.inputStream()  # for writing to
  input  = repl.outputStream() # for reading from

template expectLine(expect: string) =
  var line: string
  doAssert readLine(input, line)
  doAssert line == expect, "got: " & line

template expect(expect: static string) =
  var got = readStr(input, expect.len)
  doAssert got == expect, "got: " & got

template writeLine(line: string) =
  writeLine(output, line)
  flush(output)

expect ">>> "
# okay, startup was successful; no error was reported

# test a simple echo statement
writeLine "echo \"hello\""
expectLine "hello"

# test a simple procedure definition
expect ">>> "
writeLine "proc p() ="
expect "... "
writeLine "  echo \"here\""
expect "... "
writeLine ""
expect ">>> "
writeLine "p()" # call the procedure
expectLine "here"

# quit the REPL
expect ">>> "
writeLine "quit()"

# make sure shutdown worked without an error
let code = waitForExit(repl)
doAssert code == 0, "non-zero exit code: " & $code

repl.close()
