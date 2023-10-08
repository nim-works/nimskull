discard """
  targets: "c js vm"
  knownIssue.vm: "std/streams is not supported"
  description: '''
    Ensure that the values of impure expressions used as operands in
    ``check`` expression are properly captured (assigned to
    temporaries) and, in case of failure, displayed
  '''
"""

import std/[unittest, sequtils, strutils, exitprocs]

var
  output: seq[string] ## set after a ``check`` failure
  wasFailure: bool

type MockFormatter = ref object of OutputFormatter

method failureOccurred(f: MockFormatter, checkpoints: seq[string], _: string) {.gcsafe.} =
  # only include the printed values
  output = filterIt(checkpoints, "Check failed:" notin it)
  wasFailure = true

# we use our own formatter:
resetOutputFormatters()
addOutputFormatter(MockFormatter())

template verify(o: varargs[string]) =
  ## Compares the output of the previous ``check`` failure against `o`.
  doAssert wasFailure, "check didn't fail"
  doAssert output == o, "got: " & $output
  wasFailure = false

proc modify(x: var int): var int =
  inc x
  x

# --- the actual test cases

var a = 0
check a == modify(a)
verify "a was 0", "modify(a) was 1"

check (inc a; a) == (inc a; a)
verify "\ninc a\na was 2", "\ninc a\na was 3"

check [modify(a)] == [modify(a)]
verify "[modify(a)] was [4]", "[modify(a)] was [5]"

check (modify(a),) == (modify(a),)
verify "(modify(a),) was (6,)", "(modify(a),) was (7,)"

# reset the program result so that no test failure is reported
setProgramResult(0)