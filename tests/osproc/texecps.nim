discard """
joinable: false
matrix: "--lib:lib"
description: '''This test runs itself as a set of parallel processes in order
to test `osproc.execProcesses`.'''
"""

import osproc, streams, strutils, os

const NumberOfProcesses = 13

var
  gSetup {.threadvar.}: seq[int]
  gStartupPid {.threadvar.}: seq[int]
  gAfterPid {.threadvar.}: seq[int]
  gResults {.threadvar.}: seq[string]

proc beforeRun(idx: int) =
  # called before a process is started
  gSetup[idx] = idx

proc startRun(idx: int, p: Process) =
  # process just started
  gStartupPid[idx] = p.processID

proc afterRun(idx: int, p: Process) =
  let exitCode = p.peekExitCode
  if exitCode < len(gResults):
    gResults[exitCode] = p.outputStream.readAll.strip
  gAfterPid[idx] = p.processID

when true:
  if paramCount() == 0:
    gSetup = newSeq[int](NumberOfProcesses)
    gStartupPid = newSeq[int](NumberOfProcesses)
    gAfterPid = newSeq[int](NumberOfProcesses)
    gResults = newSeq[string](NumberOfProcesses)
    var
      checks = newSeq[string](NumberOfProcesses)
      commands = newSeq[string](NumberOfProcesses)
    for i in 0..len(commands) - 1:
      commands[i] = getAppFileName() & " " & $i
      checks[i] = $i
    let cres = execProcesses(commands, options = {poStdErrToStdOut},
                              beforeRunEvent = beforeRun,
                              startRunEvent = startRun,
                              afterRunEvent = afterRun)
    doAssert(cres == len(commands) - 1)
    doAssert(gResults == checks)
    doAssert(gSetup == @[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])
    doAssert(gStartupPid == gAfterPid)
  else:
    echo paramStr(1)
    programResult = parseInt(paramStr(1))
