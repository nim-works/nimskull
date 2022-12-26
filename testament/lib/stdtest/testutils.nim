import std/private/miscdollars
when defined(nimscript):
  import std/os # xxx investigate why needed
else:
  from std/os import getEnv
import std/[macros, genasts]

template flakyAssert*(
    cond: untyped, msg = "", notifySuccess = true): untyped =
  ## API to deal with flaky or failing tests. This avoids disabling entire
  ## tests altogether so that at least the parts that are working are kept
  ## being tested. This also avoids making CI fail periodically for tests
  ## known to be flaky. Finally, for known failures, passing `notifySuccess
  ## = true` will log that the test succeeded, which may indicate that a
  ## bug was fixed "by accident" and should be looked into.
  const info = instantiationInfo(-1, true)
  const expr = astToStr(cond)
  if cond and not notifySuccess:
    discard # silent success
  else:
    var msg2 = ""
    toLocation(msg2, info.filename, info.line, info.column)
    if cond:
      # a flaky test is failing, we still report it but we don't fail CI
      msg2.add " FLAKY_SUCCESS "
    else:
      # a previously failing test is now passing, a pre-existing bug might've been
      # fixed by accidend
      msg2.add " FLAKY_FAILURE "
    msg2.add $expr & " " & msg
    echo msg2

when not defined(js) and not defined(nimscript):
  import std/strutils

  import hmisc/core/all
  proc greedyOrderedSubsetLines*(
      lhs, rhs: string, allowPrefixMatch: bool = false): bool =
    ## Returns true if each stripped line in `lhs` appears in rhs, using a
    ## greedy matching.
    # TODO improve error reporting by showing the last matched pair
    proc isMatch(lhsi, rhsi: string): bool =
      if allowPrefixMatch:
        startsWith(rhsi, lhsi):
      else:
        lhsi == rhsi

    let rhs = rhs.strip().splitLines()
    let lhs = lhs.strip().splitLines()
    # echov lhs
    # echov rhs
    var rhsIdx = 0

    var currentLine = strip(rhs[rhsIdx])
    for lhsIdx, line in lhs:
      let line = line.strip()
      if line.len() != 0:
        # Search for the line in RHS
        while not isMatch(line, currentLine):
          inc rhsIdx
          if rhs.len() <= rhsIdx:
            return false

          else:
            currentLine = strip(rhs[rhsIdx])

      if rhs.len() <= rhsIdx:
        # Some lines from the lhs weren't matched to the RHS -- since we
        # are in the loop it is guaranteed that `lhsIdx < lhs.len()`
        return false

    return true

template enableRemoteNetworking*(): bool =
  ## Allows contolling whether to run some test at a statement-level
  ## granularity. Using environment variables simplifies propagating this
  ## all the way across process calls, e.g. `testament all` calls itself,
  ## which in turns invokes a `nim` invocation (possibly via additional
  ## intermediate processes).
  getEnv("NIM_TESTAMENT_REMOTE_NETWORKING") == "1"

template whenRuntimeJs*(bodyIf, bodyElse: untyped): untyped =
  ##[
  Behaves as `when defined(js) and not nimvm` (which isn't legal yet).
  pending improvements to `nimvm`, this sugar helps; use as follows:

  whenRuntimeJs:
    doAssert defined(js)
    when nimvm: doAssert false
    else: discard
  do:
    discard
  ]##
  when nimvm: bodyElse
  else:
    when defined(js): bodyIf
    else: bodyElse

template whenVMorJs*(bodyIf, bodyElse: untyped): untyped =
  ## Behaves as: `when defined(js) or nimvm`
  when nimvm: bodyIf
  else:
    when defined(js): bodyIf
    else: bodyElse

template accept*(a: untyped): untyped =
  doAssert compiles(a)

template reject*(a: untyped): untyped =
  doAssert not compiles(a)

template disableVm*(body: untyped): untyped =
  when nimvm: discard
  else: body

macro assertAll*(body: untyped): untyped =
  ## works in VM, unlike `check`, `require`
  runnableExamples:
    assertAll:
      1+1 == 2
      var a = @[1, 2] # statements work
      a.len == 2
  # remove this once these support VM, pending #10129 (closed but not yet fixed)
  result = newStmtList()
  for a in body:
    result.add genAst(a, a2 = a.repr, info = lineInfo(a)) do:
      # D20210421T014713:here
      # xxx pending https://github.com/nim-lang/Nim/issues/12030,
      # `typeof` should introduce its own scope, so that this
      # is sufficient: `typeof(a)` instead of `typeof(block: a)`
      when typeof(block: a) is void: a
      else:
        if not a:
          raise newException(AssertionDefect, info & " " & a2)
