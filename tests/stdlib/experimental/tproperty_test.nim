discard """
  targets: "c js vm"
"""

import experimental/property_test
import experimental/property_testing
import std/strformat

from std/options import isNone, get

# This test file uses the DSL provided by property_test to verify itself.
# Since `spec` calls `quit`, this entire file acts as one big test suite managed by `property_test`.

spec "Property Test DSL":
  
  spec "Basic Generators":
    forAll("int identity", intArb(),
      func(x: int): PTStatus =
        x == x
    )

    forAll("string identity", stringArb(),
      func(s: string): PTStatus =
        s == s
    )

  spec "Tuple Generators":
    forAll("pair identity", intArb(), intArb(),
      func(a, b: int): PTStatus =
        a + b == b + a
    )

    forAll("triple identity", intArb(), intArb(), intArb(),
      func(a, b, c: int): PTStatus =
        (a + b) + c == a + (b + c)
    )

  spec "Failure Reporting (Simulated)":
    # We can't easily assert that a failure *happens* inside the `spec` DSL 
    # because it controls execution.
    # But we can verify `execProperty` returns a failure report manually.
    
    block:
      var ctx = GlobalContext()
      let report = execProperty(
        ctx, 
        "always fails", 
        constArb(1), 
        proc (x: int): PTStatus = ptFail
      )
      if not report.hasFailure:
        echo "Expected failure but got success"
        quit(QuitFailure)
      
      if report.failures == 0:
        echo "Expected failures count > 0"
        quit(QuitFailure)

      # Check counter example
      if report.counterExample.isNone or report.counterExample.get() != 1:
        echo "Expected counter example 1"
        quit(QuitFailure)

  spec "Success Reporting (Simulated)":
    block:
      var ctx = GlobalContext()
      let report = execProperty(
        ctx, 
        "always passes", 
        constArb(1), 
        proc (x: int): PTStatus = ptPass,
        AssertParams(seed: 1, random: newRandom(1), runsBeforeSuccess: 10)
      )
      if report.hasFailure:
        echo "Expected success but got failure"
        quit(QuitFailure)
      
      if report.failures > 0:
        echo "Expected 0 failures"
        quit(QuitFailure)

