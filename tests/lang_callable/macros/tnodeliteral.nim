discard """
  description: "Tests for passing node literals (AST) around"
"""

import std/macros

block node_literals_are_opaque_in_static_analysis:
  block regression_ignore_node_literals_in_lambdalifting:
    proc sym() =
      discard

    proc outer() {.compileTime.} =
      var x = 0

      proc sym() {.compileTime.} =
        # close over an outer local in order to make `sym` a closure procedure:
        x = 0

      proc inner() {.compileTime.} =
        let x = bindSym"sym" # create a symbol choice that includes the closure
                            # procedure

      # `inner` doesn't close over a local and also doesn't access a closure
      # procedure, so it must not be a closure procedure
      doAssert inner isnot "closure"

    static:
      # call the procedure to make sure it passes the lambda-lifting pass
      outer()
