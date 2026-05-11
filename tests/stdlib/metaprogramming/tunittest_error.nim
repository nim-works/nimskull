discard """
  exitcode: 1
  outputsub: "failed: 1 == 3"
  matrix: "-d:case1; -d:case2"
  targets: "c js"
  joinable: false
"""

when defined case1:
  import std/unittest
  suite "Test":
    test "test require":
      check 1==2
      check 1==3

when defined case2:
  import std/unittest
  suite "Test":
    test "test require":
      require 1 == 3
      if true:
        quit 0 # intentional
