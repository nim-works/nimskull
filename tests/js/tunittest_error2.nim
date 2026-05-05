discard """
  exitcode: 1
  outputsub: '''
[<foreign exception>]
[FAILED] Bad test
  '''
  matrix: "-d:nodejs"
  targets: "js"
  joinable: false
"""

# bug #16978
import std/unittest
test "Bad test":
  var x: cstring = nil
  let y = x[0]
