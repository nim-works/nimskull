discard """
  output: "1"
  matrix: "--hints:on -d:release"
  ccodecheck: "'NI volatile state;'"
  targets: "c"
"""

# bug #1539

proc err() =
  raise newException(Exception, "test")

proc main() =
  var state: int
  try:
    state = 1
    err()
  except:
    echo state

main()
