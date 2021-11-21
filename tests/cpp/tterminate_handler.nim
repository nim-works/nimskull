discard """
  targets: "cpp"
  outputsub: "Error: unhandled unknown cpp exception"
  exitcode: 1
  knownIssue: "https://github.com/nim-lang/Nim/issues/13695"
"""
type Crap {.importcpp: "int".} = object

var c: Crap
raise c
