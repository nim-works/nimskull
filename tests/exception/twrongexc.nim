discard """
  targets: "c cpp"
  outputsub: "Error: unhandled exception:  [ValueError]"
  exitcode: "1"
"""

# xxx: this should work for JS, but exception reporting is inconsistent

try:
  raise newException(ValueError, "")
except OverflowDefect:
  echo("Error caught")
