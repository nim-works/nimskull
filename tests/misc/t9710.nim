discard """
  targets: "c cpp"
  matrix: "--debugger:native"
"""

# xxx: js codegen breaks

# bug #9710
for i in 1 || 200:
  discard i
