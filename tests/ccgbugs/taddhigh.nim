discard """
  output: '''@[5, 5, 5, 5, 5]'''
  matrix: "--gc:refc; --gc:orc"
"""

# TODO: the test applies to all code-generators -- move it somewhere else

# bug #1832

var s = @[5]

# Works fine:
let x = s[s.high]
s.add x

# Causes the 0 to appear:
s.add s[s.high]

s.add s[s.len-1]
s.add s[s.len-1]

echo s # @[5, 5, 0]
