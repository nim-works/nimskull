discard """
  targets: "native"
  output: '''
foo
bar
'''
"""

proc test(strings: seq[string]) =
  for s in strings:
    var p3 = addr(s)
    echo p3[]

test(@["foo", "bar"])
