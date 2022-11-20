discard """
  output: '''
foo
bar
'''
  description: ''''
    . From https://github.com/nim-lang/Nim/issues/8630
      Cannot use unsafeAddr on an element from for loop
  '''
"""

proc test(strings: seq[string]) =
  for s in strings:
    var p3 = addr(s)
    echo p3[]

test(@["foo", "bar"])