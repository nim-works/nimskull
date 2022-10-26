discard """
  errormsg: "invalid type: 'openArray[int]' for result"
  line: 12
  description: '''
    . From https://github.com/nim-lang/Nim/issues/8259
      ICE's and bugs with openarray
    .`Auto` sidesteps the check enforcing that no procedure returns
      an open array
'''
"""

proc foo(a: openArray[int]):auto = a
echo foo(toOpenArray([1, 2], 0, 2))

