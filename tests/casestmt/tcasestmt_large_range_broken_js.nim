discard """
target: js
description: '''shows broken js code gen for large ranges in a case branch, see
`tcasestmt` for the same test and issue reference #11551
'''
knownIssue: "can't generate code for case branch with large range in js"
"""

#issue #11551

proc negativeOrNot(num: int): string =
  result = case num
  of low(int) .. -1:
    "negative"
  else:
    "zero or positive"

doAssert negativeOrNot(-1) == "negative"
doAssert negativeOrNot(10000000) == "zero or positive"
doAssert negativeOrNot(0) == "zero or positive"