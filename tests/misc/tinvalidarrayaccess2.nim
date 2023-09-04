discard """
  errormsg: "index 3 not in 0 .. 1"
  knownIssue: '''
    the conversion in the array-index position is folded first, resulting in a
    range error instead of an index error
  '''
  line: 13
"""

# Note: merge in tinvalidarrayaccess.nim pending https://github.com/nim-lang/Nim/issues/9906

let a = [1,2]
echo a[3]

