discard """
  targets: "c cpp"
  errormsg: "index 3 not in 0 .. 1"
  line: 10
"""

# Note: merge in tinvalidarrayaccess.nim pending https://github.com/nim-lang/Nim/issues/9906

let a = [1,2]
echo a[3]

