discard """
  targets: "c cpp"
  description: '''should work for js as well, see:
`tcompile_time_var_at_runtime_js`
'''
  output: "1\n2\n2\n3"
"""
var a {.compileTime.} = 1

echo a
a = 2
echo a
echo a
a = 3
echo a 