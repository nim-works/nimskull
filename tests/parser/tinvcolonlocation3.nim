discard """
  errormsg: "expected: ':', but got: 'echo'"
  file: "tinvcolonlocation3.nim"
  line: 12
  column: 3
"""
try:
  echo "try"
except:
  echo "except"
finally #<- missing ':'
  echo "finally"
