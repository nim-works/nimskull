discard """
  errormsg: "expected: ':', but got: 'echo'"
  file: "tinvcolonlocation1.nim"
  line: 8
  column: 3
"""
try #<- missing ':'
  echo "try"
except:
  echo "except"
finally:
  echo "finally"
