discard """
  outputsub: '''assertion failure!
this shall be always written
'''
  exitcode: "1"
"""
# test assert and exception handling

proc callB() = assert(false)
proc callA() = callB()
proc callC() = callA()

try:
  callC()
except AssertionDefect:
  echo "assertion failure!"
except:
  echo "unknown exception!"
finally:
  echo "this shall be always written"

assert(false) #OUT assertion failure!this shall be always written
