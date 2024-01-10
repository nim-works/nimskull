discard """
  outputsub: '''
false
Within finally->try
'''
  exitCode: 1
  knownIssue.vm: '''
    Exception/finally handling is largely disfunctional in the VM
  '''
"""
# Test break in try statement:

proc main: bool =
  while true:
    try:
      return true
    finally:
      break
  return false

echo main() #OUT false

# bug https://github.com/nim-lang/nim/issues/5871
try:
  raise newException(Exception, "First")
finally:
  try:
    raise newException(Exception, "Within finally->try")
  except:
    echo getCurrentException().msg
