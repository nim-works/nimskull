discard """
  description: '''
    Regression test for a compiler crash caused by the JavaScript code
    generator
  '''
"""

# every array length > 32 caused the crash
var global: array[33, (int, int)]
