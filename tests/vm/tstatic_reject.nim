discard """
description: '''
Unhandled exception at compile-time
'''
action: reject
"""

static:
  doAssert false, "Static reject"
