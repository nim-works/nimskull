discard """
description: '''
Stropping; how you can escape reserved/keywords and use them as identifiers
using backticks
'''
"""

let `if` = "what if this was allowed, it is!"

doAssert `if` == "what if this was allowed, it is!",
  "backticks escape reserved identifiers"