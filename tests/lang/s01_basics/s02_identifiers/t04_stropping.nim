discard """
description: '''
Stropping, or how you can escape reserved/keywords and use them as identifiers
'''
"""

let `if` = "what if this was allowed, it is!"

doAssert `if` == "what if this was allowed, it is!",
  "backticks escape reserved identifiers"