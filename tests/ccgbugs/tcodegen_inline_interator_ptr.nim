discard """
labels: "array backend_c codegen inline iterator pragma ptr"
description: '''
  . From https://github.com/nim-lang/Nim/issues/8616
    Incomplete types being generated in C code from ptr to unchecked array
  . Don't leave dangling forward references to types.
'''
"""

import pkg8616 / scheduler
when true:
  init()