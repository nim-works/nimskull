discard """
  labels: "module generic scope"
  output: '''100
200'''
"""

# https://github.com/nim-lang/Nim/issues/11225

import
  module_using_generic_library

makeUseOfLibrary "test"
