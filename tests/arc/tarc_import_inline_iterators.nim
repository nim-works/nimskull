discard """
  cmd: "nim c --gc:arc $file"
  joinable: false
  description: '''
      . From https://github.com/nim-lang/Nim/issues/14864
        [ARC] C compiler error with inline iterators and imports
      . FWIW changing openArray to a seq does not solve the issue.
      . Fixed in devel after 2020-07-04. Added Test Case.
  '''
"""

import bmodule