discard """
  targets: "c js !vm"
  output: '''(v: 42)
igotdestroyed'''
"""

# knownIssue: destructors for top-level globals are not yet supported by the
#             VM backend

import objFile

echo test
