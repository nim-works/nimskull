discard """
  description: '''
    Ensure that an error is reported when attempting to create special
    atom nodes with `newNimNode`
  '''
  matrix: "--errorMax:4"
  action: reject
"""

import std/macros

static:
  discard newNimNode(nnkError) #[tt.Error
                    ^ cannot manually create a node of kind: nnkError]#

static:
  discard newNimNode(nnkIdent) #[tt.Error
                    ^ cannot manually create a node of kind: nnkIdent]#

static:
  discard newNimNode(nnkSym) #[tt.Error
                    ^ cannot manually create a node of kind: nnkSym]#

static:
  discard newNimNode(nnkType) #[tt.Error
                    ^ cannot manually create a node of kind: nnkType]#
