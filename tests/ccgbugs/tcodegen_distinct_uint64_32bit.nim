discard """
labels: "32_bit distinct var"
description: '''
  . From https://github.com/nim-lang/Nim/issues/13902
    "distinct uint64" type corruption on 32-bit, when using {.borrow.} operators
  . The problem only appears when borrowed operators for a "distinct uint64"
    type force a type inference.
  . Test was failing with either 0 or 2 echos but not with 1 echo
'''
"""

block:
  type Slot = distinct uint64
  var s = Slot(1)
  proc `$`(x: Slot): string {.borrow.}
  proc `+=`(x: var Slot, y: uint64) {.borrow.}

  s += 1

  doAssert s.uint64 == 2, $s # was failing, showing 18419607611339964418