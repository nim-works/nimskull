discard """
  description: "Access violations that aren't caught"
  nimoutFormat: sexp
  cmd: "nim check --msgFormat=sexp --filenames=canonical $options $file"
  action: reject

  knownIssue: '''int/float/NimNode/ptr values are stored directly in registers.
                 `addr` produces a `rkRegAddr` for these, which isn't
                 validated on read-through/write-through'''
"""

import std/macros
import maccess_checks

# XXX: once these tests fail, move them to `tsafety_checks`

template testLocal(t, code) =
  static:
    # needs to be wrapped in a proc since writing a pointer of the above
    # mentioned types to a global is currently forbidden (due to `rkRegAddr`)
    proc wrapper() =
      # p is a pointer to a local from a destroyed stack frame
      let p {.inject.} = localPtr(t)
      code

    wrapper()

testLocal(int): discard p[] #[tt.Error
                        ^ (VMAccessOutOfBounds)]#

testLocal(int): p[] = 1 #[tt.Error
                ^ (VMAccessOutOfBounds)]#

testLocal(float): discard p[] #[tt.Error
                          ^ (VMAccessOutOfBounds)]#

testLocal(float): p[] = 1.0 #[tt.Error
                  ^ (VMAccessOutOfBounds)]#

testLocal(ptr int): discard p[] #[tt.Error
                            ^ (VMAccessOutOfBounds)]#

testLocal(ptr int): p[] = nil #[tt.Error
                    ^ (VMAccessOutOfBounds)]#

testLocal(pointer): discard p[] #[tt.Error
                            ^ (VMAccessOutOfBounds)]#

testLocal(pointer): p[] = nil #[tt.Error
                    ^ (VMAccessOutOfBounds)]#

testLocal(NimNode): discard p[] #[tt.Error
                            ^ (VMAccessOutOfBounds)]#

testLocal(NimNode): p[] = newEmptyNode() #[tt.Error
                    ^ (VMAccessOutOfBounds)]#
