
discard """
  action: reject
  cmd: "nim check $options --hints:off $file"
  description: '''
    Test detection of invalid control-flow across the compile-/run-time
    boundary
  '''
  knownIssue: '''
    yield and return crossing the compile-/run-time boundary are not
    rejected
  '''
"""

# XXX: integrate these test cases into ``tinvalid_control_flow.nim`` once they
#      start working

block return_statement:
  proc p() =
    # initializer of constant:
    const c = (if true: return; 1) #[tt.Error
                        ^ 'return' not allowed here]#
    # static expression:
    var a = static(if true: return; 1) #[tt.Error
                            ^ 'return' not allowed here]#
    # static statement:
    static:
      return #[tt.Error
      ^ 'return' not allowed here]#

block yield_statement:
  iterator p(): int =
    # initializer of constant:
    const c = (if true: yield 1; 1) #[tt.Error
                        ^ 'yield' only allowed in an iterator]#
    # static expression:
    var a = static(if true: yield 1; 1) #[tt.Error
                            ^ 'yield' only allowed in an iterator]#
    # static statement:
    static:
      return #[tt.Error
      ^ 'yield' only allowed in an iterator]#
