discard """
  action: reject
  cmd: "nim check $options --hints:off $file"
  description: '''
    Test detection of invalid control-flow across the compile-/run-time
    boundary
  '''
"""

# control flow across the compile-/run-time boundary cannot work, and is thus
# disallowed

block unlabeled_break:
  while true:
    # initializer of constant:
    const c = (if true: break; 1) #[tt.Error
                        ^ invalid control flow: break]#
    # static expression:
    var a = static(if true: break; 1) #[tt.Error
                            ^ invalid control flow: break]#
    # static statement:
    static:
      break #[tt.Error
      ^ invalid control flow: break]#

block labeled_break:
  # initializer of constant:
  const c = ((if true: break labeled_break); 1) #[tt.Error
                       ^ invalid control flow: labeled_break]#
  # static expression:
  var a = static(if true: break labeled_break; 1) #[tt.Error
                          ^ invalid control flow: labeled_break]#
  # static statement:
  static:
    break labeled_break #[tt.Error
    ^ invalid control flow: labeled_break]#

block continue_statement:
  while true:
    # initializer of constant:
    const c = (if true: continue; 1) #[tt.Error
                        ^ invalid control flow: continue]#
    # static expression:
    var a = static(if true: continue; 1) #[tt.Error
                            ^ invalid control flow: continue]#
    # static statement:
    static:
      continue #[tt.Error
      ^ invalid control flow: continue]#

# a static expression/statement or constant expression nested inside another
# compile-time context happens at a different phase/stage, meaning that
# control-flow crossing from one into the other is also invalid

block break_across_compile_time_contexts:
  static:
    while true:
      # initializer of constant:
      const c = (if true: break; 1) #[tt.Error
                          ^ invalid control flow: break]#
      # static expression:
      var a = static(if true: break; 1) #[tt.Error
                              ^ invalid control flow: break]#
      # static statement:
      static:
        break #[tt.Error
        ^ invalid control flow: break]#