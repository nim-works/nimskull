discard """
  description: '''
    Make sure that warnings are reported for suspicious, implicit conversions
    of the `in` operand.
  '''
  action: compile
"""

var x = 1

# in both cases, there be no issue at run-time, but `x` could be very well be
# some arbitrary value

discard x in {1, 2} #[tt.Hint
        ^ suspicious code]#

discard contains({1, 2}, x) #[tt.Warning
                         ^  suspicious code]#