discard """
  description: '''
    Identical number of matches in each argument overload category leads to an
    ambiguous overload error.
  '''
  errormsg: "ambiguous call; both t01_overload_core_ambiguous_fail.impl2(exact: uint8, conv: string) [proc declared in t01_overload_core_ambiguous_fail.nim(13, 6)] and t01_overload_core_ambiguous_fail.impl2(conv: string, exact: uint8) [proc declared in t01_overload_core_ambiguous_fail.nim(14, 6)] match for: (uint8, uint8)"
"""

# TODO: this is a converter related test and should not be in s02_core

converter toString(i8: uint8): string = $i8

proc impl2(exact: uint8, conv: string): string = "exact+conv"
proc impl2(conv: string, exact: uint8): string = "widen+widen"

doAssert impl2(0u8, 0u8) == "exact+conv"