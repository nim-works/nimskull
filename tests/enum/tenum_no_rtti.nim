discard """
  output: '''A
B'''
  matrix: "--gc:arc"
  targets: "native"
"""
type
  Enum = enum A, B, C
  EnumRange = range[A .. B]
proc test_a(x: Enum): string = $x
proc test_b(x: EnumRange): string = $x
echo test_a(A)
echo test_b(B)
