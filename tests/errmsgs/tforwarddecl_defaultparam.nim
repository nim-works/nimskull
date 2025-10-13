discard """
errormsg: "overloaded 'reciprocal' leads to ambiguous calls"
line: 9
"""

# Differing float literal default args must prevent forward declaration
# and the compiler must not compare them via float equality
proc reciprocal(f: float = 0.0): float
proc reciprocal(f: float = -0.0): float = 1 / f
