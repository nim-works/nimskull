discard """
  errormsg: "index 1 not in 0 .. 0"
  line: 10
"""

# regression test for #1604: indexing a constant string literal with an
# out-of-bounds constant index used to crash the compiler with a FieldDefect
# while building the report

discard "a"[1]
