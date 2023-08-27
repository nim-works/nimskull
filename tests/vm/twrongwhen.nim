discard """
  errormsg: "accessed location 'x' doesn't exist in the current compile-time context"
  line: 7
"""

proc bla(x:int) =
  when x == 0:
    echo "oops"
  else:
    echo "good"

bla(2)  # echos "oops"

