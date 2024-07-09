discard """
  description: '''
    Ensure instantiating foreign, incomplete generic procedures leads to a
    proper error.
  '''
  errormsg: "cannot instantiate generic procedure forward-declared in another module"
  file: "mgeneric_cycle_forward.nim"
  line: 5
"""

proc forwarded*[T]()

# instantiate with `int` before starting the import cycle
forwarded[int]()

import mgeneric_cycle_forward # start the recursive import

# complete the forward declaration:
proc forwarded[T]() =
  discard
