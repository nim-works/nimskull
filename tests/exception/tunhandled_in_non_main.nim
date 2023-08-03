discard """
  targets: "c js vm"
  description: '''
    An unhandled exception not raised inside the main module must also
    terminate the program
  '''
  joinable: false
  outputsub: "Error: unhandled exception: failure [CatchableError]"
  exitcode: 1
"""

import munhandled_in_non_main

# an unhandled exception was raised when initializing the imported module;
# execution must not reach here
e.msg = "unreachable"