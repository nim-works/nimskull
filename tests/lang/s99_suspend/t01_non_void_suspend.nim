discard """
  description: '''
    Providing a non-void type to 'suspend' adds a parameter of that type to
    the continuation. The value passed to the parameter is returned by
    the 'suspend' upon resuming.
  '''
  output: "2\n"
"""

proc test() =
  let x = suspend(int, cont):
    cont[1](2, cont[0])
  echo x

test()
