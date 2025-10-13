discard """
  description: '''
    Taking the address of a location storing a procedural value does not
    incur the procedure's effects.
  '''
  action: compile
"""

proc p() {.raises: [].} =
  var a: proc() {.raises: ValueError.}
  discard addr(a)
