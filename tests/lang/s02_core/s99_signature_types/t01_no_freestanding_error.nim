discard """
  description: '''
    Signature types can only be constructed in a nominal type definition
    context, as they are nominal types.
  '''
  action: "reject"
"""

proc test(x: int) =
  discard

var x: (signature(Self) do:
  proc test(x: Self)
) = 1
