discard """
  description: '''
    Tests for making sure implicit borrows living across a suspend are
    rejected.
  '''
"""

proc get(x: int, other: int): lent int =
  x

proc test() =
  var x = @[1, 2, 3]
  discard get(x[0]) do: #[tt.Error
               ^ borrow 'x[0]' lives beyond a 'suspend', which is forbidden]#
    suspend void, _:
      discard
    1

test()
