discard """
  description: '''
    Ensure a view borrowing from the first parameter can be safely returned
    from a procedure.
  '''
  targets: c js vm
  knownIssue.js vm: "The first parameter isn't passed by reference"
"""

block direct_lent_view_primitive:
  # test borrowing from primitive-type parameter with a direct view
  proc test(x: int): lent int =
    x

  var x = 0
  doAssert addr(test(x)) == addr(x)

block object_lent_view_primitive:
  # test borrowing from primitive-type parameter with an indirect view
  type Object = object
    x: lent int

  proc test(x: int): Object =
    Object(x: x)

  var x = 0
  doAssert addr(test(x).x) == addr(x)
