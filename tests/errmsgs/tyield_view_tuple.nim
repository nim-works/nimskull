discard """
  nimoutFormat: sexp
  cmd: "nim check --msgFormat=sexp --filenames=canonical --hints:off $options $file"
  action: reject
"""

block tuple_with_var:
  proc make(x: var int): (int, var int) =
    ## Helper procedure -- not relevant to the test
    (0, x)

  iterator iter(): (int, var int) =
    var x = 0
    # this doesn't compile because the expression is not a literal tuple
    # constructor
    yield make(x) #[tt.Error
            ^ (SemYieldExpectedTupleConstr) ]#

block tuple_with_lent:
  proc make2(x: int): (int, lent int) =
    (0, x)

  iterator iter2(): (int, lent int) =
    var x = 0
    # same as for the previous test case
    yield make2(x) #[tt.Error
              ^ (SemYieldExpectedTupleConstr) ]#