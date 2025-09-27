discard """
  description: '''
    Tests for non-ast varargs, those that are not `un/typed`, with subtyping.
  '''
  knownIssue: "varargs can't handle range conversions"
"""


block subtype_match_ranges:
  type Foo = range[0..5]

  func bar(v: varargs[Foo], values: openarray[Foo]) =
    doAssert v.len == values.len
    for i, a in v.pairs:
      doAssert a == values[i], $a & " is not equal to " & $values[i]
  
  bar(0, [Foo 0])
  bar(0, 1, [Foo 0, 1])
  bar(newArray[Foo]()) # should match, `v` will be empty, and `values` the empty array

  block out_of_range_should_not_match:
    func baz(v: varargs[Foo], a, b: int) =
      doAssert v.len == 0
      doAssert a == -1
      doAssert b == 6
    
    doAssert not compiles(baz(-1, 6))
