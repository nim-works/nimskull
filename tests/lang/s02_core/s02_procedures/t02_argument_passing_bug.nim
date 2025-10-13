discard """
  description: "A separate test for a bug with varargs handling"
  knownIssue: "https://github.com/nim-works/nimskull/issues/1375"
"""

# XXX: merge back into ``t02_argument_passing`` once the below succeeds

# right now results in `(@[1, 2, 0], @[true, false, false])`
doAssert impl(1, 2, false) == (@[1, 2], @[false])
