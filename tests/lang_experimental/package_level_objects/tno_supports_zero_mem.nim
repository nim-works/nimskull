discard """
  description: '''
    Until completed, a package-level object type counts as not supporting
    zero-initialization
  '''
  action: compile
"""

import std/typetraits

type
  mypackage.Foo = object

static:
  doAssert not supportsZeroMem(Foo)
