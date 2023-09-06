discard """
  targets: "c js vm"
  knownIssue.js vm: "unfolded 'sizeof' calls are not supported"
  output: '''@[(x: 3, y: 4)]'''
"""

type
  mypackage.Foo = object
  Other = proc (inp: Foo)

# can only be computed at run-time:
doAssert sizeof(Foo) == sizeof((int, int))
doAssert alignof(Foo) == alignof(int)

import definefoo

# after this import, Foo is a completely resolved type, so
# we can create a sequence of it:
var s: seq[Foo] = @[]

# the size is also statically known now:
static:
  doAssert sizeof(Foo) == sizeof((int, int))
  doAssert alignof(Foo) == alignof(int)

s.add Foo(x: 3, y: 4)
echo s
