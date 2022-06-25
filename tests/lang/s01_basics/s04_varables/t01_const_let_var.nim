discard """
description: '''
The very basics of const, let, and var
'''
"""

# assign a value to `foo`, it's set once at a compile time
# and is therefore immutable
const foo = 1

# assign a value to `bar`, it's set once at runtime
# and is therefore immutable
let bar = 2

# assign a value to `baz`, it can be changed at runtime
# and is therefore mutable
var baz = 2
baz = 3

doAssert foo == 1, "check const value is 1"
doAssert bar == 2, "check let value is 2"
doAssert baz == 3, "check var value is 3"

# a var does not have to be initialized, but it needs a type, and will figure
# out the rest
var zaf: int

doAssert zaf == 0, "uninitialized int var is initialized to zero"