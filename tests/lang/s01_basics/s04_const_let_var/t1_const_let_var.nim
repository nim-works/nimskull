discard """
description: '''the very basics of const, let, and var'''
targets: "c cpp js"
"""

# assign a value to `foo`, it's set once at a compile time
const foo = 1

# assign a value to `bar`, it's set once at runtime
let bar = 2

# assign a value to `baz`, it's can be changed at runtime
var baz = 2
baz = 3

doAssert foo == 1, "check const value is 1"
doAssert bar == 2, "check let value is 2"
doAssert baz == 3, "check var value is 3"

# a var does not have to be initialized, but it needs a type, and will figure
# out the rest
var zaf: int

doAssert zaf == 0, "uninitialized int var is initialized to zero"