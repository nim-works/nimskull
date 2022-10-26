discard """
labels: "macro proc module"
description: '''
ensure proc annotation typed macros do not leak symbols across modules.
'''
"""

# https://github.com/nim-lang/Nim/issues/18235

import m18235

# this must error out because it was never actually exported
doAssert(not declared(foo))
doAssert not compiles(foo())

doAssert(not declared(foooof))
doAssert not compiles(foooof())

doAssert(not declared(oof))
doAssert not compiles(oof())

# this should have been exported just fine

bar()
barrab()
rab()
baz()