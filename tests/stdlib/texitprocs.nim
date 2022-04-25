discard """
targets: "c !cpp js"
output: '''
ok4
ok3
ok2
ok1
'''
"""

# xxx: this should work in CPP, it's a knownIssue, see PR:
#      https://github.com/nim-works/nimskull/pull/290

import std/exitprocs

proc fun1() {.noconv.} = echo "ok1"
proc fun2() = echo "ok2"
proc fun3() {.noconv.} = echo "ok3"
proc fun4() = echo "ok4"

addExitProc(fun1)
addExitProc(fun2)
addExitProc(fun3)
addExitProc(fun4)
