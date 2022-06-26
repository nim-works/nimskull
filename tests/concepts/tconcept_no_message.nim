discard """
errormsg: "undeclared identifier: 'x'"
line: 17
description: '''
  . From https://github.com/nim-lang/Nim/issues/4982
    Compilation breaks without showing an error
  . Trying to compile the following (nonsense) code snippet
    leads to the compiler exiting with status 1, but without
    giving any error or other hint on what might be the problem.
'''
"""

import typetraits # without this import the program compiles (and echos false)

type
  SomeTestConcept = concept t
    x.name is string # typo: t.name was intended (which would result in echo true)

type
  TestClass = ref object of RootObj
    name: string

var test = TestClass(name: "mytest")
echo $(test is SomeTestConcept)