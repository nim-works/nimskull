discard """
labels : "error_runtime ref set"
description: '''
  . From https://github.com/nim-lang/Nim/issues/9098
    Runtime Error in peg function
  . Taking the LHS type when a temporary result value was
    needed lead to bad code being generated if we get a tyRef.
  . Turns out the + operator for ref set triggers bad codegen
'''
"""

var x = new(ref set[char])
var y = new(ref set[char])
x[] = {'a'}
y[] = {'b'}

let representation =  $( x[] + y[] )
doAssert representation == "{'a', 'b'}"