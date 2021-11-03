discard """
description: '''
Test shows and explains compilation error caused by illegal recursion in object 
definition. 
'''

errormsg: '''
illegal recursion in type 'Object'
'''
"""


## It is possible to use object recursively in itself, but only 
## if it was defined or used as a `ref`/`ptr`
## 
## Regular objects are embedded directly in the resulting structure, so
## if object was to be 'embedded' in itself, it would have an infinite size.
## Using `ptr/ref` solves this issue - they have a fixed size.

block with_ref_object:
  ## No error here because using `ref object`
  type
    Object = ref object
      field: Object

block with_ptr_or_ref:
  ## No error here because using `ptr T` in fields
  type
    Object = object
      refField: ref Object
      ptrField: ptr Object


block:
  type
    Object = object
      field: Object
    