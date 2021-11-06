discard """
description: '''
Test covers compilation error caused by illegal recursion in object 
definition. 
'''

errormsg: '''
illegal recursion in type 'Object'
'''
"""


## It is possible for an object to contain itself recursively, but only 
## if it was defined or used as a `ref`/`ptr`
## 
## Regular objects are embedded directly in the resulting structure, so
## if an object was to be 'embedded' in itself, it would have an infinite size.
## Using `ptr/ref` solves this issue - they have a fixed size.

block with_ref_object:
  ## No error here because using `ref object`
  type
    Object = ref object
      field: Object
      ## Ref is 8 byte referenced pointer; therefore object is 8 bytes in size
  
  assert sizeof(Object) == 8

block with_ptr_or_ref:
  ## No error here because using `ptr T` in fields
  type
    Object = object
      refField: ref Object
      ptrField: ptr Object
      ## Ref and ptr are both 8 bytes; therefore object is 16 bytes in size

  assert sizeof(Object) == 16

block invalid_recursion:
  type
    Object = object
      field: Object
      ## Recursive embedding results in an undefined/infinite sized object
      ## This is invalid