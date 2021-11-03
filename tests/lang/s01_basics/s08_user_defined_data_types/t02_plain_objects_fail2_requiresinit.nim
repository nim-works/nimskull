discard """
description: '''
By default object's fields are initialized to default values. To requires explicit
initialization of a particular field it can be annotated with `{.requiresinit.}`
'''

errormsg: "The Object type requires the following fields to be initialized: field2."
"""

block no_required_field:
  type
    Object = object
      field1: int
      field2: int
      field3: int


  ## Omitted fields were initialized to their default values
  let it = Object(field1: 123)

  doAssert it.field1 == 123
  doAssert it.field2 == 0
  doAssert it.field3 == 0

block has_required_field:
  type
    Object =  object
      field1: int
      field2 {.requiresinit.}: int
      field3: int

  ## `field2` was explicitly initialized to some value, no compilation error
  let it = Object(field1: 123, field2: 2)


  ## No value assigned on the field on construction - compilation fails
  let it2 = Object(field1: 2)


  ## Note that `requiresinit` annotation will propagate - for example
  type
    Wrapper = object
      field: Object


  ## Compilation also fails due to
  ## `Error: The Wrapper type requires the following fields to be initialized: field.`
  ## even though there was no explicit annotation for the field itself
  let wrap = Wrapper()
