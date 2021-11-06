discard """
description: '''
Covers basic features of objects:
- definition
- construction
- field access
- field modification
- field copying
'''
"""

block inlineSimpleObject:
  type SimpleInline = object
    field1: int
    field2: int

block simple_object:
  ## Object can be defined in a `type` section using `TypeName = object`
  ## followed by listing all fields
  type
    Simple = object
      field1: int
      field2: int

  ## Object can be constructed/instantiated using `TypeName(field: value, ...)` syntax.
  ## Fields that were not explicitly listed are set to their default values.
  ## To require that a particular field is explicitly initialized when instantiated
  ## you can annotate it with `{.requiresinit.}` (for example of this see
  ## `t02_plain_object_requiresinit.nim`)
  let it = Simple(field1: 12)

  ## To get the value of a particular field, use `value.field` syntax
  let field1Value = it.field1

  doAssert field1Value == 12
  doAssert it.field1 == 12

  ## Fields that were not explicitly listed are set to their default values (depends
  ## on types default value)
  doAssert it.field2 == 0

  ## Order of field initialization is not important
  let it1 = Simple(field1: 2, field2: 3)
  let it2 = Simple(field2: 3, field1: 2)

  doAssert it1.field1 == it2.field1
  doAssert it2.field2 == it2.field2

block copying_defaulting:
  ## It is possible to create an object variable without explicitly initializing
  ## it. In that case all fields would be initialized to default state (just like
  ## explicit initialization where omitted fields were defaulted)
  type
    Object = object
      field1: int
      field2: int

  var defaulted: Object

  doAssert defaulted.field1 == 0
  doAssert defaulted.field2 == 0


  ## Object can be copied into another variable
  var copy = defaulted

  ## Modifying copy does not affect original value
  copy.field1 = 12

  doAssert copy.field1 == 12
  doAssert defaulted.field1 == 0



block no_fields:
  ## It is possible to define object with no fields.
  type
    NoFields = object

  let it = NoFields()

block order_agnostic_declarations:
  ## It is possible to define multiple objects in the same `type` section.
  type
    OtherPre = object
      field1: int
      field2: int

    Object = object
      ## It is possible to put any type as a field value *except* for the same
      ## object type *unless* it was defined as `ref object`. For example of this
      ## see `t02_plain_object_illegal_recursion.nim`
      intField: int

      anonTuple: (int, int)

      objectPre: OtherPre
      objectPost: OtherPost

      araryField: array[5, int]

    OtherPost = object
      field1: int
      field2: int

block mutating_instances:
  ## After construction, an object's field(s) can be modified
  type
    Object = object
      field: int

  var it = Object(field: 12)
  doAssert it.field == 12

  it.field = 228

  doAssert it.field == 228

block ref_object:
  ## If a particular type is almost always used as a reference (`ref T`) it is
  ## possible to declare it as `ref object`.
  type
    Object = ref object
      field1: int
      field2: int

  ## Syntax for using, declaring and assigning ref objects is identical to
  ## regular object.
  let it = Object(field1: 12)

  doAssert it.field1 == 12
  doAssert it.field2 == 0

  ## When ref object variable is constructed without explicit initalization
  ## it follows `ref` semantics - that is, it would be initialized to nil
  ## and modifying copy of an object would result in original being modified
  ## as well
  var defaulted: Object

  ## Defaults to nil
  doAssert isNil(defaulted)

  ## To create new `ref object` use `new` proc. Constructed object will have all
  ## fields initialied to default values
  new(defaulted)

  doAssert defaulted.field1 == 0
  doAssert defaulted.field2 == 0

  var copy = defaulted

  copy.field1 = 12

  doAssert copy.field1 == 12
  doAssert defaulted.field1 == 12
