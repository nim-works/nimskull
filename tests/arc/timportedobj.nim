discard """
  description: '''
    Ensure that imported object types with fake fields can be used in types
    that have lifted hooks.
    Derived from https://github.com/nim-lang/nim/issues/13269.
  '''
  targets: "c"
  action: "compile"
"""

type
  Destroy = object
    ## A type with a user-defined destructor.
    value: int

proc `=destroy`(x: var Destroy) =
  discard "body doesn't matter"

type Export {.exportc: "Export", pure.} = object
  a: int

var e: Export # make sure the type is part of the C translation unit

type
  # import the type again
  Import {.importc: "Export".} = object
    # import the field under a different name. If not directly accessed, this
    # must not cause a compilation error
    b: int

  Combined = object
    i: Import
    d: Destroy
    # since `Destroy` is used, `Combined` has automatic hooks lifted. The
    # automatic hook must not contain an access of the (non-existent) `i.b`
    # field

proc foo*(x: Combined) =
  var y = x # a copy is required
  # modify in order to prevent cursorfication:
  y.d.value = 1

foo(Combined())
