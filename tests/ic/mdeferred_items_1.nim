## This module only defines the type and procedure

type Type*[T] = object

# create an instance of the type to test with in this module. The instance of
# a generic is used, since those not loaded and cached early like top-level
# definitions are
discard $Type[int]

proc test*[T](): int =
  var counter {.global.} = 0
  inc counter
  result = counter
