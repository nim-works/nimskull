discard """
  labels: "generic inheritance"
  description: "Ensure that errors are reported for illegal inheritance"
  action: reject
  cmd: "nim check --hints:off --filenames:canonical $file"
  nimoutfull: true
"""

type
  Object = object
  RefAnonObject = ref object
    ## ref of anonymous object
  RefObject = ref Object

  Wrapper[T] = T
  Nested[T]  = Wrapper[T]

  GenericBase[T] = object of T
  GenericRef[T]  = ref T

  Final[T] = object
    ## non-inheritable generic object

# test detection of illegal base types when no unresolved generic types are
# involved
block:
  type
    A = object of Object        #[tt.Error
               ^ inheritance only works with non-final objects; for Object to be inheritable it must be 'object of RootObj' instead of 'object']#
    B = object of RefAnonObject #[tt.Error
               ^ inheritance only works with non-final objects; for RefAnonObject to be inheritable it must be 'object of RootObj' instead of 'object']#
    C = object of RefObject     #[tt.Error
               ^ inheritance only works with non-final objects; for RefObject to be inheritable it must be 'object of RootObj' instead of 'object']#
    D = object of int           #[tt.Error
               ^ cannot inherit from a type that is not an object type]#
    E = object of Wrapper[int]  #[tt.Error
               ^ cannot inherit from a type that is not an object type]#
    F = object of Nested[int]   #[tt.Error
               ^ cannot inherit from a type that is not an object type]#
    G = object of Final[int]    #[tt.Error
               ^ inheritance only works with non-final objects; for Final[system.int] to be inheritable it must be 'object of RootObj' instead of 'object']#

# test early detection of illegal base type when unresolved generic types are
# involved
block:
  type
    # the following are all detected early, and not only when the type is
    # instantiated:
    A[T] = object of Final[T]          #[tt.Error
                  ^ inheritance only works with non-final objects; for Final[A.T] to be inheritable it must be 'object of RootObj' instead of 'object']#
    B[T] = object of Wrapper[Final[T]] #[tt.Error
                  ^ inheritance only works with non-final objects; for Wrapper[Final[B.T]] to be inheritable it must be 'object of RootObj' instead of 'object']#
    C[T] = object of Nested[Final[T]]  #[tt.Error
                  ^ inheritance only works with non-final objects; for Nested[Final[C.T]] to be inheritable it must be 'object of RootObj' instead of 'object']#

block:
  # the following types are valid at definition, but can produce errors later
  # on instantiation:
  type
    A[T] = object of GenericBase[T]
    B[T] = object of GenericRef[T]
    C[T] = object of Wrapper[T]
    D[T] = object of Wrapper[GenericRef[T]]
    E[T] = object of Wrapper[ptr T]

  # works:
  discard GenericBase[RootObj]()

  discard A[RootObj]()
  discard B[RootObj]()
  discard C[RootObj]()
  discard D[RootObj]()
  discard E[RootObj]()

  # must not work (because the base type is not an object type):
  discard A[int]() #[tt.Error
           ^ cannot inherit from a type that is not an object type]#
  discard B[int]() #[tt.Error
           ^ cannot inherit from a type that is not an object type]#
  discard C[int]() #[tt.Error
           ^ cannot inherit from a type that is not an object type]#
  discard D[int]() #[tt.Error
           ^ cannot inherit from a type that is not an object type]#
  discard E[int]() #[tt.Error
           ^ cannot inherit from a type that is not an object type]#

  # must not work (because the object is final):
  discard A[Object]() #[tt.Error
           ^ inheritance only works with non-final objects; for Object to be inheritable it must be 'object of RootObj' instead of 'object']#
  discard B[Object]() #[tt.Error
           ^ inheritance only works with non-final objects; for GenericRef[tillegal_base_type.Object] to be inheritable it must be 'object of RootObj' instead of 'object']#
  discard C[Object]() #[tt.Error
           ^ inheritance only works with non-final objects; for Wrapper[tillegal_base_type.Object] to be inheritable it must be 'object of RootObj' instead of 'object']#
  discard D[Object]() #[tt.Error
           ^ inheritance only works with non-final objects; for Wrapper[GenericRef[tillegal_base_type.Object]] to be inheritable it must be 'object of RootObj' instead of 'object']#
  discard E[Object]() #[tt.Error
           ^ inheritance only works with non-final objects; for Wrapper[ptr Object] to be inheritable it must be 'object of RootObj' instead of 'object']#

  # must not work (because there's one indirection too many):
  discard D[ref RootObj]() #[tt.Error
           ^ cannot inherit from a type that is not an object type]#
  discard D[ptr RootObj]() #[tt.Error
           ^ cannot inherit from a type that is not an object type]#
  discard E[ref RootObj]() #[tt.Error
           ^ cannot inherit from a type that is not an object type]#
  discard E[ptr RootObj]() #[tt.Error
           ^ cannot inherit from a type that is not an object type]#