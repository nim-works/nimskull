discard """
  errormsg: "cannot instantiate: 'GenericParentType[T]'; Maybe generic arguments are missing?"
  line: 11
"""

type
  GenericParentType[T] = ref object of RootObj
  GenericChildType[T] = ref object of GenericParentType # missing the [T]
    val: T

var instance : GenericChildType[int] = nil
