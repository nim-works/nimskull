discard """
description: '''
An enumeration defines a type with a closed set of values. These values
have an identifier, are ordered, and have corresponding integer and string
values.
'''
"""

block enum_definition_syntax:
  type SimpleEnum = enum name1, name2, name3

block enum_values:
  ## Each enum constnat can be assigned and integer value, and constant's string
  ## representation can be overriden.

  block enum_default_int_values:
    ## enums int values are zero based indexed by default, `ord` is used
    ## to retrieve their int value
    type
      Abc = enum
        a
        b
        c

    doAssert ord(a) == 0, "1st enum int value corresponds to 0"
    doAssert ord(b) == 1, "2nd enum int value corresponds to 1"
    doAssert ord(c) == 2, "3rd enum int value corresponds to 2"

  block enum_default_int_values:
    ## It is possible to override single value in the enum - all subsequent 
    ## ones will be incremented by one
    type
      Abc = enum
        a = 12
        b
        c

    doAssert ord(a) == 12, "1st enum int value corresponds to 0"
    doAssert ord(b) == 12 + 1, "2nd enum int value corresponds to 1"
    doAssert ord(c) == 12 + 2, "3rd enum int value corresponds to 2"


  block enum_unqualified_access:
    ## access only needs to be qualified if ambiguous -- here it isn't
    type
      Abc = enum
        a
        b
        c
    doAssert ord(b) == 1, "unqualified access"

  block enum_default_string_values:
    ## enum string values are derived from their name by default, use `$` for
    ## conversion to a string
    type
      Abc = enum
        a
        b
        c

    doAssert $a == "a", "value's string derives from the name by default - a"
    doAssert $b == "b", "value's string derives from the name by default - b"
    doAssert $c == "c", "value's string derives from the name by default - c"

  block enum_assigned_int_values:
    ## Enum constants can be assigned with arbitrary values, but they have to
    ## be placed in increasing order. So `2, 3, 5` is ok, but `2, 4, 3` is 
    ## not allowed.
    type
      Abc = enum
        a = 2
        b = 3
        c = 5
        
    doAssert ord(a) == 2, "enum assigned int value - a"
    doAssert ord(b) == 3, "enum assigned int value - b"
    doAssert ord(c) == 5, "enum assigned int value - c"

  block enum_assigned_string_representation:
    ## In addition to overriding integer value of the enum constant, you can also
    ## override it's string representation (without affecting the value)
    type
      Abc = enum
        a = "apple"
        b = "zebra"
        c = "panda"

    doAssert $a == "apple", "enum assigned string value - a"
    doAssert $b == "zebra", "enum assigned string value - b"
    doAssert $c == "panda", "enum assigned string value - c"

  block enum_default_behaviour_retention:
    ## enum int and string representation can be assigned and gaps are filled, int is
    ## incremented and strings revert to deriving from the name
    type
      Abc = enum
        a = (0, "ape")
        b = ("see")
        c = 5

    doAssert ord(a) == 0 and $a == "ape", "assigned int or string representation - a"
    doAssert ord(b) == 1 and $b == "see", "assigned int or string representation - b"
    doAssert ord(c) == 5 and $c == "c",   "assigned int or string representation - c"

  block enum_assigned_equal_string_representation:
    # enum values can be assigned the same string representation, and they can be empty
    # xxx: this seems like a bug/design flaw
    type
      Abc = enum
        a = ""
        b = ""
        c = ""

    doAssert $a == "", "enum assigned an empty string value - a"
    doAssert $b == "", "enum assigned an empty string value - b"
    doAssert $c == "", "enum assigned an empty string value - c"



## Enum type can be used in sets just like any other ordinal
## type. In addition to sets it can also serve as an index to
## array type.

block enum_sets:
  type Enum = enum en1, en2, en3, en4, en5

  ## Enum sets support all built-in operations and are not different 
  ## from set with any other type. 
  
  ## `in` operator
  doAssert en1 in {en1, en2}

  ## `notin`
  doAssert en2 notin {en1}

  ## Set intersection 
  doAssert {en1, en2} * {en1, en3} == {en1}

  ## And so on
  doAssert {en1} is set[Enum]


block enum_arrays:
  ## In order to use enum as an index to array it must not contain
  ## gaps (holes) in values.
  type
    Index = enum
      idx1
      idx2
      idx3

  block full_array_index:
    var index: array[Index, string]

    index[idx1] = "hello"
    index[idx2] = "world"

    ## Note - when using enum as an index for arrays it is no longer possible to 
    ## access values with integer-based indices (because integers are not implicitly
    ## convertible to enum)
    doAssert index[Index(0)] == "hello"

    ## Size of the enum-indexed array follows regular rules (len(array) * sizeof(object))
    doAssert sizeof(index) == sizeof(string) * len(index)
    doAssert sizeof(index) == sizeof(string) * 3

  block partial_aray_index:
    ## It is also possible to index array using enum *slice* - delimiting based
    ## on start/end values. In this case size of the array would be smaller as well.

    var index: array[idx1 .. idx2, string] 
    index[idx1] = "idx1"
    index[idx2] = "idx2"

    ## Indexing restructions still apply here
    doAssert index[Index(0)] == "idx1"
    doAssert index[Index(1)] == "idx2"

  block partial_aray_index_offset:
    ## Enum slice can start and end with any enumeration value
    var index: array[idx2 .. idx3, string] 
    index[idx2] = "idx1"
    index[idx3] = "idx2"

    doAssert index[Index(1)] == "idx1"
    doAssert index[Index(2)] == "idx2"

  block enum_indexed_literal:
    ## It is also possible to construct enum-indexed array literals
    ## directly.
    
    block full:
      ## If all the keys in array literal are supplied it functions as
      ## regular `array[Enum, V]` array.
      let index = [idx1: 1, idx2: 22, idx3: 333]

      doAssert index is array[Index, int]

      doAssert index[idx1] == 1
      doAssert index[idx2] == 22
      doAssert index[idx3] == 333

    block partial:
      ## Array literals that map only slice of the enumeration values are 
      ## also supported, but keys **must* be placed in sequential order and
      ## don't contain holes. `idx1: , idx2:` is allowed, as well as `idx2:, idx3`,
      ## but not `idx1:, idx3`
      let index = [idx1: 1, idx2: 22]

      doAssert index is array[idx1 .. idx2, int]

      doAssert index[idx1] == 1
      doAssert index[idx2] == 22

    


