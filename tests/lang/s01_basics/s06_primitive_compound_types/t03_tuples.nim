discard """
description: '''
Tuples refer to a broadset of types, each of which has zero or more fields; however,
the total fields and their order, names, and individual types are fixed at time
of definition. Tuples are structurally matched; two tuple types that
have the same fields (in order, name, and type) are considered the same.

Covers:
- literals
- anonymous tuples
- type definitions
- indexing
- type match/mismatch
'''
"""

block tuple_anon_literals:
  let
    empty = ()
    single = (1,)        # need a trailing `,` to disambiguate from parentheses
    pair = (2, "string")
    triple = (3, "foo", ("bar", "baz"))

  doAssert empty == (),           "compare element-wise - empty"
  doAssert single == (1,),        "compare element-wise - single"
  doAssert single != (2,),        "compare element-wise - single unequal"
  doAssert pair == (2, "string"), "compare element-wise - pair"
  doAssert triple == (3, "foo", ("bar", "baz")),
    "compare element-wise - triple"



block accessing_tuple:
  ## In order to access particular value from the tuple you can index into it using `[]` 
  ## operator. Note that index value must be known at compile-time, because this 
  ## affects type of the expression.
  let it = (12, "123")

  doAssert it[0] == 12
  doAssert it[1] == "123"

  doAssert it[0] is int
  doAssert it[1] is string


block tuple_type:
  block single_tuple:
    let single: (int,) = (12,)
    doAssert single[0] is int
    doAssert single[0] == 12

  block pair_tuple:
    let pair: (int, string) = (12, "sfd")
    doAssert pair[0] is int
    doAssert pair[1] is string

  block triple_tuple:
    let triple: (int, string, (string, string)) = (3, "foo", ("bar", "baz"))


  block array_of_tuple:
    let values: array[3, (int, string)] = [(12, "13"), (14, "15"), (16, "17")]

    doAssert values[0][0] is int
    doAssert values[0][1] is string



block unpacking_tuples:
  ## Another way of extracting individual values from tuples is unpackind 
  let it = (12, "123")

  let (firstItem, secondItem) = it

  doAssert firstItem is int
  doAssert firstItem == 12

  doAssert secondItem is string
  doAssert secondItem == "123"

  ## It can also be used directly with initialization, for more compact declaration of
  ## multiple variables at once
  let (asgn1, asgn2) = (12, "123")

  ## Number of elements in the unpacking must match with number of elements in the 
  ## tuple (see `t01_tuples_wrong_number_of_variables.nim`). To ignore particular values
  ## target assignment can use `_`
  let (_, _) = (12, "123")

  doAssert asgn1 == 12
  doAssert asgn2 == "123"

  ## It is also possible to unpack tuples from `if` expressions
  let (if1, if2) = if true: (1, 2) else: (3, 4)

  doAssert if1 == 1
  doAssert if2 == 2

  
  let (case1, case2) = 
    case 1:
      of 0: ("name1", "name2")
      of 1: ("name3", "name3")
      of 2: ("name4", "name5")
      else: ("default", "xxx")
  

block unpack_to_mutable:
  ## It is possible to unpack in already defined variables
  
  var asgn1: int = 1
  var asgn2: string = "default"

  doAssert asgn1 == 1
  doAssert asgn2 == "default"

  (asgn1, asgn2) = (12, "new")

  doAssert asgn1 == 12
  doAssert asgn2 == "new"


  ## Unpacking can also be performed into complex expressions
  var arr: array[4, int]

  doAssert arr == [0, 0, 0, 0]

  (arr[0], arr[1]) = (12, 2)

  doAssert arr == [12, 2, 0, 0]


  



block named_tuple_literals:
  ## It is possible to assign names to tuple fields. 
  let 
    single = (number: 1)
    pair = (number: 1, text: "string")
    triple = (number: 3, text: "string", intuple: ("text1", "text2"))

  ## It is still possible to compared named tuples with their anonymous variants, as
  ## long as elements are placed in the same order
  doAssert single == (1,)
  doAssert pair == (1, "string")
  doAssert triple == (3, "string", ("text1", "text2"))

block named_tuple_type:
  let
    pair1: tuple[f1: int, f2: string] = (f1: 12, f2: "text")
    pair2: tuple[f1, f2: int] = (f1: 12, f2: 24)
    pair3: tuple[f1: int, f2: string] = (12, "text")

    pair4: tuple[other1: int, other2: string] = (12, "text")

  ## Named tuples have different types of their field names are different 
  doAssert pair4 is tuple[other1: int, other2: string]
  doAssert pair1 isnot tuple[other1: int, other2: string]

  ## But any named tuple still matches with anonymous one
  doAssert pair4 is (int, string)
  doAssert pair1 is (int, string)
  doAssert pair3 is (int, string)

  ## And can be structurally compared
  doAssert pair1 == (12, "text")
  doAssert pair2 == (12, 24)
  doAssert pair3 == (12, "text")
  doAssert pair4 == (12, "text")
