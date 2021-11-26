discard """
description: '''
Test declaration and field access (checked and unchecked) for variant objects.
'''
"""

block simple_variant:
  type
    Enum = enum
      en1
      en2
      en3
      en4
      en5

    EnumVariant = object
      case kind: Enum
        of en1:
          ## Object variants branches can have one value
          f1: int

        of en2, en3:
          ## Or several values at once
          f23: int

        of {en4}:
          ## Set syntax is also supported
          f4: int

        else:
          ## As well as regular 'else' case.
          f5: int

    EnumVariant2 = object
      case kind: Enum
        of en1 .. en4:
          ## It is possible to switch over ranges of enums at once
          f1: int

        else:
          ## Case must be exhaustive, but if there are no fields you
          ## want to put there you might use `discard`
          discard

    BoolVariant = object
      case isSelected: bool
        of true:
          fTrue: int

        of false:
          fFalse: int

    RangeVariant = object
      case idx: range[0..4]:
        of 0:
          f1: int

        of 1 .. 3:
          f123: int

        of 4:
          f4: int

    


block multiple_branches:
  type
    BranchVariant = object
      case isSelected1: bool
        of true:
          f1True: int

        of false:
          f1False: int

      case isSelected2: bool
        of true:
          f2True: int

        of false:
          f2False: int         


block nested_branches:
  type
    BranchVariant = object
      case isSelected1: bool
        of true:
          f1True: int

          case isSelected2: bool
            of true:
              f12True: int

            of false:
              f12False: int     

        of false:
          f1False: int
    
{.push fieldChecks:off.}

block unchecked_assign: 
  type
    BoolVariant = object
      case isSelected: bool
        of true:
          fTrue: int

        of false:
          fFalse: int

  var it: BoolVariant

  doAssert it.isSelected == false

  {.cast(uncheckedAssign).}:
    it.isSelected = true

  doAssert it.isSelected == true
  doAssert it.fTrue == 0

{.pop.}
