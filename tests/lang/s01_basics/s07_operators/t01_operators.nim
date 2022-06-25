discard """
description:'''
Operators and tokens in the language:
- precedence is fixed
- some keywords are operators as well
- other tokens, not considered operators

To be covered elsewhere:
- operator overloading is allowed (to be covered laters/elsewhere)
  - though not all operators are available for overload
  - user defined operators one or a a combination of tokens
'''
"""

#[
  Operator Tokens:
  =     +     -     *     /     <     >
  @     $     ~     &     %     |
  !     ?     ^     .     :     \
]#

#[
  Keywords, also Operators:
  and   or    not   xor
  shl   shr
  div   mod
  in    notin
  is    isnot
  of
  as    from
]#

# Precedence:
#
# 11 precedence levels, 10 is the highest, and 0 is the lowest
# (NB: overload related items will be covered in later tests)
#
# level to existing operator mapping:
# 10 - `$`                      to string
# 9  - `* / div mod shl shr %`  division & multiplication
# 8  - `+ -`                    addition & subtraction
# 7  - `&`                      concatenation
# 6  - `..`                     slice
# 5  - `== <= < >= > != in      comparison
#       notin is isnot not of
#       as from`
# 4  - `and`                    logical and/multiply
# 3  - `or xor`                 logical or/sum
# 2  - N/A                      overloading related
# 1  - `=`                      assignment
# 0  - N/A                      arrow-like

doAssert false == false, "`==` is composed of the `=` token in a row"
doAssert $1 == "1", "to string operation"

doAssert 2 + 2 * 10 + -2 / 2 == 21,
  "typical math order of operations, and unary `-`"

doAssert true and true,  "`and` keyword is a binary operator"
doAssert true or  false, "`or` keyword is a binary operator"
doAssert true xor false, "`xor` keyword is a binary operator"
doAssert true or false and false, "`and` has precedence over `or`"

doAssert true or false == false, "`==` (comparison) preceeds `or` (logical)"

doAssert not false,          "`not` is a unary operator"
doAssert not false and true, "`not` has precedence over `and`"
doAssert not false or false, "`not` has precedence over `or`"

const foo = false or false
doAssert foo == false, "assignement operators, end with `=`, have precedence 9"