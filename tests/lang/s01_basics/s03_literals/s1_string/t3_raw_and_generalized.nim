discard """
description: '''
Raw and generalized raw string literals:
- do not interpret escape sequences as doublequoted string literals
- raw string literals are any doublequoted string prefixed with an `r`, r"foo"
    or the capital letter `R`, R"foo"
- raw string literals can escape the doublequote with itself r"a""b" -> a"b
- generalized raw string literals are any doublequoted string literally,
    prefixed with an identifier that will pass the raw string literal value, no
    escaping, to the identifier like a call.
- generalized raw string literals can be applied to triplequoted strings
    unlike normal raw string literals
'''
"""

doAssert r"\n" == "\\n", "raw string literals don't apply escape sequences"
doAssert r"\n" != "\n",  "regular string literals are not the same"
doAssert r"\n" == R"\n", "raw string literals can be created using `r` or `R`"

doAssert r"a""b" == "a\"b", "raw string literals escape doublequotes"

proc myStr(a: string): string =
  ## strips the last two chars, see test below
  a

doAssert r"A\nB" == myStr"A\nB",
  "identifier prefixed string literals are generalized raw strings"
doAssert "A\nB" == myStr"""A
B""",
  "identifier prefixed tripleQuoted literals are generalized raw triplequoted"
