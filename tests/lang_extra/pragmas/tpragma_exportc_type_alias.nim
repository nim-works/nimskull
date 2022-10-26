discard """
  errormsg: "{.exportc.} not allowed for type aliases"
  line: 21
  description: '''
    . From https://github.com/nim-lang/Nim/issues/5149
      C++ codegen: type alias with exportc leads to incorrect generated code;
    . The reason is that b.nim defines Y not as an alias, but as a separate
      struct type, resulting in mismatched definitions
      for impl_12kjjADT6kRMb9a9albDQ9bBQ.
    . IMO the compiler should forbid this construct, you can't annotate
      implementation details like this after the actual type definition.
    . https://github.com/nim-lang/Nim/pull/9979
      exportc not allowed for type aliases
    '''

"""

type
  X* = object
    a: int
  Y* {.exportc.} = X

proc impl*(x: X) =
  echo "it works"

