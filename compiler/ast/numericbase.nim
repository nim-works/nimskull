## This module doesn't do much beyond store a common type between the `lexer` &
## `ast` called `NumericalBase` to break cyclic dependencies

type
  NumericalBase* = enum
    base10,                   ## base10 is listed as the first element,
                              ## so that it is the correct default value
    base2, base8, base16