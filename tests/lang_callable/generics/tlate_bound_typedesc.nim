discard """
  errormsg: "The default parameter 'arg' has incompatible type with the explicitly requested proc instantiation"
  line: 12
"""

# original issue https://github.com/nim-lang/nim/issues/11660

# XXX: this semantics of ``typedesc`` as the parameter type are very likely
#      going the change in the future. For the time being, the test ensures
#      that the behaviour doesn't silently change

func typedescDefault(T: typedesc; arg: T = 0) =
  discard

typedescDefault(int)