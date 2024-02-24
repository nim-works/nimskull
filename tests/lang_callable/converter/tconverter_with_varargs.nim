
# bug https://github.com/nim-lang/Nim/issues/888

type
  PyRef = object
  PPyRef* = ref PyRef

converter to_py*(i: int): PPyRef = nil

proc to_tuple*(vals: openarray[PPyRef]): PPyRef =
  discard

proc abc(args: varargs[PPyRef]) =
  let args_tup = to_tuple(args)
  discard

abc(1, 2)
