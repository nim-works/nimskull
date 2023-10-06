from typetraits import supportsCopyMem

proc tgeneric[T](x: var T) =
  when supportsCopyMem(T):
    discard

discard """
$nimsuggest --tester $file
>outline $path/toutline_generic.nim
outline;;skProc;;toutline_generic.tgeneric;;*;;3;;5;;"";;100
"""
