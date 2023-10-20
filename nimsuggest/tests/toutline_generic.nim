# Regression test for symbols within template and generic bodies being
# wrongfully reported by `outline`.

proc generic[T]() =
  var x = 0 #[!]#
  return # <- when no cursor is provided, this triggered `x` being reported

template templ() =
  var x = 0 #[!]#
  return # <- when no cursor is provided, this triggered `x` being reported

# try both with and without a specified cursor position, they should be the same

discard """
$nimsuggest --tester $file
>outline $1
outline;;skProc;;toutline_generic.generic;;*;;4;;5;;"";;100
outline;;skTemplate;;toutline_generic.templ;;*;;8;;9;;"";;100

>outline $2
outline;;skProc;;toutline_generic.generic;;*;;4;;5;;"";;100
outline;;skTemplate;;toutline_generic.templ;;*;;8;;9;;"";;100

>outline $path/toutline_generic.nim
outline;;skProc;;toutline_generic.generic;;*;;4;;5;;"";;100
outline;;skTemplate;;toutline_generic.templ;;*;;8;;9;;"";;100
"""
