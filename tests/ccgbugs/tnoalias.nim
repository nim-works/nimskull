discard """
  ccodecheck: "\\i@'NI64* NIM_NOALIAS field;' @'NIM_CHAR* NIM_NOALIAS x,' @'void* NIM_NOALIAS q'"
"""

type
  BigNum = object
    field {.noalias.}: ptr UncheckedArray[int64]

proc p(x {.noalias.}: openArray[char]) =
  var q {.noalias.}: pointer = addr(x[0])

var bn: BigNum
p "abc"
