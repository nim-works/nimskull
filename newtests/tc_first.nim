import tester

compilationSpec(
  name = "converter",
  description = "take exactly one argument",
  compilationWillFail(
    errorMsg = "a converter takes exactly on argument",
    line = 12
  )
)

converter foo(): float = 1.0

echo foo()