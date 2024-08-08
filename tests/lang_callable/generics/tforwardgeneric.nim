discard """
  output: "1.1 11\n42\n0"
  ccodecheck: "!@'ClEnv'"
"""

proc p[T](a, b: T): T

echo p(0.9, 0.1), " ", p(9, 1)

proc p[T](a, b: T): T =
  let c = b
  result = a + b + c

# https://github.com/nim-lang/Nim/issues/4908
proc foo(t: typedesc[int]): int
proc bar(): int = foo(int)
proc foo(t: typedesc[int]): int =
  return 0

# https://github.com/nim-lang/Nim/issues/4104
proc print[T](t: T) # Error: implementation of 'print.print(t: int)' expected
print 42 # moving this line after the implementation fixes the error,
         # but such behaviour makes forward declaration pointless
proc print[T](t: T) =
  echo t

echo bar()

block unresolved_generic_param_in_body:
  # instantiating a forwarded generic proc before its body is
  # present led to the generic parameters in the instantiated body to not be
  # properly resolved
  proc forwarded[T]()
  forwarded[int]()

  proc forwarded[T]() = # complete the definition
    doAssert ($T) == "int"

# Differing comments must not prevent forward declaration
template noop(a: untyped): untyped = a

proc comm[T](arg = ( noop (;
    ## comment
    true
  )
 ) )
proc comm[T](arg = ( noop (;
    ## this comment is different
    true
  )
 ) ) = discard

comm[bool](true)
