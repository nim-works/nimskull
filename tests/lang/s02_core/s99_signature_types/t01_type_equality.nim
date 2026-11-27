
type A = (signature(Self) do:
  discard
)
type B = (signature(Self) do:
  discard
)
doAssert A is A # a signature type is equal to itself
doAssert B isnot A
doAssert A isnot B
