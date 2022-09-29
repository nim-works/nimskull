
template tmp(x: untyped): untyped =
  # prevent constant folding by introducing a temporary
  let v = x
  v

block setops:
  doAssert tmp(255'u8) in {0'u8, 255'u8}
  doAssert tmp(1'u8) notin {0'u8}