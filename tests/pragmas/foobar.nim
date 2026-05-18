import std/macros
macro async*(body: untyped): untyped =
  return body
