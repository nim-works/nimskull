##[
internal API for now, subject to modifications and moving around

string API's focusing on performance, that can be used as building blocks
for other routines.

Un-necessary allocations are avoided and appropriate algorithms are used at the
expense of code clarity when justified.

xxx: this is Tim's stuff, it's likely all a bad idea and should be removed.
]##

proc dataPointer*[T](a: T): pointer =
  ## Note: safe to use when a.len == 0 but whether the result is nil or not
  ## is implementation defined for performance reasons.
  when T is string | seq:
    if a.len == 0: nil else: cast[pointer](a[0].unsafeAddr)
  elif T is array:
    when a.len > 0: a.unsafeAddr
    else: nil
  elif T is cstring:
    cast[pointer](a)
  else: static: doAssert false, $T

proc isUpperAscii(c: char): bool {.inline.} =
  # avoids import strutils.isUpperAscii
  c in {'A'..'Z'}

proc toLowerAscii*(a: var string) =
  ## optimized and inplace overload of strutils.toLowerAscii
  # refs https://github.com/timotheecour/Nim/pull/54
  # this is 10X faster than a naive implementation using a an optimization trick
  # that can be adapted in similar contexts. Predictable writes avoid write
  # hazards and lead to better machine code, compared to random writes arising
  # from: `if c.isUpperAscii: c = ...`
  for c in mitems(a):
    c = chr(c.ord + (if c.isUpperAscii: (ord('a') - ord('A')) else: 0))
