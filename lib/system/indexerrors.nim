# imported by other modules, unlike helpers.nim which is included
# xxx this is now included instead of imported, we should import instead

# NOTE: these routines are used by compilerprocs raising exceptions, which
# currently cannot rely on finalizers (both explicit and implicit). In other
# words, the routines here need to make sure that they don't inject any local
# or temporary that requires destruction.

proc formatErrorIndexBoundStr(i, a, b: sink string): string =
  "index " & $i & " not in " & $a & " .. " & $b

template formatErrorIndexBound*[T](i, a, b: T): string =
  when defined(standalone):
    "indexOutOfBounds"
  else:
    if b < a: "index out of bounds, the container is empty"
    else: formatErrorIndexBoundStr($i, $a, $b)

template formatErrorIndexBound*[T](i, n: T): string =
  formatErrorIndexBound(i, 0, n)

proc formatFieldDefect*(f, discVal: sink string): string =
  f & discVal & "'"
