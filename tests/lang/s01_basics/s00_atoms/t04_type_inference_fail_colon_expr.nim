discard """
description: '''
Type inference does not work if rhs is a complex expression

Also potentially related - https://github.com/nim-lang/Nim/issues/7321 although
```nim
var s: seq[Shape] = @[x]
```

is actually a non-trivial expressions, since it does `var s: seq[Shape] = @([x])` -
which calls magic/generic `@`.

'''
knownIssue: "https://github.com/nim-lang/RFCs/issues/149"
knownIssue: "https://github.com/nim-lang/Nim/issues/11777"
"""

block infer_set_type:
    # where this one does work
    var y: set[char] = {}

    # this one doesn't work (crashes the compiler)
    var x: set[char] = (discard 12; {})

    # this crashes compiler as well
    var q: seq[int] = (discard 1; @[])
