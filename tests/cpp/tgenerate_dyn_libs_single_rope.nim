discard """
  targets: "cpp"
  action: "compile"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/8241
    Strange codegen ICE with importcpp #8241
  . The compiler assumes that once the nodes reach genCall the symbol
    sym.loc.r points to a valid leafy rope but that's not the case when
    mangleDynLibProc is thrown in the mix. We could just make mangleDynLibProc
    always produce a single rope piece.

  . From https://github.com/nim-lang/Nim/issues/9222
  . Compiler is using a global temp string to hold the result of the findlib
    invocation. If you have several modules using different libraries
    with different findlib functions, you will get a link time error saying
    that you have multiple definitions of the same global variable _T1.
    The fix is to use local variable.

  . From https://github.com/nim-lang/Nim/issues/8946
    compiler workchain
  . i would suggest to introduce a step of reordering the
    lines in a way that import, const, var, let, proc are arranged at top
    before compiling the source. i guess it may scramble up with the line
    addresses in error messaging and a scheme to readdress the lines might
    be needed. Anyway imo the current situation is inconvenient and reminds
    me of writing pascal in the '80s.
'''
"""

proc foo(): cstring {.importcpp: "", dynlib: "".}
echo foo()


## bug #9222
import os
import amodule
proc findlib2: string =
  let path = getEnv("MYLIB2_DOES_NOT_EXIST_PATH")
  if path.len > 0 and dirExists(path):
    path / "alib_does_not_matter.dll"
  else:
    "alib_does_not_matter.dll"

proc imported_func2*(a: cint): cstring {.importc, dynlib: findlib2().}

echo imported_func(1)
echo imported_func2(1)

# issue #8946

from json import JsonParsingError
import marshal

const nothing = ""
doAssertRaises(JsonParsingError):
  var bar = marshal.to[int](nothing)