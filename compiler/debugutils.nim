##[
Utilities to help with debugging nim compiler.

Experimental API, subject to change.
]##

#[
## example
useful debugging flags:
--stacktrace -d:debug -d:nimDebugUtils
 nim c -o:bin/nim_temp --stacktrace -d:debug -d:nimDebugUtils compiler/nim

]#

when not defined(nimDebugUtils):
  {.error: "`nimDebugUtils` was not defined, set via -`d:nimDebugUtils`".}

import options

proc isCompilerDebug*(conf: ConfigRef): bool {.inline.} =
  ##[
  Provides a simple way for user code to enable/disable logging in the compiler
  in a granular way. This can then be used in the compiler as follows:
  ```nim
  if conf.isCompilerDebug():
    echo n.sym.typ.len
  ```
  ]##
  runnableExamples:
    proc main =
      echo 2
      {.define(nimCompilerDebug).}
      echo 3.5 # code section in which `isCompilerDebug` will be true
      {.undef(nimCompilerDebug).}
      echo 'x'
  conf.isDefined("nimCompilerDebug")
