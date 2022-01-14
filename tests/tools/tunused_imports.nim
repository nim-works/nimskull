discard """
  cmd: '''nim c --hint:Processing:off $file'''
  joinable: false
  nimout: '''
tunused_imports.nim(12, 10) Warning: BEGIN [User]
tunused_imports.nim(37, 10) Warning: END [User]
tunused_imports.nim(35, 8) Warning: imported and not used: 'strutils' [UnusedImport]
'''
  action: "compile"
"""

{.warning: "BEGIN".}

# bug #12885

import tables, second

template test(key: int): untyped =
  `[]`(dataEx, key)

echo test(1)

import net, dontmentionme

echo AF_UNIX

import macros
# bug #11809
macro bar(): untyped =
  template baz() = discard
  result = getAst(baz())

bar()

import strutils

{.warning: "END".}
