discard """
  action: compile
  description: '''include statements are dropped from `typed` so they're only
                  processed once'''
"""

import std/macros

# xxx: ideally we don't need to drop them, instead the typing of the parameter
#      shouldn't pollute the callsite, followed by re-evaluation of the
#      untrusted macro output being analysed yet again (if the untrusted AST
#      contains the `include`, it'll be evaluated twice and produce a
#      redefinition error).

block:
  macro bob(n: typed) =
    doAssert n.kind != nnkIncludeStmt
    n

  bob:
    include mbar

block:
  macro bob(n: typed) =
    doAssert n.kind != nnkIncludeStmt
    discard

  # typing it twice shouldn't be an issue
  bob:
    include mbar
  bob:
    include mbar