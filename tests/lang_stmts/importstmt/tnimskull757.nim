discard """
  description: "Test import statements are properly analysed"
  output: '''
StmtList
  ImportStmt
    Ident "foo"
  ImportStmt
    Ident "foo"
    Ident "bar"
  FromStmt
    Ident "foo"
    Ident "bar"
  FromStmt
    Ident "foo"
    Ident "bar"
    Ident "baz"
  ImportExceptStmt
    Ident "foo"
    Ident "bar"
  ImportExceptStmt
    Ident "foo"
    Ident "bar"
    Ident "baz"
'''
"""

import std/macros

macro blah(n: untyped) =
  let output = treeRepr n
  quote:
    echo `output`
blah:
  import foo
  import foo, bar
  from foo import bar
  from foo import bar, baz
  import foo except bar
  import foo except bar, baz

