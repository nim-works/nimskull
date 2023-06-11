discard """
  output: '''
StmtList
  FromStmt
      Sym "tables"
          Sym "OrderedTable"
'''
"""

import std/macros

macro blah(n: untyped) =
  let output = treeRepr n
  quote:
    echo `output`
blah:
  from std/tables import OrderedTable
