discard """
action: compile
description: '''
  . From https://github.com/nim-lang/Nim/issues/3669
    Multiple generic table types with different type signatures lead to
    compilation errors.
  . Removing the inodes field, switching inodes and rnodes in the
    declaration, or making inodes a Table[int,int] makes the code compile.
'''
"""

import tables

type
  G[T] = object
    inodes: Table[int, T]
    rnodes: Table[T, int]

var g: G[string]
echo g.rnodes["foo"]

