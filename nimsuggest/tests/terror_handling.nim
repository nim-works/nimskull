# Ensure that AST not having the correct number of nodes doesn't cause
# crashes. This a regression test for `checkSonsMinLen` and
# `checkSonsLen` not aborting analysis.

import std/macros

macro t1(): untyped =
  newNimNode(nnkCall) # an call AST with not enough sub-nodes

t1()

#[!]#
discard """
$nimsuggest --tester $file
>chk $1
chk;;skUnknown;;;;Error;;$file;;10;;23;;"illformed AST: ()";;0
"""
