# Regression test for server initialazed with project file but query another
# file that not indexed with project's graph. this covers below usa cases
# 1. a new file under project directory structure not be imported nor included
#    by compiled files
# 2. a file that used by explicitly importing in another project.

discard """
$nimsuggest --tester $file
>def $path/tinclude.nim:7:14
def;;skProc;;minclude_import.create;;proc (greeting: string, subject: string): Greet{.noSideEffect, gcsafe, locks: 0.};;*fixtures/minclude_include.nim;;3;;5;;"";;100
"""
