# Regression test for querying a file/module that is not the project file/module
# or part of the transitive closure of all dependencies (via import or include).
# In such a case the queried file is not indexed within the project's graph.
#
# Covered use cases:
# 1. a new file under project directory structure not imported nor included by
#    compiled files.
# 2. a file that's used by explicitly importing in another project.

discard """
$nimsuggest --tester $file
>def $path/tinclude.nim:7:14
def;;skProc;;minclude_import.create;;proc (greeting: string, subject: string): Greet{.noSideEffect, gcsafe, locks: 0.};;*fixtures/minclude_include.nim;;3;;5;;"";;100
"""
