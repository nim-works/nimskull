discard """
matrix: "--gc:refc -d:fulldebug -d:smokeCycles"
output: '''@["a"]'''
labels: "codegen gc seq"
description: '''
 . From https://github.com/nim-lang/Nim/issues/6279
   GC Bug
 . I've ran it a couple of times, and the result is different,
   either some garbage, or empty seq, or crash.
'''
"""


var foo = newSeq[tuple[a: seq[string], b: seq[string]]]()
foo.add((@["a"], @["b"]))
echo foo[0].a # Crashes on this line