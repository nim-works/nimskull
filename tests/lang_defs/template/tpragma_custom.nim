discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/18113
    Sequtils bug: cannot attach a custom pragma to s2
  . Commenting the {.push raises: [Defect].} is a workAround.
  . Breaking the toSeq and Filter into separate operations also does not
    trigger the issue.
  . Caused by https://github.com/nim-lang/Nim/pull/17448
  . The issue is exposed by the fact that I moved s.ast = n ahead of
    the pragma handling. Moving that after pragma handling "fixes"
    the problem but really it's just papering over what's
    likely a more fundamental issue.
  . Ensure template pragma handling doesn't eagerly attempt to add an implicit
    'pushed' pragma to the evaluation of any intermediate AST prior to
    substitution.
'''
"""

import sequtils

{.push raises: [Defect].}

var a = toSeq([1, 2, 3, 5, 10]).filterIt(it > 5)

doAssert a.len == 1
doAssert a[0] == 10

