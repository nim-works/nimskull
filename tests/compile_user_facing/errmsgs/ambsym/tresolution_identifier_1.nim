discard """
errormsg: "ambiguous identifier"
file: "tresolution_identifier_1.nim"
line: 17
labels: "enum identifier resolution stdlib"
description: '''
 . mresolution_identifier_1 defines TExport2, an enum with mDec, mInc, mAssign.
 . times, from the stdlib, defines Month, an enum which also contains mDec.
 . We expect an ambiguous identifier: With just the enum value, we have no
   way of knowing whether we're referring to the first or second definition.
'''
"""


import mresolution_identifier_1, times
var
  v = mDec

