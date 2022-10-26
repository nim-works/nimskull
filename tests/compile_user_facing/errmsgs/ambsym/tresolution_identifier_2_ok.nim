discard """
labels: "enum identifier proc resolution"
description: '''
  . mresolution_identifier1 defines TExport, an enum with x, y, z.
  . mresolution_identifier2 defines TExport, an enum with a, b, c.
  . By expliciting a namespace alongside the type name, we are able to
    resolve the ambiguity.
  . This goes for both the TExport and the `foo` call. The `foo` call should
    be resolved to the one defined in identifier_2, *not* in identifier_2_copy.
'''
"""

import mresolution_identifier_1, mresolution_identifier_2
var v: mresolution_identifier_1.TExport

doAssert mresolution_identifier_2.foo(3) == 2
doAssert mresolution_identifier_2.foo(3) != 0
