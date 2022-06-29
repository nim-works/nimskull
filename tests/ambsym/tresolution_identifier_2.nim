discard """
errormsg: "ambiguous identifier"
file: "tresolution_identifier_2.nim"
line: 16
labels: "enum identifier import resolution"
description: '''
  . mresolution_identifier1 defines TExport, an enum with x, y, z.
  . mresolution_identifier2 defines TExport, an enum with a, b, c.
  . We expect an ambiguous identifier: With just the enum name, we have no
    way of knowing whether we're referring to the first or second definition.
  '''
"""

import mresolution_identifier_1, mresolution_identifier_2

var v: TExport


