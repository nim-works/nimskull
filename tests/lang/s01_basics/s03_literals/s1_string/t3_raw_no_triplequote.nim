discard """
description: '''
There are no raw triplequoted string literals, though generalized string
literals do support this, and result in an unclosed triplequoted literal error
'''
errormsg: "closing \"\"\" expected, but end of file reached"
line: 11
column: 1
"""

discard r""" == "\"", "tripleQuoted raw string literals are not possible"