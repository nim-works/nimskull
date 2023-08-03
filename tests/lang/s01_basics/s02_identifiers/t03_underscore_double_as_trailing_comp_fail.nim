discard """
description: '''
Two consecutive underscores are considered a break in an identifier, or a
trailing underscore per the error.
'''
action: reject
errormsg: "invalid identifier: only non-consecutive and non-trailing underscores allowed: eg. 'a__b' and 'a_' are invalid"
line: 13
column: 31
"""

# identifiers cannot have two consecutive underscore
discard true and true == true a__nd true
