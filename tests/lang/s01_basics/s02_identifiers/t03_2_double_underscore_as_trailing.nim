discard """
description: '''
two consecutive underscores are considered a break in an identifier, or a
trailing underscore per the error.
'''
action: reject
errormsg: "invalid token: trailing underscore"
line: 13
column: 31
"""

# identifiers cannot have two consecutive underscore
discard true and true == true a__nd true
