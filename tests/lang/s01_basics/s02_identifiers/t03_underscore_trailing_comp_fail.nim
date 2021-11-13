discard """
description: '''
Two consecutive underscores are considered a break in an identifier, or a
trailing underscore per the error.
'''
action: reject
errormsg: "invalid token: trailing underscore"
line: 13
column: 31
"""

# identifiers cannot have a trailing underscore
discard true and true == true and_ true
