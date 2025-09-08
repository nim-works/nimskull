discard """
description: '''
Identifiers match the first letter in a case sensitive manner
'''
errormsg: "attempting to call routine: 'true'"
line: 10
column: 31
"""

discard true and true == true And true
