discard """
description: '''
Triple Quoted string literals:
- allow for long lines
- do not need to escape non-consecutive occurences of `"`
- end when three double quotes are followed by a non-doublequote character
'''
"""

doAssert """""" == "", "empty string and triplequoted literals are equivalent"

doAssert """test
this""" == "test\nthis", "new lines can easily be insert"

doAssert """
test
this""" == "test\nthis", "newline after the opening are ignored"

doAssert """

test
this""" == "\ntest\nthis", "only the first newline after an opening is ignored"

doAssert """  
test
this""" == "test\nthis", "whitespace between the opening & newline are ignored"