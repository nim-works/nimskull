discard """
description: '''
Covers character literals and key escape sequences. These tests are mostly
here to ensure things work the same across platforms regardless of the
particular escaping method used. Otherwise the tests are pretty silly.

ASCII reference: https://en.wikipedia.org/wiki/ASCII#Control_characters
'''
"""

# NB: `\[0-9]+` - is the decimal encoding of a character

doAssert '\r' == '\c',  "escape sequence: 'carriage return' using: \\r or \\c"
doAssert '\r' == '\13', "'carriage return' is decimal code 31"
doAssert '\c' == '\13', "'carriage return' is decimal code 31"

doAssert '\n' == '\l',  "escape sequence: 'line feed' using: \\n or \\l"
doAssert '\n' == '\10', "'line feed' is decimal code 10"
doAssert '\l' == '\10', "'line feed' is decimal code 10"

doAssert '\f' == '\12', "'form feed' is decimal code 12"

doAssert '\t' == '\9',  "'horizontal tab' is decimal code 9"

doAssert '\v' == '\11', "'vertical tab' is decimal code 11"

doAssert '\\' == '\92', "'backslash' is decimal code 92"

doAssert '\"' == '"', "'double quote' or 'quotation mark' are the same"
doAssert '"'  == '\34', "'double quote' is decimal code 34"
doAssert '\"' == '\34', "'quotation mark' is decimal code 34"

doAssert '\'' == '\39', "'single quote' is decimal code 39"

doAssert '\a' == '\7', "'alert' or 'bell' is decimal code 7"

doAssert '\b' == '\8', "'backspace' is decimal code 8"

doAssert '\e' == '\27', "'escape' is decimal code 27"

# hex encoded character escape sequence using `\xHH`
doAssert '\0'   == '\x00', "hex encoded `\\xHH` control plane start: Null"
doAssert '\31'  == '\x1F', "hex encoded `\\xHH` control plane end: Unit Sep"
doAssert '\32'  == '\x20', "hex encoded `\\xHH` character plane start: Space"
doAssert '\126' == '\x7E', "hex encoded,`\\xHH` character plane end: ~"
doAssert '\127' == '\x7F', "hex encoded `\\xHH` last control char: Delete"
