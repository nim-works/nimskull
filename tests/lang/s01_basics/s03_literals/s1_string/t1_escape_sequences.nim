discard """
description: '''
Covers the string literals and key escape sequences. These tests are mostly
here to ensure things work the same across platforms regardless of the
particular escaping method used. Otherwise the tests are pretty silly.

ASCII reference: https://en.wikipedia.org/wiki/ASCII#Control_characters
'''
"""

# NB: `\[0-9]+` - is the decimal encoding of a character

doAssert "\r" == "\c",  "escape sequence: 'carriage return' using: \\r or \\c"
doAssert "\r" == "\13", "'carriage return' is decimal code 31"
doAssert "\c" == "\13", "'carriage return' is decimal code 31"

doAssert "\n" == "\l",  "escape sequence: 'line feed' using: \\n or \\l"
doAssert "\n" == "\10", "'line feed' is decimal code 10"
doAssert "\l" == "\10", "'line feed' is decimal code 10"

doAssert "\f" == "\12", "'form feed' is decimal code 12"

doAssert "\t" == "\9",  "'horizontal tab' is decimal code 9"

doAssert "\v" == "\11", "'vertical tab' is decimal code 11"

doAssert "\\" == "\92", "'backslash' is decimal code 92"

doAssert "\"" == "\34", "'double quote' is decimal code 34"

doAssert "\'" == "'", "'apostrophe' or 'single quote' using: \\' or '"
doAssert "\'" == "\39", "'apostrophe' is decimal code 39"
doAssert "'"  == "\39", "'single quote' is decimal code 39"

doAssert "\a" == "\7", "'alert' or 'bell' is decimal code 7"

doAssert "\b" == "\8", "'backspace' is decimal code 8"

doAssert "\e" == "\27", "'backspace' is decimal code 27"

# hex encoded character escape sequence using `\xHH`
doAssert "\0"   == "\x00", "hex encoded `\\xHH` control plane start: Null"
doAssert "\31"  == "\x1F", "hex encoded `\\xHH` control plane end: Unit Sep"
doAssert "\32"  == "\x20", "hex encoded `\\xHH` character plane start: Space"
doAssert "\126" == "\x7E", "hex encoded,`\\xHH` character plane end: ~"
doAssert "\127" == "\x7F", "hex encoded `\\xHH` last control char: Delete"

# hex encoded unicode code point using `\xHHHH`
doAssert "\x00" == "\u0000", "`\\uHHHH` hex unicode codepoint overlap: Null"
doAssert "\x1F" == "\u001F", "`\\uHHHH` hex unicode codepoint overlap: Unit Sep"
doAssert "\x20" == "\u0020", "`\\uHHHH` hex unicode codepoint overlap: Space"
doAssert "\x7E" == "\u007E", "`\\uHHHH` hex unicode codepoint overlap: ~"
doAssert "\x7F" == "\u007F", "`\\uHHHH` hex unicode codepoint overlap: Delete"

# hex encoded unicode code point value using `\x{H+}`
doAssert "\u0000" == "\u{0000}", "`\\u{H+}` unicode codepoint value: Null"
doAssert "\u001F" == "\u{001F}", "`\\u{H+}` unicode codepoint value: Unit Sep"
doAssert "\u0020" == "\u{0020}", "`\\u{H+}` unicode codepoint value: Space"
doAssert "\u007E" == "\u{007E}", "`\\u{H+}` unicode codepoint value: ~"
doAssert "\u007F" == "\u{007F}", "`\\u{H+}` unicode codepoint value: Delete"
