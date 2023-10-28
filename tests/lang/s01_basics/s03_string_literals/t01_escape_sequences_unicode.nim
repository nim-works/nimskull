discard """
description: '''
Covers the string literals unicode escape sequences. These tests are mostly
here to ensure things work the same across platforms regardless of the
particular escaping method used. Otherwise the tests are pretty silly.
'''
"""

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
