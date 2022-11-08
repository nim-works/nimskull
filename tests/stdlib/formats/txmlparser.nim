discard """
description: '''
Basic tests for the xmlparser module
'''
"""

# note the module and tests might still be removed or heavily reworked, only
# including here to remove the testament hack for testing std lib files

let xmlData = r"""
<?xml version="1.0" encoding="UTF-8" ?>
<root>
  <tag>
    <test arg="blah" arg2="test"/>
    <test2>
      bla ah absy hsh
      hsh
      &woohoo;
      sjj
    </test2>
    <test><teh>bla</teh></test>
  </tag>
</root>


"""

import std/xmlparser
from std/xmltree import `$`

block parse_an_xml_string:
  ## parse an xml string, should work without raising an invalid xml exception
  try:
    discard parseXml(xmlData)
  except:
    doAssert false, "Should not result in any errors, got: " &
                    getCurrentExceptionMsg()

block unescape_escaped_characters:
  ## escape characters should preserve the spaces that follow
  ## original regressions: https://github.com/nim-lang/Nim/issues/1518
  let expected = "<tag>One &amp; two</tag>"
  try:
    doAssert $parseXml("<tag>One &amp; two</tag>") == expected,
      "failed to handle escape characters"
  except:
    doAssert false, "unexpected error with test, got: " &
                    getCurrentExceptionMsg()
    