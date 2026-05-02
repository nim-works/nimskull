discard """
  output: '''is
finally
nice!'''
"""

import std/tables

const
  foo = {"ah": "finally", "this": "is", "possible.": "nice!"}.toTable()

# protect against overly smart compiler:
var x = "this"

echo foo[x]
x = "ah"
echo foo[x]
x = "possible."
echo foo[x]
