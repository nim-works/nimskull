
# the defaults must be set and the appropriate defines present
when defined(c) and defined(gcOrc):
  var myGlobal = 0

when defined(js):
  # not compiled
  var myGlobal2 = 0

myGloba#[!]#

discard """
$nimsuggest --tester $file
>sug $1
sug;;skVar;;tbackend_defines.myGlobal;;int;;*nimsuggest/tests/tbackend_defines.nim;;4;;6;;"";;100;;Prefix
"""