discard """
cmd: "nim check $options $file"
errormsg: "power of two expected"
labels: "alignment error_message"
description: '''
  . From https://github.com/nim-lang/Nim/pull/12643
    implemented alignas pragma
  . The difference compared to the C version is, this implementation requires
    an integer argument, in C a type argument is allowed as well. The reason
    for this limitation is simple: A solution that would cover imported types
    (unknown alignment to Nim) would be too much effort, at least for now.
'''
"""

proc foobar() =
  let something {.align(33).} = 123
