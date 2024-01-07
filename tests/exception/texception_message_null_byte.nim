discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/13115
    newException terminates msg early from NULL byte, other IO does not
  . Error output produced by raise newException is inconsistent with other
    IO such as echo or stdout.write.
'''
matrix: ";-d:debug;-d:danger"
outputsub: "` and works fine! [Exception]"
exitcode: 1
"""

# `\0` not preserved on windows, so only the trailing part can be reliably
# tested for
const msg = "This char is `" & '\0' & "` and works fine!"

# bug https://github.com/nim-lang/nim/issues/13115
raise newException(Exception, msg)