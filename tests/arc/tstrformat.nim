discard """
  output: '''
verstuff
'''
  matrix: "--gc:arc"
  targets: c
  knownIssue: '''
    Semantic analysis duplicates the analysed argument into each usage
    inside the template. Since it contains definitions of local variables,
    multiple definitions of the same entity exist in the expanded code, which
    is illegal at the MIR level, thus making the `injectdestructors` pass fail
    '''
"""

# TODO: the test is not directly related to ``--gc:arc`` -- it's a test for the
#       ``injectdestructors`` pass. Reduce it further and move it to a
#       different category

# bug #13622

import strformat

template necho*(args: string) {.dirty.} =
  if getCurrentException() != nil:
    echo args
  else:
    stdout.writeLine(args)

proc main(cond: bool; arg: string) =
  if cond:
    necho &"ver{arg}\n"

main(true, "stuff")
