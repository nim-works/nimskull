discard """
  description: '''
    Ensure nested non-noreturn statements aren't treated as such
  '''
"""

let x =
  if true:
    10
  else:
    try:
      discard
    except:
      discard