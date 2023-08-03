discard """
  description: "Tuple types cannot reference themselves"
  errormsg: "illegal recursion in type 'Single'"
"""

type
  Single = (Single,)