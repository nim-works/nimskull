discard """
  description: "Distinct types cannot reference themselves"
  errormsg: "illegal recursion in type 'Id'"
"""

type
  Id = distinct Id