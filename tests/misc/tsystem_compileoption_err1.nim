discard """
  description: "Regression tests to see if invalid option fails correctly"
  errormsg: "Invalid compiler option - 'bob'"
"""

static:
  discard compileOption("bob")