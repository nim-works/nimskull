discard """
  description: "Regression tests to see if invalid option fails correctly"
  errormsg: "Unexpected value for option 'exceptions'. Expected one of native, goto, but got 'bob'"
"""

static:
  discard compileOption("exceptions", "bob")