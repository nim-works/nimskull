discard """
targets: native
errormsg: "Mixing types and values in tuples is not allowed."
"""

var bar = (int, 1)
