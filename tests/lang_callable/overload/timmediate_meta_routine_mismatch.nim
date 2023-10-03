discard """
action: reject
description: "Ensure immediate macros/templates mismatches are reported"
"""

template unique(x, y: untyped) =
  ## A template (macro would work too) that is not overloaded. 
  # it's important that an argument is actually used within the template, in
  # order to force traveral of the actual arguments (it's lazy and driven by
  # the body).
  y

proc test[T]() =
  unique("test")

test()