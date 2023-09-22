discard """
  description: '''
    Ensure that the code passed to a ``compiles`` is compiled as if appearing
    within the nearest explicit execution context
  '''
"""

# the ``break`` statement passed to the ``compiles`` procedure is
# wrapped in an ``if true:``, otherwise the code would not be
# syntactically valid

proc f(x: static bool): bool =
  x

# ``when`` condition
block label:
  # the ``when`` condition opens a new context, but it's implicit
  when compiles(if true: break label):
    discard "all fine"
  else:
    {.error: "`compiles` failed".}

# ``compiles`` within nested implicit contexts
block label:
  # each initializer expression happens within a new context, but they're
  # implicit
  const a = (const b = compiles(if true: break label); b)
  doAssert a

# ``compiles`` within implicit contexts created for arguments
block label:
  # the separate context the argument expression is anaylzed within
  # is *implicit*, so the ``break label`` statement is analyzed as if
  # within the context the ``block label`` is also part of
  doAssert f(compiles(if true: break label))

block label:
  static:
    # a ``static`` block opens an *explicit* context
    doAssert not compiles(if true: break label)

  # a coercion to ``static`` also opens an *explicit* context
  doAssert static(not compiles(if true: break label))