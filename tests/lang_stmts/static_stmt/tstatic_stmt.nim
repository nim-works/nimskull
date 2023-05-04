discard """
description: "Tests static statement blocks"
"""

block regression_void_try_stmt:
  # static blocks are currently statements (void expressions), `vmgen` used to
  # produce code that just happened to work for most void expressions, but
  # failed when encountering the one below. This is what led to the distinction
  # of existing with an `opcEof` vs `opcRet`.
  static:
    try:
      discard
    finally:
      discard