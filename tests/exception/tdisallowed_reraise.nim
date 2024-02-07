discard """
  description: '''
    Ensure that re-raise statements are rejected where required by the
    language specification.
  '''
  cmd: "$nim check --hints:off $options $file"
  action: reject
  nimoutFull: true
  nimout: '''
tdisallowed_reraise.nim(26, 5) Error: an exception can only be re-raised within the scope of an except, with no finally in-between
tdisallowed_reraise.nim(20, 3) Error: an exception can only be re-raised within the scope of an except, with no finally in-between
tdisallowed_reraise.nim(39, 5) Error: an exception can only be re-raised within the scope of an except, with no finally in-between
tdisallowed_reraise.nim(50, 7) Error: an exception can only be re-raised within the scope of an except, with no finally in-between
'''
"""


block standalone_reraise:
  # a standalone re-raise statement is disallowed
  raise

block reraise_in_procedure:
  proc reraise() =
    # the re-raise statement doesn't appear within an ``except`` clause's
    # scope
    raise

  try:
    raise CatchableError.newException("")
  except:
    # where the procedure is called has no effect on the rules
    reraise()

block reraise_in_finally:
  try:
    raise CatchableError.newException("")
  finally:
    # a re-raise statement within a finally clause is disallowed
    raise

block reraise_in_nested_finally:
  try:
    raise CatchableError.newException("")
  except:
    try:
      break
    finally:
      # a re-raise statement within a finally clause that's within an except
      # clause is still disallowed
      raise

block reraise_in_nested_try:
  # no error is reported here
  try:
    raise CatchableError.newException("")
  except:
    try:
      # a re-raise within a try's scope is allowed, if the try is within the
      # scope of an except
      raise
    finally:
      discard

block reraise_in_nested_finally_try:
  # no error is reported here
  try:
    discard
  finally:
    try:
      raise CatchableError.newException("")
    except:
      # a re-raise within an except within a finally, but not directly
      # within, is allowed.
      raise
