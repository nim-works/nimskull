discard """
"""

block ensure_lifting_is_not_confused_by_type_wrappers:
  ## previously this led to a code gen bug where a non-existent hidden type
  ## field was being copied. the various wrappers alias/distinct weren't being
  ## skipped when testing to see if the underlying type was an object.
  type WithHooks = object

  proc `=destroy`(x: var WithHooks) =
    # force `WithHooks` to require lifetime hooks
    discard

  type
    Object = object
      x: WithHooks # make sure that `Object` gets a synthesized copy hook
    Alias = Object
    Distinct = distinct Alias

  proc test(y: Distinct) =
    var x = y
    discard (addr x) # prevent `x` being turned into a cursor

  test(default(Distinct))
