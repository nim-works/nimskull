proc getGlobal(): int = 
  ## `state` variable is stored globally (that is - not destroyed when procedure finishes
  ## execution), but can only be accessed inside of the procedure body.
  var state {.global.}: int
  inc state
  return state

doAssert getGlobal() == 1
doAssert getGlobal() == 2