discard """
  description: '''
    Regression test to make sure that locals inside the initializer of a global
    defined inside a procedure's body are properly destroyed
  '''
  targets: "c js vm"
"""

var wasDestroyed {.noInit.}: bool
# don't initialize the global; doing so would override the value assgined
# during pre-initialization

# initialization of ``.global.``s happens *before* the module's top-level
# code is run, so if the destructor was called, ``wasDestroyed`` has to be
# 'true' here
doAssert wasDestroyed

type Resource = object

proc `=destroy`(x: var Resource) =
  wasDestroyed = true

proc prc() =
  var v {.global.} = block:
    var r = Resource() # `r` needs to be destroyed at the end of the block
    1
  doAssert v == 1

prc() # use the procedure so that the global is part of the alive code