discard """
  description: '''
    Regression test to make sure that locals inside the initializer of a global
    defined inside a procedure's body are properly destroyed
  '''
  targets: "c"
  knownIssue: "the `injectdestructors` pass is not invoked for the initializer"
"""

var wasDestroyed: bool
# don't initialize the global; doing so might override the value assgined
# during pre-initialization

# initialization of ``.global.``s currently happens *before* the code part
# of the module is executed, so if the destructor was called, ``wasDestroyed``
# has to be true here
doAssert wasDestroyed

type Resource = object

proc `=destroy`(x: var Resource) =
  wasDestroyed = true

proc prc() =
  var v {.global.} = block:
    var r = Resource() # `r` needs to be destroyed at the end of the block
    1

prc() # use the procedure so that the global is part of the alive code