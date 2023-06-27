discard """
  target: "!js"
  outputsub: '''Error: unhandled exception: cannot dispatch; dispatcher is nil [NilAccessDefect]'''
  exitcode: 1
"""

# disabled on JS because sem/codegen lets it through and we get a runtime NPE

# bug #5599
type
    Base = ref object of RootObj
    Derived = ref object of Base

method inner(obj: Base) {.base.} =
    quit "to override"

method outer(obj: Base) {.base.} =
    echo "outer"
    obj.inner()

method inner(obj: Derived) =
    echo "inner Derived"

var x: Derived
x.outer()
