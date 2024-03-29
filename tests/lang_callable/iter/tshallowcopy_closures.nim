discard """
  target: "!js !vm"
  ccodecheck: "!@('{' \\s* 'NI HEX3Astate;' \\s* '}')"
  output: '''
a1 10
a1 9
'''
"""

# TODO: ccodecheck only works for the C target. Adjust the test to not depend
#       on inspecting the generated C code and enable it for the VM/JS target

# bug #1803

type TaskFn = iterator (): float

iterator a1(): float {.closure.} =
    var k = 10
    while k > 0:
        echo "a1 ", k
        dec k
        yield 1.0


iterator a2(): float {.closure.} =
    var k = 15
    while k > 0:
        echo "a2 ", k
        dec k
        yield 2.0

var
  x = a1
  y = a2
  z: TaskFn

discard x()
shallowCopy(z, x)
shallowCopy(z, y)
discard x()
