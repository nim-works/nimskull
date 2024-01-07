discard """
  target: "vm"
  description: '''An empty exception name is overridden on raise when run in
                  the VM'''
  knownIssue
"""

# The VM treats `cstring(nil)` and `cstring("")` as the same thing

try:
  raise (ref CatchableError)(name: "")
except CatchableError as e:
  doAssert e.name == ""