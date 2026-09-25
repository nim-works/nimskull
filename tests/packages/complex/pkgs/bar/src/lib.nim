import pkg/baz
import pkg/qux
assert baz.greeter() == "Hello from baz!"
assert qux.greeter() == "Hello from qux!"
proc greeter*(): string = "Hello from bar!"