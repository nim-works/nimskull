discard """
  joinable: false
"""
import pkg/foo
import pkg/foo/other as foo_other
import pkg/bar
import pkg/bar/other as bar_other
import pkg/baz
import pkg/baz.other as baz_other
import pkg/qux
import pkg/qux/other as qux_other
import pkg/quux
import pkg/quux/other as quux_other

assert foo.greeter() == "Hello from foo!"
assert foo_other.farewell() == "Goodbye from foo"
assert bar.greeter() == "Hello from bar!"
assert bar_other.farewell() == "Goodbye from bar"
assert baz.greeter() == "Hello from baz!"
assert baz_other.farewell() == "Goodbye from baz"
assert qux.greeter() == "Hello from qux!"
assert qux_other.farewell() == "Goodbye from qux"
assert quux.greeter() == "Hello from quux!"
assert quux_other.farewell() == "Goodbye from quux"