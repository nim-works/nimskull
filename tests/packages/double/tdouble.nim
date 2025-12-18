import pkg/foo
import pkg/foo/other
import bar
import bar/other as bar_other

assert foo.greeter() == "Hello from foo!"
assert other.farewell() == "Goodbye from foo"
assert bar.greeter() == "Hello from bar!"
assert bar_other.farewell() == "Goodbye from bar"