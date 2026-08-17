import pkg/baz/other as baz_other
import pkg/qux/other as qux_other
assert baz_other.farewell() == "Goodbye from baz"
assert qux_other.farewell() == "Goodbye from qux"
proc farewell*(): string = "Goodbye from bar"