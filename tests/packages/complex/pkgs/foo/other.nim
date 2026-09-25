import pkg/bar.other as bar_other
assert bar_other.farewell() == "Goodbye from bar"
proc farewell*(): string = "Goodbye from foo"