import pkg/bar
assert bar.greeter() == "Hello from bar!"
proc greeter*(): string = "Hello from foo!"