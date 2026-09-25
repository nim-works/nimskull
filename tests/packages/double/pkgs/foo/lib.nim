import pkg/barfoo
assert barfoo.greeter() == "Hello from bar!"
proc greeter*(): string = "Hello from foo!"