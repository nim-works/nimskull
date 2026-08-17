import pkg/complex
import quux/other
assert complex.greeter() == "Hello from complex!"
assert other.farewell() == "Goodbye from quux"
proc greeter*(): string = "Hello from quux!"