Language elements that can affect control flow of code execution.

# Issues with This Section

All these should not be tested here but later:
- `try` and `defer`, advanced/syntatic sugar based constructs
- `proc` are used but not defined under `basics`
- `case` tightly relates to `enum`s and shouldn't be handled here
- `when` is an advanced concept for conditional compilation

# Old Description

_needs updating_

- `break`
- `if`
- `while`
- `case`
- `return`, `try/except/finally` and general procedure calls. Note - this
  section only specifies how control flow is transferred between bodies of
  the functions - everything related to the procedures themselves (argument
  passing, returning values, generic procedures and so on) is documented in
  the "core" section of the specification.

The `for` statement is not specified since its largely syntactic sugar based on
`while` loops and the yet to be specificed `iterator` routine kind.