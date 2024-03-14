## What belongs here?

This section contains tests for inter- and intra-evaluation-order of
expressions and their sub-expressions.

The tests document where and how effects of expression are observable --
without effectful expression (i.e., expression that do more than just return
a value), order of evaluation would not matter.

## Assumptions

- nothing beyond a single module/file
- assertions might still be built-ins
- user-defined types are supported and work
- procedures and calls thereof work
- the `lent T` return type is supported
- `var T` is supported as a parameter's type