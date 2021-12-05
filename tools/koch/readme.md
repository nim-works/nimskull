# The compiler build tool

This is the native source for the compiler build tool `koch`. There are two
parts to this tool: `koch.nim` (this directory) and [`koch.py`](../../koch.py)
(the launcher, located in the repository root).

## Architecture

All invocations of `koch.nim` is done via the launcher, which makes sure that
the bootstrapping compiler is up-to-date and `koch.nim` is built.

To ensure that the launcher and the native version are in sync, environment
variables are currently employed to communicate data between them. The `KOCH_`
prefix is used for those variables.
