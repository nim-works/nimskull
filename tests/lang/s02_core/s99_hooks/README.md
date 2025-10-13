## What belongs here

This section contains tests related to hook procedures, that is, procedures:
- to which calls are statically inserted by the compiler
- that are invoked by the runtime at run-time

This should cover:
- syntax
- restrictions on the routine definitions
- restrictions on the run-time behaviour (if any)
- where the hooks are injected

## Assumptions

- nothing beyond a single module/file
- assertions may still be built-ins
- user-defined types are supported and work
- procedures and calls thereof work
- `var T` is supported as a parameter's type
- raising and catching exceptions work
- tag effect tracking works