# Bootstrap

This directory describes the minimal parts of the language if one was just
starting on a barebone bootstrap compiler.

Focus is limited to the absolute minimum core syntax, literals, expressions,
statements, minimal control flow, basic routines, and primitive and trivial
compound types.

A conforming compiler would be barely functional, fail to compile most code
in the wild, but could be iterated upon.

# Notes for Spec Authors and Readers

## Assumptions

- nothing beyond a single module/file
- assertions/echo can be built-ins for a hypothetical implementation
- system module doesn't exist (built-in/hardcoding most things)

## Test Specifics

- `t10_empty_module` tests that an empty module is valid
