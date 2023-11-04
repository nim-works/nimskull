# What Belongs Here?

Here we have tests for the very simple parts of the language, that we often
would not think to test. Mostly this captures minor trivia around:

- comments
- literals and primitive data types
- primitive expressions
- if, while and case statements
- code block and scopes

Tests in this section are mostly used to document how most basic interactions
are done - stuff you would find in almost any regular imperative programming
language.

A conforming compiler would be able to compile simple expressions, but still
fail to compile most code in the wild as there would be no facilities for
abstraction such as procedures or user defined types.

# Notes for Spec Authors and Readers

## Assumptions

- nothing beyond a single module/file
- no type inference for variables
- assertions/echo might still be built-ins, per `s00_bootstrap`

## Test Specifics

- anything starting with `s99_` needs to be entirely reworked