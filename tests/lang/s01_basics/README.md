# What Belongs Here?

Here we have tests for the very simple parts of the language, that we often
would not think to test. Mostly this captures minor trivia around:

- comments
- literals
- if, while and case statements
- code block and scopes
- primitive expressions
- Pure-nim user-defined types, without going into details about interfacing
  with C and C++ objects, pragmas and any other complications.

Tests in this section are mostly used to document how most basic interactions
are done - stuff you would find in almost any regular imperative programming 
language. Things that are more nim-specific should be placed in `s02_core` or
other sections.