# Language Specification Tests

Contained within are a series of tests which ensure the language behaves exactly as documented. You can consider it to be a form of documentation in itself.

## Organization

Specification is structured and written using bottom-up approach - first we
specify the most fundamental language elements, such as string and integer
literals, variables. Then we add scopes, control flow-altering constructs
and so on. When new language construct is added, it first is described
separately, and then, if needed, revised again to account for preexisting
language features. Separate language constructs should be placed in
different sections or files, but "revisions" can be exemplified in the same
test file.

You might think of specification as an aid to help you to write an own
implementation of nim, or support a new backend. First you implement the
most basic language features, then you progressively iterate them to
correctly match their behavior with everything that has been added prior to
that.

Example of how decision about file structuring is made - we need document
implementation of the iterators - they can return values, overload on
arguments, their behavior can be altered using keywords such as `break`,
`continue` and `yield`. This is already enough for us to decide that this
is not a "basic" feature, and formulate the prerequisites that language
implementation must have before implementing iterators.

1. Language must support overloading, which must've been tested in the
   "procedures" section.
2. `break` and `continue` must be at least recognized by the parser - their
   behavior might be different inside of an iterator body, so their
   behavior on `while`/`block` and so on is only important for the sake of
   consistency (you can *expect* `break` to work with iterators the same
   way it does with `while`)

When it comes to writing a test for the iterator itself we first start with
the most basic iterator that does not accept any arguments, and can only
`yield` values. We check if order of execution for statements is correct,
and then `yield` does really return the same value. Iterator must also
preserve it's state between execution transfers - we test it too.

Then we got to check how iterator implements `break` and `continue`, then
overloading and so on.

Different language constructs should be specified and tested in the same
order - first you imagine adding new feature to the language, specify it's
core properties (`yield`, control transfer, state preservation), then
expand the definition by providing a specification for interactions with
other language constructs (`break`, `continue`, `defer`).

The only part of the specification that does not follow these rules is
"atoms" section, that describes fundamental capabilities of the language
such as *expressions*, *statements*, *type*, as well as a constructs used
in the tests themselves.


## Contributing tests

To add new tests please use following template

```nim
discard """
description: '''
Description of the test itself. Brief list of checks
'''
"""

## Language feature documentation.

block example1:
  # ...



```

Please note that it is strongly encouraged to comment the tests as much as possible; the main objective is not simply *"list all possible combinations of all features"*; the objective, is instead, to provide a ***literate executable specification*** of the language.

## Error tests & specifications

It is also necessary to document compilation and runtime errors within, preferably with examples on how to fix the particular issue.

For error reporting you can use following template:


```nim
discard """
description: '''
Test description
'''

errormsg: '''
Actual error message
'''
"""

## Detailed! explanation for the error cause and possible solutions to it.

block working_example:

# One or more examples of working code

block failing_example:

# Example of the failing code

```


## File naming

- `t01_feature_name.nim` - start file name with number to explicitly order features.
- `t01_feature_name_fail.nim` - show example of the runtime failure
