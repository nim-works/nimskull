# Language Specification Tests

Contained within are a series of tests, that along with the comments therein
comprise an executable specification of the language.

## Organization

Specification is structured and written using bottom-up approach - first we
specify the most fundamental language elements (comments, identifiers, blocks),
then layers on additional capabilities building upon the previous. It is
written as a conformance test suite starting from a barely functional barebones
compiler up to a full featured current version.

Generally speaking, each language constructure should be described in its own
test spec(s), with separate additional specs where interaction with other
capabilities requires further clarification, regression testing, and so on.

### Example Approach to Adding Specs

Example of how decision about file structuring is made - we need document
implementation of the iterators - they can return values, overload on
arguments, their behavior can be altered using keywords such as `break`,
`continue` and `yield`. This is already enough for us to decide that this
is not a "basic" feature, and formulate the prerequisites that language
implementation must have before implementing iterators.

1. Language must support overloading, which itself must be tested in a prior
   section.
2. `break` and `continue` must be at least recognized by the parser - their
   behavior within `while`, `block`, etc must be tested so we can ensure
   they're consistent when used with iterators (equivalent coverage).

When it comes to writing a test for the iterator itself we first start with
the most basic iterator that does not accept any arguments, and can only
`yield` values. We check if order of execution for statements is correct,
and then `yield` returns the correct value, and control is passed correctly,
with state being preserved as requried.

At this point we should test `break` and `continue`, and then proceed to
overload resolution.

Different language constructs should be specified and tested in the same
order - first you imagine adding new feature to the language, specify its
core capabilities (`yield`, resumption) and properties (control flow and state
preservation), then expand the definition by providing a specification for
interactions with other language constructs (`break`, `continue`, `defer`).


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

Please note that it is strongly encouraged to comment the tests as much as
possible; the main objective is not simply *"list all possible combinations of
all features"*; the objective, is instead, to provide a
***literate executable specification*** of the language.

## Error Specifications

It is also necessary to document compilation and runtime errors within,
preferably with examples on how to fix the particular issue.

For error specs you can use following template:


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


## File Naming Conventions

- `t01_feature_name.nim` - start with number to explicitly order features.
- `t01_feature_name_fail.nim` - show example of the runtime failure
