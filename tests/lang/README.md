# Language Specification Tests

Contained within are a series of tests which ensure the language behaves exactly as documented. You can consider it to be a form of documentation in itself.

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
- `t01_feature_name_run_fail.nim` - show example of the runtime failure
- `t01_feature_name_comp_fail.nim` - show compilation error related to the feature
- `t01_feature_name_warning.nim` - show compilation warning related to the feature