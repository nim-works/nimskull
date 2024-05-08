discard """
  description: '''
    The `compile` pragma can also be used to compile all files matching a
    pattern, by providing a two-element tuple argument.
  '''
  targets: "c"
  joinable: false
"""

## The first operand is a file pattern for matching the C files to add to the
## build. Only `*.suffix` patterns are guaranteed to work. The pattern is
## relative to the current module's parent directory.
##
## The second operand specifies a pattern for the object file name. The object
## file(s) are always placed in the project's cache directory, even if the
## pattern evaluates to an absolute or relative file path. Similarily, the file
## extension in the pattern is ignored too.
{.compile: ("*.c", "~/cache/$1_generated.suffix").}

proc c_compiled_only(arg: cint): cint {.importc.}

doAssert c_compiled_only(1) == 2
