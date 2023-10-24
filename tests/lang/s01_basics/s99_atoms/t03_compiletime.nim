discard """
description: '''
Execution of the code at compilation time.
'''
output: '''
echo outside of static
'''
"""

# Given this is a specification I don't think it is an appropriate place to
# start a lenghtly introduction to what "compile time is", but considering it is
# a *fundamental* part of the Nimskull language it would not hurt to start with
# some definitions before going into concrete details.

static:
  ## This code will be executed *during compilation* of the project
  echo "echo inside of static"

## This code will be executed only when compiled binary is run.
echo "echo outside of static"

static:
  ## This code will also be executed during compilation.
  echo "second echo inside of static"

## It is possible to define compile-time variable using `{.compiletime.}` pragma
## in the variable declaration. This variable will not be accessible from
## regular runtime code, but can be used in `macro` or `static:`.

var globalCompiletimeInt {.compiletime.}: int = 12

macro compiletimeGetter() =
  echo "current value of the compiletime: ", globalCompiletimeInt

compiletimeGetter()

static:
  globalCompiletimeInt = 12

compiletimeGetter()

## Data can be moved from `compiletime` variables into `const`

const canAccessAtRuntime = globalCompiletimeInt

doAssert canAccessAtRuntime == 12

## Note that subsequent modifications of the compiletime variable will not
## affect value of the constant after it has been declared.

static:
  globalCompiletimeInt = 24

const newConst = globalCompiletimeInt

doAssert newConst == 24
doAssert canAccessAtRuntime == 12
