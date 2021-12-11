#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## module handles reporting errors, it's used in conjunction with errorhandling
##
## Todo:
## * write an error reporting proc that handles string conversion and also
##   determines which error handling strategy to use doNothing, raise, etc.

import ast, errorhandling, renderer, strutils, astmsgs, types, options, reports
from msgs import TErrorHandling

export compilerInstInfo, walkErrors, errorKind
# export because keeping the declaration in `errorhandling` acts as a reminder
# of what the contract is with the subtleties around line and column info
# overloading

proc errorHandling*(err: PNode): TErrorHandling =
  ## which error handling strategy should be used given the error, use with
  ## `msg.liMessage` when reporting errors.
  assert err.isError, "err can't be nil and must be an nkError"
  case err.errorKind:
    of rsemCustomGlobalError: doRaise
    of rsemFatalError: doAbort
    else: doNothing
