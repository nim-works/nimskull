#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module only contains ModuleId, as it's required by all other `data_*`
## modules within newast.

type
  ModuleId* = int32
  ## id for this module within a project compilation
  ## XXX: refactor along with `ModuleAst` as this is mostly to shim with the
  ##      existing compiler instead of properly handling module vs module in
  ##      a package vs during a specific compilation.