#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains the core memory/data types for the compiler internals,
## it's part of an attempt to migrate the compiler towards a set of lowerings.
##
## The physical data model makes compromises for the sake of performance, this
## means that it does not always map cleanly to the logical model it supports.
## The logical model, or rather the information model, is conveyed by a set of
## `distinct` types that create logical views on top of the physical model.

import data_module, data_token, data_ast, data_normast

export ModuleId

type  
  ModuleData* = object
    ## stores the AST and various intermediate representation data for a module
    ##
    ## distinct types should be used to provide limited access views to this
    ## type in order to protect it from unwanted mutation or data change
    ## lifecycle violations
    ## 
    ## fields are kept in the order of how they're populated/mutated over time
    id*: ModuleId
      ## id created whenever a module is encountered
    tokens*: TokenList
      ## list of tokens, mutated while tokenizing, then read thereafter
    ast*: Ast
      ## ast, mutated based on parsing tokens, then read thereafter
    norm*: NormAst
      ## highlevel intermediate representation mutated based on the ast, then
      ## read thereafter

proc initModule*(id: ModuleId, hintLen: Natural): ModuleData =
  ## initialize a ModuleData, pre-allocate storage based on the `hintLen` to
  ## excessive allocations/moves.
  ## xxx: clean-up how size hinting works
  ModuleData(
    id:     id,
    ast:    initAst(hintLen),
    tokens: initTokenList(hintLen)
  )