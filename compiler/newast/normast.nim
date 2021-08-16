#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module is the new normalized AST, it's part of an attempt to migrate
## the compiler towards a set of lowerings.
##
## This is meant to analyze the AST (see `newast`) and produce a normalized AST
## that can be used for analysis (attribute grammar) and for generating
## intermediate representations.

import data, data_normast

type
  ModuleNorm* = distinct ModuleData

func id*(m: ModuleNorm): ModuleId = m.ModuleData.id
func tokens*(m: ModuleNorm): TokenList = m.ModuleData.tokens
func ast*(m: ModuleNorm): Ast = m.ModuleData.ast
func norm*(m: var ModuleNorm): var NormAst = m.ModuleData.hir
