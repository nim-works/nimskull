#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module handles the conditional symbols.

import
  std/strtabs

from compiler/front/options import Feature
import compiler/ast/reports


proc initDefines*(symbols: StringTableRef) =
  # for bootstrapping purposes and old code:
  template defineSymbol(s) =
    symbols[s] = "true"

  # > 0.20.0
  defineSymbol("nimNoZeroExtendMagic")
  defineSymbol("nimMacrosGetNodeId")
  for f in Feature:
    defineSymbol("nimHas" & $f)

  for s in repWarningKinds:
    defineSymbol("nimHasWarning" & $s)

  for s in repHintKinds:
    defineSymbol("nimHasHint" & $s)

  defineSymbol("nimFixedOwned")
  defineSymbol("nimHasStyleChecks")
  defineSymbol("nimToOpenArrayCString")
  defineSymbol("nimHasUsed")
  defineSymbol("nimnomagic64")
  defineSymbol("nimNewShiftOps")
  defineSymbol("nimHasCursor")
  defineSymbol("nimAlignPragma")
  defineSymbol("nimHasExceptionsQuery")
  defineSymbol("nimHasIsNamedTuple")
  defineSymbol("nimHashOrdinalFixed")

  defineSymbol("nimHasSinkInference")
  defineSymbol("nimNewIntegerOps")
  defineSymbol("nimHasInvariant")
  defineSymbol("nimHasStacktraceMsgs")
  defineSymbol("nimHasLentIterators")
  defineSymbol("nimHasDeclaredMagic")
  defineSymbol("nimHasStacktracesModule")
  defineSymbol("nimHasEffectTraitsModule")
  defineSymbol("nimHasCastPragmaBlocks")
  defineSymbol("nimHasDeclaredLocs")
  defineSymbol("nimHasJsBigIntBackend")
  defineSymbol("nimHasWarningAsError")
  defineSymbol("nimHasHintAsError")
  defineSymbol("nimHasSpellSuggest")
  defineSymbol("nimHasCustomLiterals")
  defineSymbol("nimHasUnifiedTuple")
  defineSymbol("nimHasDragonBox")
  defineSymbol("nimHasHintAll")
  defineSymbol("nimHasTrace")
  defineSymbol("nimHasEffectsOf")
  defineSymbol("nimHasNkComesFromNodeRemoved")
  defineSymbol("nimHasNkParForStmtNodeRemoved")
  defineSymbol("nimHasTyConceptRemoved")
  defineSymbol("nimHasNkBreakStateNodeRemoved")
  defineSymbol("nimHasTyOwnedRemoved")

  defineSymbol("nimskullReworkStaticExec")
  defineSymbol("nimskullNoMagicNewAssign")