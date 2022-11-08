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
  defineSymbol("nimhygiene") # deadcode
  defineSymbol("niminheritable") # deadcode
  defineSymbol("nimmixin") # deadcode
  defineSymbol("nimeffects") # deadcode
  defineSymbol("nimbabel") # deadcode
  defineSymbol("nimcomputedgoto") # deadcode
  defineSymbol("nimunion") # deadcode
  defineSymbol("nimnewshared") # deadcode
  defineSymbol("nimNewTypedesc") # deadcode
  defineSymbol("nimrequiresnimframe") # deadcode
  defineSymbol("nimparsebiggestfloatmagic") # deadcode
  defineSymbol("nimalias") # deadcode
  defineSymbol("nimlocks") # deadcode
  defineSymbol("nimnode") # deadcode pending `nimnode` reference in opengl package
    # refs https://github.com/nim-lang/opengl/pull/79
  defineSymbol("nimvarargstyped") # deadcode
  defineSymbol("nimtypedescfixed") # deadcode
  defineSymbol("nimKnowsNimvm") # deadcode
  defineSymbol("nimArrIdx") # deadcode
  defineSymbol("nimHasalignOf") # deadcode
  defineSymbol("nimDistros") # deadcode
  defineSymbol("nimHasCppDefine") # deadcode
  defineSymbol("nimGenericInOutFlags") # deadcode
  when false: defineSymbol("nimHasOpt") # deadcode
  defineSymbol("nimNoArrayToCstringConversion") # deadcode
  defineSymbol("nimHasRunnableExamples") # deadcode
  defineSymbol("nimNewDot") # deadcode
  defineSymbol("nimHasNilChecks") # deadcode
  defineSymbol("nimSymKind") # deadcode
  defineSymbol("nimVmEqIdent") # deadcode
  defineSymbol("nimNoNil") # deadcode
  defineSymbol("nimNoZeroTerminator") # deadcode
  defineSymbol("nimNotNil") # deadcode
  defineSymbol("nimVmExportFixed") # deadcode
  defineSymbol("nimHasSymOwnerInMacro") # deadcode
  defineSymbol("nimNewRuntime") # deadcode
  defineSymbol("nimIncrSeqV3") # xxx: turn this into deadcode
  defineSymbol("nimAshr") # deadcode
  defineSymbol("nimNoNilSeqs") # deadcode
  defineSymbol("nimNoNilSeqs2") # deadcode
  defineSymbol("nimHasUserErrors") # deadcode
  defineSymbol("nimUncheckedArrayTyp") # deadcode
  defineSymbol("nimHasTypeof") # deadcode
  defineSymbol("nimErrorProcCanHaveBody") # deadcode
  defineSymbol("nimHasInstantiationOfInMacro") # deadcode
  defineSymbol("nimHasHotCodeReloading") # deadcode
  defineSymbol("nimHasNilSeqs") # deadcode
  defineSymbol("nimHasSignatureHashInMacro") # deadcode
  defineSymbol("nimHasDefault") # deadcode
  defineSymbol("nimMacrosSizealignof") # deadcode

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
  defineSymbol("nimFixedForwardGeneric")
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
  defineSymbol("nimDoesntTrackDefects")
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
  defineSymbol("nimHasIterable")
  defineSymbol("nimHasTypeofVoid")
  defineSymbol("nimHasDragonBox")
  defineSymbol("nimHasHintAll")
  defineSymbol("nimHasTrace")
  defineSymbol("nimHasEffectsOf")
  defineSymbol("nimHasEnforceNoRaises")
  defineSymbol("nimHasNkComesFromNodeRemoved")
  defineSymbol("nimHasNkParForStmtNodeRemoved")
  defineSymbol("nimHasTyConceptRemoved")
  defineSymbol("nimHasNkBreakStateNodeRemoved")
