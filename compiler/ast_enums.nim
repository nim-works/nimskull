## Module contains declarations of some of the enums used in the `ast.nim`
## module. Split was created to reuse the enums in the `reports.nim`
## without having to import the whole `ast.nim` (and thus avid cyclic
## dependencies)


type
  EffectsCompat* = enum
    efCompat
    efRaisesDiffer
    efRaisesUnknown
    efTagsDiffer
    efTagsUnknown
    efLockLevelsDiffer
    efEffectsDelayed

  MismatchKind* = enum
    ## Procedure call argument mismatch reason
    kUnknown
    kAlreadyGiven           ## Named argument already given
    kUnknownNamedParam      ## No such named parameter
    kTypeMismatch           ## Parameter type mismatch
    kVarNeeded              ## Parameter should be mutable
    kMissingParam           ## Missing procedure parameter
    kExtraArg               ## Too many arguments for a procedure call
    kPositionalAlreadyGiven ## Positional parameter has already been givend
    ## as a named parameter

  TTypeRelation* = enum      ## order is important!
    isNone
    isConvertible
    isIntConv
    isSubtype
    isSubrange               ## subrange of the wanted type; no type conversion
                             ## but apart from that counts as ``isSubtype``
    isBothMetaConvertible    ## generic proc parameter was matched against
                             ## generic type, e.g., map(mySeq, x=>x+1),
                             ## maybe recoverable by rerun if the parameter is
                             ## the proc's return value
    isInferred               ## generic proc was matched against a concrete type
    isInferredConvertible    ## same as above, but requiring proc CC conversion
    isGeneric
    isFromIntLit             ## conversion *from* int literal; proven safe
    isEqual

  TCallingConvention* = enum
    ccNimCall      = "nimcall"  ## nimcall, also the default
    ccStdCall      = "stdcall"  ## procedure is stdcall
    ccCDecl        = "cdecl"    ## cdecl
    ccSafeCall     = "safecall" ## safecall
    ccSysCall      = "syscall"  ## system call
    ccInline       = "inline"   ## proc should be inlined
    ccNoInline     = "noinline" ## proc should not be inlined
    ccFastCall     = "fastcall" ## fastcall (pass parameters in registers)
    ccThisCall     = "thiscall" ## thiscall (parameters are pushed right-to-left)
    ccClosure      = "closure"  ## proc has a closure
    ccNoConvention = "noconv"   ## needed for generating proper C procs sometimes

  ProcConvMismatch* = enum
    pcmNoSideEffect
    pcmNotGcSafe
    pcmLockDifference
    pcmNotIterator
    pcmDifferentCallConv
