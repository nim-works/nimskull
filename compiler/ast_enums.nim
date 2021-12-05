## Module contains declarations of some of the enums used in the `ast.nim`
## module. Split was created to reuse the enums in the `reports.nim`
## without having to import the whole `ast.nim` (and thus avid cyclic
## dependencies)

type
  TSymKind* = enum
    ## the different symbols (start with the prefix sk);
    ## order is important for the documentation generator!
    skUnknown             ## unknown symbol: used for parsing assembler blocks
                          ## and first phase symbol lookup in generics
    skConditional         ## symbol for the preprocessor (may become obsolete)
    skDynLib              ## symbol represents a dynamic library; this is used
                          ## internally; it does not exist in Nim code
    skParam               ## a parameter
    skGenericParam        ## a generic parameter; eq in ``proc x[eq=`==`]()``
    skTemp                ## a temporary variable (introduced by compiler)
    skModule              ## module identifier
    skType                ## a type
    skVar                 ## a variable
    skLet                 ## a 'let' symbol
    skConst               ## a constant
    skResult              ## special 'result' variable
    skProc                ## a proc
    skFunc                ## a func
    skMethod              ## a method
    skIterator            ## an iterator
    skConverter           ## a type converter
    skMacro               ## a macro
    skTemplate            ## a template; currently also misused for user-defined
                          ## pragmas
    skField               ## a field in a record or object
    skEnumField           ## an identifier in an enum
    skForVar              ## a for loop variable
    skLabel               ## a label (for block statement)
    skStub                ## symbol is a stub and not yet loaded from the ROD
                          ## file (it is loaded on demand, which may
                          ## mean: never)
    skPackage             ## symbol is a package (used for canonicalization)
    skAlias               ## an alias (needs to be resolved immediately)
  TSymKinds* = set[TSymKind]

type
  EffectsCompat* = enum
    efCompat
    efRaisesDiffer
    efRaisesUnknown
    efTagsDiffer
    efTagsUnknown
    efLockLevelsDiffer
    efEffectsDelayed

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
