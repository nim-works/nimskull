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
