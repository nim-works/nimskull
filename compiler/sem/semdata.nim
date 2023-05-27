#
#
#           The Nim Compiler
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains the data structures for the semantic checking phase.

import
  std/[
    intsets,
    sets,
    tables,
    strutils
  ],
  compiler/front/[
    options,
    msgs,
  ],
  compiler/ast/[
    ast,
    astalgo,
    idents,
    renderer,
    lineinfos,
    linter,
    trees,
    wordrecg,
  ],
  compiler/modules/[
    magicsys,
    modulegraphs,
  ],
  compiler/vm/[
    vmdef,
  ],
  compiler/ic/[
    ic
  ],
  compiler/utils/[
    pathutils,
    astrepr,
  ]

from compiler/ast/reports_sem import reportAst,
  reportSym

# TODO: `ReportKinds` is being abused for notes again, it covers far too much
from compiler/ast/report_enums import ReportKinds,
  ReportKind

export TExprFlag, TExprFlags

type
  TOptionEntry* = object      ## entries to put on a stack for pragma parsing
    options*: TOptions
    defaultCC*: TCallingConvention
    dynlib*: PLib
    notes*: ReportKinds
    features*: set[Feature]
    otherPragmas*: PNode      ## every pragma can be pushed
    warningAsErrors*: ReportKinds

  POptionEntry* = ref TOptionEntry
  PProcCon* = ref TProcCon
  TProcCon* {.acyclic.} = object ## procedure context; also used for top-level
                                 ## statements
    owner*: PSym              ## the symbol this context belongs to
    resultSym*: PSym          ## the result symbol (if we are in a proc)
    nestedLoopCounter*: int   ## whether we are in a loop or not
    nestedBlockCounter*: int  ## whether we are in a block or not
    next*: PProcCon           ## used for stacking procedure contexts
    mappingExists*: bool
    mapping*: TIdTable
    caseContext*: seq[tuple[n: PNode, idx: int]]
    localBindStmts*: seq[PNode]

    inStaticContext*: int
      ## > 0 if we are inside a ``static`` block/expression or initializer
      ## expression of a ``const``
      ##
      ## written:
      ##  - semexprs: save/restore in ``tryExpr``, inc/dec in ``semStaticExpr``
      ##  - semstmts: inc/dec around ``semConst`` and ``semStaticStmt``
      ## read:
      ##  - semBindSym: whether to resolve the binding or not
      ##  - inCompileTimeOnlyContext

  TMatchedConcept* = object
    candidateType*: PType
    prev*: ptr TMatchedConcept
    depth*: int

  TInstantiationPair* = object
    genericSym*: PSym
    inst*: PInstantiation


  ImportMode* = enum
    importAll, importSet, importExcept
  ImportedModule* = object
    m*: PSym
    case mode*: ImportMode
    of importAll: discard
    of importSet:
      imported*: IntSet          # of PIdent.id
    of importExcept:
      exceptSet*: IntSet         # of PIdent.id

  PContext* = ref TContext

  # Refactoring of `TContext` in progress:
  # --------------------------------------
  #
  # TContext is used for a lot of stuff that it probably shouldn't be. In terms
  # of compiler architecture this is post parsing, so we're taking parsed code
  # and evaluating it. The result of this evaluation should be a value, an
  # error, or code that can be compiled (think of the exe as a partially
  # evaluated function, waiting for params to finish and become a value).
  #
  # So we're analysing, interpreting, and compiling, etc... that means we'll
  # have various environments, overall compilation, host env, nested
  # compilations, scopes, symbol tables, type environments, etc... all of these
  # have various lifetimes where they're written/updated and then read/queried.
  #
  # `TContext` is meant to do that, but:
  # - some things should be in the AST/IR itself (long-term)
  # - read and write lifetimes are unclear (first thing to figure out)
  # - there are latent bugs because of the previous point
  #
  # Strategy for fixing `TContext`:
  # 1. figure out when things are written/change
  # 2. know the various consumers/uses
  # 3. limit the lifetime of writes and ensure ordered reads
  # 4. refined the above until the code is not dumb
  # 
  # Notes about documenting `TContext` (step 1 and 2 above):
  # 1. what writes something
  # 2. what reads something/why
  # 3. (optional) additional collective purpose of 1/2
  # 4. what's the write lifetime
  # 5. what's the read lifetime
  # 6. is the overall lifetime less than that of the modules?

  TContext* = object of TPassContext ## a context represents the module
                                     ## that is currently being compiled

    # -------------------------------------------------------------------------
    # start: scope/traversal lifetime
    # -------------------------------------------------------------------------
    enforceVoidContext*: PType
      ## for `if cond: stmt else: foo`, `foo` will be evaluated under
      ## enforceVoidContext != nil; meaning we infered the `if` to contain a
      ## `stmt` (void) and so `foo` must result in an expression that can be
      ## `void`. Which plays into discard checks.
      ##
      ## It's used as a sentinel value, setting a node's typ field to this can
      ## then be compared with later to raise errors or to see if something is
      ## meant to be in the void context.
      ##
      ## `enforceVoidContext` is not the `void` type, see `voidType`
      ## 
      ## written: once at sem start
      ## read:
      ##  - semExprWithType: check if an expr has no type
      ##  - semAsgn: ensure assignments aren't an expression
      ##  - semstmts: for/while/etc stmt enforcement 
    currentScope*: PScope
      ## current scope based on semantic analysis' traversal through the ast,
      ## then read during lookups of various symbols/idents
      ##
      ## written: sem/exprs/call/...
      ## read: lookups
    p*: PProcCon
      ## procedure context used all over semantic analysis. Tracks various
      ## aspects of when traversal is in a proc, top level module, blocks,
      ## loops, etc
      ##
      ## written: all over sem
      ## read: all over sem
    # xxx: semtempl for example has a specialized context, maybe we should pull
    #      this out too? it's not straightfoward, but even an attempt will
    #      clean things up

    # type related contexts
    matchedConcept*: ptr TMatchedConcept
      ## the current concept being matched
      ##
      ## written:
      ##  - seminst: start of generateInstance save old one, at end restore it
      ##  - sigmatch matchUserTypeClass
      ## read:
      ##  - afterCallActions
      ##  - semExpr after ident/sym lookup and type nodes nkTypeOfExpr, etc
      ##  - semstmts var/let, discard, list
      ##  - sigmatch: typeRel and friends
    inTypeContext*: int
      ## track whether we're in a "type context", eg: type section, used by
      ## `suggest`, but that might leak into other parts of sem.
      ##
      ## written: semgnrc, semstmts, semtempl, semtypes
      ## read: suggest
    inConceptDecl*: int
      ## whether we're in a concept declaration, `concepts` tracks this so we
      ## can differentiate whether it's the concept description vs the concept
      ## check (hence need implementations and should error without one)
      ##
      ## written: concepts
      ## read: semIterator (one line for errors, seems janky)
      ##
      ## xxx: some refactoring should allow for removal

    # template and general instantiation
    templInstCounter*: ref int
      ## gives each template instantiation a unique id
      ##
      ## written:
      ##  - init once at sem start per module
      ##  - strung along to each new context per macro
      ##    or template eval; it's mutable
      ##  - evalTemplate increments it
      ## read: captured and read in `TemplCtx`
    instCounter*: int
      ## To prevent endless macro pragma and generic instantiations.
      ##
      ## written:
      ##  - pragmas: track macro pragmas expansions
      ##  - seminst: track generic instantiations
      ## read:
      ##  - pragmas: check count and error
      ##  - seminst: check count and error
    inGenericContext*: int
      ## > 0 if we are in a generic type
      ##
      ## written:
      ##  - semexprs: save/restore in `tryExpr`
      ##  - semstmts: `typeSectionRighSidePass`, guard `semTypeNode`
      ## read:
      ##  - semexprs: checked in `semIs`
      ##  - semInst: `generateInstance` guard instantiation until we're out
      ##  - semtypes: `semTypeNode` and friends guard instantiation
    inGenericInst*: int
      ## > 0 if we are instantiating a generic, weird this seems an awful lot
      ## like `instCounter`
      ## 
      ## written:
      ##  - semexprs: save/restore in `tryExpr`
      ##  - semstmts: `typeSectionRighSidePass`, guard `semTypeNode`
      ## read:
      ##  - semexprs: checked in `semIs`
      ##  - semInst: `generateInstance` guard instantiation until we're out
      ##  - semtypes: `semTypeNode` and friends guard instantiation
    generics*: seq[TInstantiationPair]
      ## pending list of instantiated generics to compile accumulates generics
      ## to compile for the backend on module close
      ## 
      ## written:
      ##  - semexprs: save/restore in `tryExpr`
      ##  - seminst: `fixupInsantiatedSymbols` scan generics looking for a
      ##             match based on symbol id
      ## read:
      ##  - sem: module close read and add to module ast
      ##  - seminst: `fixupInsantiatedSymbols` updates the generic, one reason
      ##              it's mutated is to support forward declarations
    lastGenericIdx*: int      ## used for the generics stack (`generics`)
                              ##
                              ## written/read:
                              ##  - sem: updated after each close of a module,
                              ##         why? xxx: is this because of suggest?
                              ## NB this isn't preserved restored in `tryExpr`,
                              ##    but it's not mutated anywhere else, suggest
                              ##    seems to be the only thing that might need
                              ##    this and it's likely a leak -- growing over
                              ##    time.

    # hlo??
    inUnrolledContext*: int    ## > 0 if we are unrolling a loop
                               ##
                               ## written:
                               ##  - semexprs: save/restore in `tryExpr`
                               ##  - semfields: inc/dec guards in semForFields
                               ##               and semForObjectFields
                               ## read: `semVarOrLet` to determine if the var
                               ##       or let is shadowed
                               ## xxx: we don't read it for const, but prior to
                               ##      evaluation aren't they shadowed?

    # recursive compilation `compiles` magic
    compilesContextId*: int 
      ## > 0 if we are in a ``compiles`` magic
      ## 
      ## written:
      ##  - semexprs: save/restore in `tryExpr`
      ##              xxx: only for 1 level nesting some generics related hacks
      ##  - suggest: inc/dec likely used to supress excessive error output
      ## read:
      ##  - lookups: guard error output and cascading errors during compiles
      ##  - semcall: guard diagnostic/errors during compiles
      ##  - semexpr: check imports are top level
      ##  - seminst: use to set/fetch from the generic cache
      ##  - sempass2: guard diagnostics during compiles context
      ##  xxx: much of this simplifies with nkError
    compilesContextIdGenerator*: int
      ## sequence to generate `compilesContextId` values, only gets bumped when
      ## we go from a non-compilesContext to a compiles context, but not from
      ## compiles to compiles context.
      ## 
      ## written:
      ##  - semexprs: save/restore in `tryExpr`; increment if we're heading
      ##              into a nested compile from the level
      ##              xxx: only for 1 level nesting some generics related hacks
      ## read:
      ##  - `tryExpr`: read and assigned to `compilesContextId`

    # pragma stack varies by traversal. maybe base entries are module wide?
    optionStack*: seq[POptionEntry]
      ## when pragmas are pushed/popped
      ##
      ## written:
      ##  - pragmas: pushed calling convention, dyn lib, etc or process pop
      ##  - semdata: setters used to init compiler settings etc
      ## read:
      ##  - pragmas: check to see if proc should remove from stacktrace and
      ##             which implicit pragmas to include
      ##  - semtypes: if there are options then create a dummy proc type node
      ##              to merge pragmas... xxx: this seems silly

    # object construction, signature matching (named args?)
    inUncheckedAssignSection*: int
      ## if we're in an unassigned check, via a pragma block
      ##
      ## written:
      ##  - sempragmablock in semstmts
      ## read:
      ##  - sigmatch: islvalue
      ##  - semobjconstr: errors for case object/discriminator assignments
      ##  - analyseIfAddressTakenInCall: check to see if we're in an unsafe
      ##                                 assignment block

    # using statement parameter tracking
    signatures*: TStrTable
      ## stores types for params in `using` statements
      ## 
      ## written:
      ##  - semUsing: adds an skParam symbol for lookup in semProcTypeNode
      ## read:
      ##  - semProcTypeNode: lookup of params defined via the `using` statement
    # -------------------------------------------------------------------------
    # end: scope/traversal lifetime
    # -------------------------------------------------------------------------


    # -------------------------------------------------------------------------
    # start: module env; module lifetime
    # -------------------------------------------------------------------------

    # lookups: imports / scopes
    module*: PSym
      ## the module sym belonging to the context used everywhere
    topLevelScope*: PScope 
      ## scope for all top-level symbols of the module as in the module's scope
      ## 
      ## written:
      ##  - sem: `myopen` is called per module, and we open a new module scope
      ##         and reset in `recoverContext` for exception recovery
      ## read:
      ##  - lookups: for symbol search and check if a sym is at the top level
      ##  - semexprs: `semexpr` guard for no top level defer stmts and in
      ##              `lookupForDeclared` for module qualified symbols
      ##  - suggest: iterate through top level symbols for suggestions
    imports*: seq[ImportedModule]
      ## scope for all imported symbols
      ##
      ## written:
      ##  - importer: written when adding imports
      ## read:
      ##  - importer: look for duplicate imports
      ##  - lookups: to scan for imported symbols
    friendModules*: seq[PSym] 
      ## friend modules; may access private data; this is used so that generic
      ## instantiations can access private object fields.
      ##
      ## written:
      ##  - semdata: for each `newContext` a module is its own friend
      ##  - semAfterMacroCall: calling a macro declared in another macro adds
      ##                       the declaring module as a friend for the life of
      ##                       the `semAfterMacroCall`, then we remove it
      ##  - generateInstance: instancing a generic, like macro above, makes the
      ##                      declaring module a friend for the lifetime of the
      ##                      instancing, then we remove it
      ## read:
      ##  - suggest: scan for visible fields in `fieldVisible` which is used
      ##             beyond suggestions
    
    # lookups: imports / scopes, technical whole program but maybe sandboxed
    moduleScope*: PScope
      ## scope where we shove module symbols, the module itself, and other
      ## modules that are imported.
      ## 
      ## written:
      ##  - lookups: push error symbols here to avoid cascading errors in
      ##             interactive or outside of nested compilation contexts
      ##  - sem: init in `myopen`, then add itself, during system import add
      ##         the system module symbol
      ## read:
      ##  - implicitly read as it's part of the chain of scopes

    # module statistics
    topStmts*: int
      ## counts the number of encountered top level statements
      ##
      ## written:
      ##  - sem: incremented in semStmtAndGenerateGenerics
      ## read:
      ##  - semData: query if it's the first top level statement

    # recursive includes detection
    includedFiles*: IntSet
      ## used to detect recursive include files
      ##
      ## written:
      ##  - semdata: initialized
      ##  - semstmts: remove included file after processing
      ##              xxx: this is buggy and leads to bad error messages, we
      ##                   should instead mark it as already processed for nice
      ##                   errors about duplicate includes and declarations
      ## read:
      ##  - semstmts: check for recursive includes

    # error reporting and suggestions
    unusedImports*: seq[(PSym, TLineInfo)]
      ## tracks which imports are unused, assume all new imports are unused,
      ## then remove them from here as we encounter usages.
      ## 
      ## written:
      ##  - importer: add all imported module as unused
      ##  - suggest: `markOwnerModuleAsUsed`, used beyond suggestions
      ## read:
      ##  - sem: on pass close report all unused modules

    features*: set[Feature]
      ## compiler feature flags, can be varied via pragmas, so per module;
      ## maybe that shouldn't be a thing?
      ## 
      ## written:
      ##  - pragmas: `processExperimental` enable experimental features
      ##  - semdata: init, push/pop pragma options
      ##  - sempass2: save/restore features during strict nil checks
      ##  - nileval: initialized for the interpreter
      ## read:
      ##  - lexer: for unicode operators
      ##  - semcall: implicit deref
      ##  - lookups: query overloadable enums
      ##  - semgnrc: query overloadable enums
      ##  - semtempl: query overloadable enums
      ##  - semtypes: query overloadable enums and strict not nil
      ##  - options: query nil checks enabled
      ##  - sempass2: query strict effects, views, funcs, and not nil
      ##  - typeallowed: query views
      ##  - semmagic: query dynamic bind sym
      ##  - semstmts: query dot and call operators
      ##  - vm/gen/ops: query allowInfiniteLoops and vmopsDanger
    importModuleMap*: Table[int, int]
      ## module.id => module.id; the key is an imported module, and the value
      ## is the real module in case of alias
      ##
      ## written:
      ##  - importer: track imported module/alias and the real module
      ## read:
      ##  - suggest: `markOwnerModuleAsUsed`, used beyond suggestions, query
      ##             imported module to exclude from unused modules
    exportIndirections*: HashSet[(int, int)]
      ## (module.id, symbol.id); the first is the exporting/origin module and
      ## the second is the exported symbol
      ##
      ## written:
      ##  - importer: track if we're reexporting a symbol
      ## read:
      ##  - suggest: `markOwnerModuleAsUsed`, used beyond suggestions, query
      ##             export indirections to track usage info
    recursiveDep*: seq[tuple[importer, importee: FileIndex]]
      ## used to detect recursive `importer` issues populated by importer and
      ## used by `lookups`
      ## 
      ## written:
      ##  - importer: add dependency
      ##  - lookups: truncated to avoid excessive nim check errors
      ## read:
      ##  - lookups: query if for possible recursive dependencies
    # -------------------------------------------------------------------------
    # end: module env; module lifetime
    # -------------------------------------------------------------------------


    # -------------------------------------------------------------------------
    # start: caches/common defs, likely whole program, per modules sandboxing?
    # -------------------------------------------------------------------------
    
    # ident cache used everywhere; sandboxed to avoid pollution?
    cache*: IdentCache
      ## used everywhere for identifier caching

    # module graph, access back to the whole compilation
    graph*: ModuleGraph
      ## access to the compilation graph, used everywhere

    # type lookups
    voidType*: PType
      ## for typeof(stmt)
      ## 
      ## written: sem: initialized
      ## read: semExprWithType: set type for ast to void

    intTypeCache*: array[-5..32, PType]
      ## cache some common integer types to avoid type allocations
      ##
      ## written/read entirely in semdata `getIntLitType`
    nilTypeCache*: PType
      ## store the nil type
      ##
      ## written/read in semexprs getNilType
      # xxx: seems weird we don't just init it like the voidType

    # symbols/dispatch
    converters*: seq[PSym]
      ## track current converters and then look them up during dispatch in
      ## `sigmatch`.
      ##
      ## written:
      ##  - semdata: init and append in `addConverter` which is used when a
      ##             converter is defined or imported
      ## read:
      ##  - sigmatch: to query converters
    pureEnumFields*: TStrTable
      ## pure enum fields that can be used unambiguously
      ## 
      ## written:
      ##  - importer: add when a pure enum field is declared or imported
      ##  - semdata: init
      ## read:
      ##  - lookups: search for pure enums if all else fails
      ##  - semgnrc: lookup pure enums

    # term rewriting/hlo
    patterns*: seq[PSym]
      ## sequence of pattern matchers used for term rewriting and also `hlo`,
      ## where highlevel optimizations are effectively pattern matches.
      ## 
      ## written:
      ##  - semdata: init and updated via addPattern, which isn't used by hlo??
      ##  - hlo: `applyPatterns` add patterns for optimizations
      ## read:
      ##  - hlo: check len to see if there are patterns to apply
    
    # pragmas and codegen ref
    userPragmas*: TStrTable
      ## user pragma symbols
      ## 
      ## written:
      ##  - pragmas: add user defined pragmas as encountered
      ##  - semdata: init
      ## read:
      ##  - pragmas: query user pragmas to see if we need to process it now
      ##  - semstmts: query user pragmas for proc/conv/etc annotation lookup
      ##  - semtypes: query user pragmas for type section pragmas
    libs*: seq[PLib]
      ## all libs used by this module, mostly setup pragmas.
      ##
      ## written:
      ##  - semdata: init
      ##  - pragmas: add to dynamic libs
      ## read:
      ##  - pragmas: query dynamic libs

    # hacks used for lookups
    isAmbiguous*: bool # little hack <-- it never is "little"
      ## track whether ident lookups are in ambiguous mode
      ## 
      ## written:
      ##  - lookups: set so it can be fetched in `semexpr`
      ##  - semexpr: set prior to lookup
      ## read:
      ##  - semexpr: queried to see how to treat symbol ambiguity
    # -------------------------------------------------------------------------
    # end: caches/common defs, likely whole program, per modules sandboxing?
    # -------------------------------------------------------------------------


    # -------------------------------------------------------------------------
    # start: not entirely clear why, function pionters for certain sem calls?
    # -------------------------------------------------------------------------
    semConstExpr*: proc (c: PContext, n: PNode): PNode {.nimcall.} # for the pragmas
      ## used to break cyclic dependencies, init in sem during module open and
      ## read in pragmas, semmagic, and semtypinst
    semExpr*: proc (c: PContext, n: PNode, flags: TExprFlags = {}): PNode {.nimcall.}
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in concepts, pragmas, semtypinst, sigmatch, and suggest
    semTryExpr*: proc (c: PContext, n: PNode, flags: TExprFlags = {}): PNode {.nimcall.}
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in sigmatch
    semTryConstExpr*: proc (c: PContext, n: PNode): PNode {.nimcall.}
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in semcall and sigmatch
    computeRequiresInit*: proc (c: PContext, t: PType): bool {.nimcall.}
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in semtypinst
    hasUnresolvedArgs*: proc (c: PContext, n: PNode): bool
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in pragmas
    semOperand*: proc (c: PContext, n: PNode, flags: TExprFlags = {}): PNode {.nimcall.}
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in semcall and sigmatch
    semConstBoolExpr*: proc (c: PContext, n: PNode): PNode {.nimcall.} # XXX bite the bullet
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in pragmas
    semOverloadedCall*: proc (c: PContext, n: PNode,
                              filter: TSymKinds, flags: TExprFlags): PNode {.nimcall.}
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in pragmas and semtypinst
    semTypeNode*: proc(c: PContext, n: PNode, prev: PType): PType {.nimcall.}
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in pragmas
    semInferredLambda*: proc(c: PContext, pt: TIdTable, n: PNode): PNode
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in sigmatch
    semGenerateInstance*: proc (c: PContext, fn: PSym, pt: TIdTable,
                                info: TLineInfo): PSym
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in sigmatch
    instTypeBoundOp*: proc (c: PContext; dc: PSym; t: PType; info: TLineInfo;
                            op: TTypeAttachedOp; col: int): PSym {.nimcall.}
      ## read to break cyclic dependencies, init in sem during module open and
      ## read in liftdestructors and semtypinst
    # -------------------------------------------------------------------------
    # end: not entirely clear why, function pionters for certain sem calls?
    # -------------------------------------------------------------------------


    # -------------------------------------------------------------------------
    # start: sempass2; updated via traversal, lifetime module
    # -------------------------------------------------------------------------
    sideEffects*: Table[int, seq[(TLineInfo, PSym)]]
      ## symbol.id => (info, sym); indexed on symbol id, tracks effects per sym
      ## 
      ## written/read in sempass2, for marking and listing side effects
    # -------------------------------------------------------------------------
    # end: sempass2; updated via traversal, lifetime module
    # -------------------------------------------------------------------------


    # -------------------------------------------------------------------------
    # start: hlo; module lifetime
    # -------------------------------------------------------------------------
    hloLoopDetector*: int
      ## used to prevent endless loops in the HLO
      ## 
      ## written/read in hlo, detect if hlo pattern
      ## applications are looping
    # -------------------------------------------------------------------------
    # end: hlo; module lifetime
    # -------------------------------------------------------------------------


    # -------------------------------------------------------------------------
    # start: suggestion/code completion; lifetime unsure
    # -------------------------------------------------------------------------
    suggestionsMade*: bool
      ## track whether suggestion were made, written/read in sem
    lastTLineInfo*: TLineInfo
      ## last line info used in suggest by `markUsed` to track deprecated
      ## symbol locations
    # -------------------------------------------------------------------------
    # end: suggestion/code completion; lifetime unsure
    # -------------------------------------------------------------------------

template config*(c: PContext): ConfigRef = c.graph.config

func isfirstTopLevelStmt*(c: PContext): bool =
  ## `true` if this is the first top level statement encountered for this
  ## module pass.
  c.topStmts == 0

proc getIntLitType*(c: PContext; literal: PNode): PType =
  # we cache some common integer literal types for performance:
  let value = literal.intVal
  if value >= low(c.intTypeCache) and value <= high(c.intTypeCache):
    result = c.intTypeCache[value.int]
    if result == nil:
      let ti = getSysType(c.graph, literal.info, tyInt)
      result = copyType(ti, nextTypeId(c.idgen), ti.owner)
      result.n = literal
      c.intTypeCache[value.int] = result
  else:
    let ti = getSysType(c.graph, literal.info, tyInt)
    result = copyType(ti, nextTypeId(c.idgen), ti.owner)
    result.n = literal

proc setIntLitType*(c: PContext; result: PNode) =
  let i = result.intVal
  case c.config.target.intSize
  of 8: result.typ = getIntLitType(c, result)
  of 4:
    if i >= low(int32) and i <= high(int32):
      result.typ = getIntLitType(c, result)
    else:
      result.typ = getSysType(c.graph, result.info, tyInt64)
  of 2:
    if i >= low(int16) and i <= high(int16):
      result.typ = getIntLitType(c, result)
    elif i >= low(int32) and i <= high(int32):
      result.typ = getSysType(c.graph, result.info, tyInt32)
    else:
      result.typ = getSysType(c.graph, result.info, tyInt64)
  of 1:
    # 8 bit CPUs are insane ...
    if i >= low(int8) and i <= high(int8):
      result.typ = getIntLitType(c, result)
    elif i >= low(int16) and i <= high(int16):
      result.typ = getSysType(c.graph, result.info, tyInt16)
    elif i >= low(int32) and i <= high(int32):
      result.typ = getSysType(c.graph, result.info, tyInt32)
    else:
      result.typ = getSysType(c.graph, result.info, tyInt64)
  else:
    c.config.internalError(
      result.info, rintUnreachable, "invalid int size")

proc makeInstPair*(s: PSym, inst: PInstantiation): TInstantiationPair =
  result.genericSym = s
  result.inst = inst

proc filename*(c: PContext): string =
  # the module's filename
  return toFilename(c.config, FileIndex c.module.position)

proc scopeDepth*(c: PContext): int {.inline.} =
  result = if c.currentScope != nil: c.currentScope.depthLevel
           else: 0

proc getCurrOwner*(c: PContext): PSym =
  ## owner stack (used for initializing the
  ## owner field of syms)
  ## the documentation comment always gets
  ## assigned to the current owner
  result = c.graph.owners[^1]

proc pushOwner*(c: PContext; owner: PSym) =
  c.graph.owners.add(owner)

proc popOwner*(c: PContext) =
  c.config.internalAssert(c.graph.owners.len > 0, "popOwner")
  setLen(c.graph.owners, c.graph.owners.len - 1)

proc lastOptionEntry*(c: PContext): POptionEntry =
  result = c.optionStack[^1]

proc pushProcCon*(c: PContext, owner: PSym) {.inline.} =
  c.config.internalAssert(owner != nil, "owner is nil")
  var x: PProcCon
  new(x)
  x.owner = owner
  x.next = c.p
  c.p = x

proc popProcCon*(c: PContext) {.inline.} = c.p = c.p.next

proc put*(p: PProcCon; key, val: PSym) =
  if not p.mappingExists:
    initIdTable(p.mapping)
    p.mappingExists = true
  #echo "put into table ", key.info
  p.mapping.idTablePut(key, val)

proc get*(p: PProcCon; key: PSym): PSym =
  if not p.mappingExists: return nil
  result = PSym(p.mapping.idTableGet(key))

proc getGenSym*(c: PContext; s: PSym): PSym =
  if sfGenSym notin s.flags: return s
  var it = c.p
  while it != nil:
    result = get(it, s)
    if result != nil:
      #echo "got from table ", result.name.s, " ", result.info
      return result
    it = it.next
  result = s

proc considerGenSyms*(c: PContext; n: PNode) =
  if n == nil:
    discard "can happen for nkFormalParams/nkArgList"
  else:
    case n.kind
    of nkSym:
      let s = getGenSym(c, n.sym)
      if n.sym != s:
        n.sym = s
    of nkError:
      discard
    else:
      for i in 0..<n.safeLen:
        considerGenSyms(c, n[i])

proc newOptionEntry*(conf: ConfigRef): POptionEntry =
  new(result)
  result.options = conf.options
  result.defaultCC = ccNimCall
  result.dynlib = nil
  result.notes = conf.notes
  result.warningAsErrors = conf.warningAsErrors

proc pushOptionEntry*(c: PContext): POptionEntry =
  new(result)
  var prev = c.optionStack[^1]
  result.options = c.config.options
  result.defaultCC = prev.defaultCC
  result.dynlib = prev.dynlib
  result.notes = c.config.notes
  result.warningAsErrors = c.config.warningAsErrors
  result.features = c.features
  c.optionStack.add(result)

proc popOptionEntry*(c: PContext) =
  c.config.options = c.optionStack[^1].options
  c.config.notes = c.optionStack[^1].notes
  c.config.warningAsErrors = c.optionStack[^1].warningAsErrors
  c.features = c.optionStack[^1].features
  c.optionStack.setLen(c.optionStack.len - 1)

proc newContext*(graph: ModuleGraph; module: PSym): PContext =
  new(result)
  result.optionStack = @[newOptionEntry(graph.config)]
  result.libs = @[]
  result.module = module
  result.friendModules = @[module]
  result.converters = @[]
  result.patterns = @[]
  result.includedFiles = initIntSet()
  initStrTable(result.pureEnumFields)
  initStrTable(result.userPragmas)
  result.generics = @[]
  result.cache = graph.cache
  result.graph = graph
  initStrTable(result.signatures)
  result.features = graph.config.features
  if graph.config.symbolFiles != disabledSf:
    let id = module.position
    assert graph.packed[id].status in {undefined, outdated}
    graph.packed[id].status = storing
    graph.packed[id].module = module
    initEncoder graph, module

template packedRepr*(c): untyped = c.graph.packed[c.module.position].fromDisk
template encoder*(c): untyped = c.graph.encoders[c.module.position]

proc addIncludeFileDep*(c: PContext; f: FileIndex) =
  if c.config.symbolFiles != disabledSf:
    addIncludeFileDep(c.encoder, c.packedRepr, f)

proc addImportFileDep*(c: PContext; f: FileIndex) =
  if c.config.symbolFiles != disabledSf:
    addImportFileDep(c.encoder, c.packedRepr, f)

proc addPragmaComputation*(c: PContext; n: PNode) =
  if c.config.symbolFiles != disabledSf:
    addPragmaComputation(c.encoder, c.packedRepr, n)

proc inclSym(sq: var seq[PSym], s: PSym): bool =
  for i in 0..<sq.len:
    if sq[i].id == s.id: return false
  sq.add s
  result = true

proc addConverter*(c: PContext, conv: LazySym) =
  assert conv.sym != nil
  if inclSym(c.converters, conv.sym):
    add(c.graph.ifaces[c.module.position].converters, conv)

proc addConverterDef*(c: PContext, conv: LazySym) =
  addConverter(c, conv)
  if c.config.symbolFiles != disabledSf:
    addConverter(c.encoder, c.packedRepr, conv.sym)

proc addPureEnum*(c: PContext, e: LazySym) =
  assert e.sym != nil
  add(c.graph.ifaces[c.module.position].pureEnums, e)
  if c.config.symbolFiles != disabledSf:
    addPureEnum(c.encoder, c.packedRepr, e.sym)

proc addPattern*(c: PContext, p: LazySym) =
  assert p.sym != nil
  if inclSym(c.patterns, p.sym):
    add(c.graph.ifaces[c.module.position].patterns, p)
  if c.config.symbolFiles != disabledSf:
    addTrmacro(c.encoder, c.packedRepr, p.sym)

proc exportSym*(c: PContext; s: PSym) =
  strTableAdds(c.graph, c.module, s)
  if c.config.symbolFiles != disabledSf:
    addExported(c.encoder, c.packedRepr, s)

proc reexportSym*(c: PContext; s: PSym) =
  strTableAdds(c.graph, c.module, s)
  if c.config.symbolFiles != disabledSf:
    addReexport(c.encoder, c.packedRepr, s)

proc newLib*(kind: TLibKind): PLib =
  new(result)
  result.kind = kind          #initObjectSet(result.syms)

proc addToLib*(lib: PLib, sym: PSym) =
  #if sym.annex != nil and not isGenericRoutine(sym):
  #  LocalError(sym.info, errInvalidPragma)
  sym.annex = lib

proc newTypeS*(kind: TTypeKind, c: PContext): PType =
  result = newType(kind, nextTypeId(c.idgen), getCurrOwner(c))

proc makePtrType*(owner: PSym, baseType: PType; idgen: IdGenerator): PType =
  result = newType(tyPtr, nextTypeId(idgen), owner)
  addSonSkipIntLit(result, baseType, idgen)

proc makePtrType*(c: PContext, baseType: PType): PType =
  makePtrType(getCurrOwner(c), baseType, c.idgen)

proc makeTypeWithModifier*(c: PContext,
                           modifier: TTypeKind,
                           baseType: PType): PType =
  assert modifier in {tyVar, tyLent, tyPtr, tyRef, tyStatic, tyTypeDesc}

  if modifier in {tyVar, tyLent, tyTypeDesc} and baseType.kind == modifier:
    result = baseType
  else:
    result = newTypeS(modifier, c)
    addSonSkipIntLit(result, baseType, c.idgen)

proc makeVarType*(c: PContext, baseType: PType; kind = tyVar): PType =
  if baseType.kind == kind:
    result = baseType
  else:
    result = newTypeS(kind, c)
    addSonSkipIntLit(result, baseType, c.idgen)

proc makeVarType*(owner: PSym, baseType: PType; idgen: IdGenerator; kind = tyVar): PType =
  if baseType.kind == kind:
    result = baseType
  else:
    result = newType(kind, nextTypeId(idgen), owner)
    addSonSkipIntLit(result, baseType, idgen)

proc makeTypeDesc*(c: PContext, typ: PType): PType =
  if typ.kind == tyTypeDesc:
    result = typ
  else:
    result = newTypeS(tyTypeDesc, c)
    incl result.flags, tfCheckedForDestructor
    result.addSonSkipIntLit(typ, c.idgen)

proc makeTypeSymNode*(c: PContext, typ: PType, info: TLineInfo): PNode =
  let typedesc = newTypeS(tyTypeDesc, c)
  incl typedesc.flags, tfCheckedForDestructor
  internalAssert(c.config, typ != nil, "[FIXME]")
  typedesc.addSonSkipIntLit(typ, c.idgen)
  let sym = newSym(skType, c.cache.idAnon, nextSymId(c.idgen), getCurrOwner(c), info,
                   c.config.options).linkTo(typedesc)
  result = newSymNode(sym, info)

proc makeTypeFromExpr*(c: PContext, n: PNode): PType =
  result = newTypeS(tyFromExpr, c)
  assert n != nil
  result.n = n

proc newTypeWithSons*(owner: PSym, kind: TTypeKind, sons: seq[PType];
                      idgen: IdGenerator): PType =
  result = newType(kind, nextTypeId(idgen), owner)
  result.sons = sons

proc newTypeWithSons*(c: PContext, kind: TTypeKind,
                      sons: seq[PType]): PType =
  result = newType(kind, nextTypeId(c.idgen), getCurrOwner(c))
  result.sons = sons

proc makeStaticExpr*(c: PContext, n: PNode): PNode =
  result = newNodeI(nkStaticExpr, n.info)
  result.sons = @[n]
  result.typ = if n.typ != nil and n.typ.kind == tyStatic: n.typ
               else: newTypeWithSons(c, tyStatic, @[n.typ])

proc nMinusOne(c: PContext; n: PNode): PNode =
  result = newTreeI(nkCall, n.info, newSymNode(getSysMagic(c.graph, n.info, "pred", mPred)), n)

# Remember to fix the procs below this one when you make changes!
proc makeRangeWithStaticExpr*(c: PContext, n: PNode): PType =
  let intType = getSysType(c.graph, n.info, tyInt)
  result = newTypeS(tyRange, c)
  result.sons = @[intType]
  if n.typ != nil and n.typ.n == nil:
    result.flags.incl tfUnresolved
  result.n = newTreeI(nkRange, n.info, newIntTypeNode(0, intType),
    makeStaticExpr(c, nMinusOne(c, n)))

template rangeHasUnresolvedStatic*(t: PType): bool =
  tfUnresolved in t.flags

proc errorType*(c: PContext): PType =
  ## creates a type representing an error state
  result = newTypeS(tyError, c)
  result.flags.incl tfCheckedForDestructor

proc errorNode*(c: PContext, n: PNode): PNode =
  # xxx: convert to `nkError` instead of `nkEmpty`
  result = newNodeI(nkEmpty, n.info)
  result.typ = errorType(c)

proc fillTypeS*(dest: PType, kind: TTypeKind, c: PContext) =
  dest.kind = kind
  dest.owner = getCurrOwner(c)
  dest.size = - 1

proc makeRangeType*(c: PContext; first, last: BiggestInt;
                    info: TLineInfo; intType: PType = nil): PType =
  let intType = if intType != nil: intType else: getSysType(c.graph, info, tyInt)
  var n = newNodeI(nkRange, info)
  n.add newIntTypeNode(first, intType)
  n.add newIntTypeNode(last, intType)
  result = newTypeS(tyRange, c)
  result.n = n
  addSonSkipIntLit(result, intType, c.idgen) # basetype of range

proc markIndirect*(c: PContext, s: PSym) {.inline.} =
  if s.kind in {skProc, skFunc, skConverter, skMethod, skIterator}:
    incl(s.flags, sfAddrTaken)
    # XXX add to 'c' for global analysis

proc checkSonsLen*(n: PNode, length: int; conf: ConfigRef) =
  if n.len != length:
    conf.globalReport(n.info, reportAst(
      rsemIllformedAst, n,
      str = "Expected $1 elements, but found $2" % [$length, $n.len]))

proc checkMinSonsLen*(n: PNode, length: int; conf: ConfigRef) =
  if n.len < length:
    conf.globalReport(n.info, reportAst(
      rsemIllformedAst, n,
      str = "Expected at least $1 elements, but found $2" % [$length, $n.len]))

proc isTopLevel*(c: PContext): bool {.inline.} =
  result = c.currentScope.depthLevel <= 2

proc isTopLevelInsideDeclaration*(c: PContext, sym: PSym): bool {.inline.} =
  # for routeKinds the scope isn't closed yet:
  c.currentScope.depthLevel <= 2 + ord(sym.kind in routineKinds)

proc inCompileTimeOnlyContext*(c: PContext): bool =
  ## Returns whether the current analysis happens for code that can only run
  ## at compile-time
  c.p.inStaticContext > 0 or sfCompileTime in c.p.owner.flags

proc pushCaseContext*(c: PContext, caseNode: PNode) =
  c.p.caseContext.add((caseNode, 0))

proc popCaseContext*(c: PContext) =
  discard pop(c.p.caseContext)

proc setCaseContextIdx*(c: PContext, idx: int) =
  c.p.caseContext[^1].idx = idx

template addExport*(c: PContext; s: PSym) =
  ## convenience to export a symbol from the current module
  addExport(c.graph, c.module, s)

proc storeRodNode*(c: PContext, n: PNode) =
  if c.config.symbolFiles != disabledSf:
    toPackedNodeTopLevel(n, c.encoder, c.packedRepr)

proc addToGenericProcCache*(c: PContext; s: PSym; inst: PInstantiation) =
  c.graph.procInstCache.mgetOrPut(s.itemId, @[]).add LazyInstantiation(module: c.module.position, inst: inst)
  if c.config.symbolFiles != disabledSf:
    storeInstantiation(c.encoder, c.packedRepr, s, inst)

proc addToGenericCache*(c: PContext; s: PSym; inst: PType) =
  c.graph.typeInstCache.mgetOrPut(s.itemId, @[]).add LazyType(typ: inst)
  if c.config.symbolFiles != disabledSf:
    storeTypeInst(c.encoder, c.packedRepr, s, inst)

proc sealRodFile*(c: PContext) =
  if c.config.symbolFiles != disabledSf:
    if c.graph.vm != nil:
      for (m, n) in PCtx(c.graph.vm).vmstateDiff:
        if m == c.module:
          addPragmaComputation(c, n)
    c.idgen.sealed = true # no further additions are allowed

proc rememberExpansion*(c: PContext; info: TLineInfo; expandedSym: PSym) =
  ## Templates and macros are very special in Nim; these have
  ## inlining semantics so after semantic checking they leave no trace
  ## in the sem'checked AST. This is very bad for IDE-like tooling
  ## ("find all usages of this template" would not work). We need special
  ## logic to remember macro/template expansions. This is done here and
  ## delegated to the "rod" file mechanism.
  if c.config.symbolFiles != disabledSf:
    storeExpansion(c.encoder, c.packedRepr, info, expandedSym)

proc extractPragma(s: PSym): PNode =
  if s.kind in routineKinds:
    result = s.ast[pragmasPos]
  elif s.kind in {skType, skVar, skLet}:
    if s.ast != nil and s.ast.len > 0:
      if s.ast[0].kind == nkPragmaExpr and s.ast[0].len > 1:
        # s.ast = nkTypedef / nkPragmaExpr / [nkSym, nkPragma]
        result = s.ast[0][1]
  doAssert result == nil or result.kind in {nkPragma, nkEmpty}

proc warnAboutDeprecated(conf: ConfigRef; info: TLineInfo; s: PSym) =
  var pragmaNode: PNode
  pragmaNode =
    if s.kind == skEnumField:
      extractPragma(s.owner)
    else:
      extractPragma(s)

  if pragmaNode != nil:
    for it in pragmaNode:
      if whichPragma(it) == wDeprecated and it.safeLen == 2 and
          it[1].kind in {nkStrLit..nkTripleStrLit}:
        localReport(conf, info, reportSym(
          rsemDeprecated, s, str = it[1].strVal))
        return

  localReport(conf, info, reportSym(rsemDeprecated, s))

proc userError(conf: ConfigRef; info: TLineInfo; s: PSym) =
  let pragmaNode = extractPragma(s)
  if pragmaNode != nil:
    for it in pragmaNode:
      if whichPragma(it) == wError and it.safeLen == 2 and
          it[1].kind in {nkStrLit..nkTripleStrLit}:
        localReport(conf, info, reportSym(
          rsemUsageIsError, s, str = it[1].strVal))
        return

  localReport(conf, info, reportSym(rsemUsageIsError, s))

proc markOwnerModuleAsUsed*(c: PContext; s: PSym) =
  var module = s
  while module != nil and module.kind != skModule:
    module = module.owner
  if module != nil and module != c.module:
    var i = 0
    while i <= high(c.unusedImports):
      let candidate = c.unusedImports[i][0]
      if candidate == module or
         c.importModuleMap.getOrDefault(candidate.id, int.low) == module.id or
         c.exportIndirections.contains((candidate.id, s.id)):
        # mark it as used:
        c.unusedImports.del(i)
      else:
        inc i

proc markUsed*(c: PContext; info: TLineInfo; s: PSym) =
  let conf = c.config
  incl(s.flags, sfUsed)
  if s.kind == skEnumField and s.owner != nil:
    incl(s.owner.flags, sfUsed)
    if sfDeprecated in s.owner.flags:
      warnAboutDeprecated(conf, info, s)
  if {sfDeprecated, sfError} * s.flags != {}:
    if sfDeprecated in s.flags:
      if not (c.lastTLineInfo.line == info.line and
              c.lastTLineInfo.col == info.col):
        warnAboutDeprecated(conf, info, s)
        c.lastTLineInfo = info

    if sfError in s.flags: userError(conf, info, s)
  when defined(nimsuggest):
    if c.graph.onMarkUsed != nil:
      c.graph.onMarkUsed(c.graph, info, s, c.graph.usageSym, false)
  if {optStyleHint, optStyleError} * conf.globalOptions != {}:
    styleCheckUse(conf, info, s)
  markOwnerModuleAsUsed(c, s)
