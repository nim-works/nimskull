#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module is the new ast, it's an attempt to migrate the compiler towards
## a set of lowerings.
##
## Naming Conventions:
## * `legacy` related to bridging from old AST to the new one

from ".." / ast import TNodeKind, PNode, safeLen, `[]`, pairs

type
  ModuleAst* = object
    ## AST for a module
    id: ModuleId
    ast*: Ast         # xxx: exported for debugging, remove before merge
    tokens: TokenList

  Ast* = object
    ## AST of some source code fragment or possibly entire module
    nodes*: seq[AstNode] # xxx: exported for debugging, remove before merge
    extra*: AstExtra     # xxx: exported for debugging, remove before merge

  AstNode* = object
    ## xxx: turn this into struct-of-arrays
    ##
    ## An AST node in the most generous sense, it does two things:
    ## 1. overlay a tree structure upon sequentially stored nodes
    ## 2. stores indices for lookup into auxiliary information structures
    kind: AstNodeKind
      ## AST node this represents, mostly follows the written language grammar.
      ## Exceptions are made for data compactness, see: `AstNodeKind`
    token: TokenIndex
      ## `token` is used to lookup the corresponding `PNode` that was
      ## originally parsed.
      ## xxx: meant to be a temporary, replace with an index into a TokenList
      ##      representing the output of tokenizing source code
    left: AstLeft
      ## interpretation depends upon `kind`, typically this is an index for
      ## looking up the left child index in the list of `AstNode`.
    right: AstRight
      ## interpretation depends upon `kind`, typically this is an index for
      ## looking up the right child index in the list of `AstNode`.

  AstExtra* = seq[AstIndex]
    ## this sequence is used by `Ast` to lookup extra data that do not fit into
    ## `AstNode`'s `left` and `right` properties. Instead the `kind` tag on the
    ## `AstNode` will determine two things:
    ##   1. how to interpret `left` or `right` is an index into this sequence
    ##   2. how many nodes in this sequence to traverse
    ##
    ## For example a proc definition requires an index into the AST for the
    ## name, generic params, formal params, pragmas, and body. In this case
    ## we'd expect to read 5 AstIndex items in the corresponding order.
    ##
    ## Lastly, an AstIndex of `0`, as it travels backwards, will be treated as
    ## an empty/special value, determined by the `kind`. Given the previous
    ## example, a proc definition without any generic parameters might indicate
    ## a generic parameter position of 0.

  TokenList* = object
    ## list of tokens, typically for an entire module or fragment of code
    list: seq[Token]

  Token* = object
    ## xxx: should be a single token in a token list from tokenization.
    ##      instead it shims info between the old `ast.TNode` and the new AST.
    node: PNode

  AstNodeKind* {.pure.} = enum
    ## discriminant for different types of AST nodes, see `AstNode`
    ## Naming conventions:
    ## - names are composed of words categorizing them from general to specific
    ##    eg: `ankLitxxx`, the `Lit` is a category. This grouping means we can
    ##    use ranges of node kinds for easy comparisons
    ## - suffixes on nodes of `One`, `Two`, `N`, etc... indicate the number of
    ##    args that are accepted. This allows for simpler interpretation of
    ##    `left` and `right` fields in an AST node

    # the first kind, AKA `low` needs to be 0, as this is used to detect
    # uninitialized state -- see `isUninitialized`

    ankError,        ## ast error of some sort
    ankEmpty,        ## for optional parts of the AST, eg: inferred return type

    ankIdent,        ## identifier of some sort, excluding literals

    # Literals - Begin
    ankLitChar,      ## character literal `''`, `'a'`, etc
    
    ankLitInt,       ## signed int literal `1`, platform determines bits
    ankLitInt8,      ## signed 8-bit int literal `1'int8`
    ankLitInt16,     ## signed 16-bit int literal `1'int16`
    ankLitInt32,     ## signed 32-bit int literal `1'int32`
    ankLitInt64,     ## signed 64-bit int literal `1'int64`
    
    ankLitUInt,      ## unsigned int literal `1`, platform determines bits
    ankLitUInt8,     ## unsigned 8-bit int literal `1'uint8`
    ankLitUInt16,    ## unsigned 16-bit int literal `1'uint16`
    ankLitUInt32,    ## unsigned 32-bit int literal `1'uint32`
    ankLitUInt64,    ## unsigned 64-bit int literal `1'uint64`
    
    ankLitFloat,     ## signed float literal `1.0`, platform determines bits
    ankLitFloat32,   ## signed 32-bit float literal `1'float32`
    ankLitFloat64,   ## signed 64-bit float literal `1'float64`
    ankLitFloat128,  ## signed 128-bit float literal `1'float128`

    ankLitStr,       ## string literal
    ankLitRawStr,    ## raw string literal
    ankLitTripleStr, ## triple quoted string literal """foo"""

    ankLitNil,       ## `nil` literal
    # Literals - Finish

    # Calls - Begin

    # Calls - end
    ankCall,         ## call with parens and zero or more args `sum(x, y)`
    ankCallCmd,      ## command without parens and one or more args `echo 1`
    ankCallRawStr,   ## call with a raw or triplequoted string literal `r"foo"`
                     ## or `x"""foobar"""`
    ankCallInfix,    ## infix call, eg: `a + b`
    ankCallPrefix,   ## prefix call `!p`
    ankCallPostfix,  ## only postfix operation is on symbols for exports eg:
                     ## `type Point* = ...`
    # Calls - end

    # Arguments, Ident Defintions, & Parameters - Begin
    ankExprEqExpr,   ## name arg with equals: `arg = val`

    ankExprColonExpr,## name arg with colon: `arg: val`, pragma calls, etc

    ankIdentDefs,    ## identifiers, type, and default value, used in params,
                     ## const, let, var declarations,
                     ## eg: `a, b: typeDesc = expr`

    ankUnpack,       ## unpack a tuple, in const, for, let, and var contexts:
                     ## `let (a,) = expr`, unpack a 1-tuple, const/for/let/var
                     ## `const (a, b) = expr`, unpack a pair, const/for/let/var
                     ## `for a, b, c in expr:`, unpack an n > 2 tuple
    # Arguments, Ident Defintions, & Parameters - Finish

    # Braces, Brackets, Parentheses, and Constructors - Begin
    ankPar,          ## parentheses `()` with zero or more args, eg:
                     ## - empty parens `()`, maybe empty tuple constructor
                     ## - one child parens `(1)`
                     ## - two child parens `(1, b)`
                     ## - etc...

    ankBracket,      ## bracket literal, `[]`, typically an array literal, eg:
                     ## - array construct `[]`, zero args
                     ## - array construct `[1]`, one arg
                     ## - array construct `[1, 2]`, two args

    ankCurly,        ## braces `{}` with zero or more items often a set:
                     ## - `{}` often a set, empty or zero items
                     ## - `{ankParN}`, often a set, one item
                     ## - `{ankParN, ankCallN}`, often a set, two items
                     ## - `{ankParN, ankCallN, ankCurlyN}`, often a set
                     ## - etc...

    ankTupleConstr,  ## tuple constructor with zero or more args, eg:
                     ## - `()`
                     ## - `(1,)` single arg tuple, note trailing comma
                     ## - `(1, "foo")` two arg tuple, AKA pair
                     ## - etc...

    ankObjConstr,    ## object constructor, eg:
                     ## - null-ary object constructor: `T()`
                     ## - unary object constructor: `T(a: 1)`
                     ## - binary object constructor: `T(a: 1, b: 2)`
                     ## - n-ary object constructor: `T(a: 1, b: 2, c: 3)`

    ankTblConstr,    ## table constructor, very similar to `ankCurly`, has at
                     ## least one colon separated argument, empty is a
                     ## `ankCurly`, eg:
                     ## - null-ary table constructor: `{:}`
                     ## - unary table constructor: `{a: 1}`
                     ## - binary table constructor: `{a: 1, b: 2}`
                     ## - etc...

    ankBracketExpr,  ## bracket expression, like array indexing, eg:
                     ## - `a[]`, zero args
                     ## - `a[i]`, one arg
                     ## - `a[i, j]`, two args
                     ## - etc...

    ankCurlyExpr,    ## braces used somewhat like bracket indexing `a{}` with
                     ## zero or more items, eg:
                     ## - `a{i}`, one arg
                     ## - `a{i, j}`, two args
                     ## - etc...
    # Braces, Brackets, Parentheses, and Constructors - Finish

    # Pragma - Begin
    ankPragmaExpr,   ## pragma expression `a {.i.}`, eg:
                     ## - `a {.}`, zero args
                     ## - `a {.i.}`, one arg
                     ## - `a {.i, j.}`, two args
                     ## - etc...
                     ## xxx: `a {..}` is not legal, this should be possible to
                     ##      disambiguate
    ankPragmaStmt,   ## `{.emit: """Foo""".}`
    ankPragmaBlock,  ## `{.cast(gcsafe).}: ...`
    # Pragma - Finish

    # names and field access, dot expr, stropped - Begin
    ankDotExpr,      ## `a.b`
    ankAccQuoted,    ## '`a`' accent quoted or stropped ident
    # names and field access, dot expr, stropped - Finish

    # control flow - branch expressions and statements - begin
    ankIf,           ## an `if` statement or expression
    ankElif,         ## an `elif` statement or expression
    ankElse,         ## an `else` statement or expression

    ankCase,         ## a `case` statement or expression
    ankOf,           ## an `of` branch in a case statement or expression

    ankWhen,         ## a `when` statement or expression
    # control flow - branch expressions and statements - finish

    # control flow - blocks & loops - begin
    ankFor,          ## for loop
    ankWhile,        ## while loop

    ankBlock,        ## `block: ...` expression or statement
    ankBlockNamed,   ## `block foo: ...` expression or statement
    # control flow - blocks & loops - finish

    # control flow - pass control - begin
    ankDiscard,      ## `discard` statement
    ankDiscardSome,  ## `discard "thing"` discard an expression or statement
    ankContinue,     ## `continue` the nearest loop

    ankBreak,        ## `break` statement to the nearest loop
    ankBreakLabel,   ## break with a expression/label `break foo: ...`

    ankReturn,       ## `return` without any expression
    ankReturnExpr,   ## `return a` with an expression

    ankReRaise,      ## `raise`, re-raise an exception
    ankRaise,        ## `raise newException(IOError, "e")`, raise an exception

    ankYield,        ## `yield a`, yield statement
    # control flow - pass control - finish

    # exceptions - begin
    ankTry,         ## `try` expression or statement
    ankExceptAny,   ## follows try `except:`, no type specified
    ankExcept,      ## follows try `except IOError:` or `expect IOError as e:`
    ankFinally,     ## follows try or try/except, expression or statement

    ankDefer,       ## `defer stmt` statement
    # exceptions - finish

    # lambdas and anonymous procs - begin
    ankLambda,       ## lambda expression, `proc(): int = 42`
    ankDo,           ## do expression, lambda block as trailing proc param
    # lambdas and anonymous procs - finish

    # bind, mixin - begin
    ankBind,         ## `bind a` statement, only in metaprog/generic context
    ankMixin,        ## `mixin b` statement, only in metaprog/generic context
    # bind, mixin - finish

    # cast, static - begin
    ankCast,         ## type cast, `cast Foo bar`
    ankStatic,       ## static statement or expression, `static 10`
    # cast, static - finish

    # assignment - begin
    ankAssign,       ## assignment `a = b`
    # assignment - finish

    # params - begin
    ankParamsGeneric,## generic params `[T]`, eg:
                     ## - null-ary generic params, etc `[]`
                     ## - unary generic param, etc `[T]`
                     ## - binary generic params, etc `[T, R]`
                     ## - etc...

    ankParamsFormal, ## formal params, `(i: int): int`, eg:
                     ## - no return or args `() =`
                     ## - return only, no args `(): int`
                     ## - return & one arg, `(i: int): int`
                     ## - `(i: int, j: string)` as the return can be empty
                     ## - etc...
    # params - finish

    # statements - begin
    ankStmtList,     ## a list of statements, such as a proc body
    ankStmtListExpr, ## a list of statements ending in an expression, useful
                     ## in many places such as the final expression as return
    # statements - finish

    # import, include, & export - begin
    ankImport,       ## `import` statement
    ankImportExcept, ## `import` statement with an `except`
    ankImportAs,     ## `a as b` in an import statement

    ankImportFrom,   ## `from foo_module import bar`

    ankInclude,      ## `include` statement

    ankExport,       ## `export` statment
    ankExportExcept, ## `export` statment with an `except`
    # import, include, & export - finish

    # definitions - begin
    # definitions - variable sections - begin
    ankSecConst,     ## const section with n identdefs, eg:
                     ## - `const a = 1`, has one identdefs
                     ## - `const: a = 1, b = 2`, has two identdefs
                     ## - etc...

    ankSecLet,       ## let section with one or more identdefs, eg:
                     ## - `let a = 1`, has one identdefs
                     ## - `let: a = 1, b = 2`, has two identdefs
                     ## - etc...
    
    ankSecVar,       ## var section with one or more identdefs, eg:
                     ## - `var a = 1`, has one identdefs
                     ## - `var: a = 1, b = 2`, has two identdefs
                     ## - etc...
    # definitions - variable sections - finish

    # definitions - routine - begin
    ankDefProc,      ## procedure definition `proc f() = ...`
    ankDefFunc,      ## function definition `func f(): int = ...`
    ankDefMethod,    ## method  definition `method m(i: int) = ...`
    ankDefConverter, ## converter definition `converter c(i: int): uint = ...`
    ankDefIterator,  ## iterator definition `iterator i(a: string): int = ...`
    ankDefMacro,     ## macro definition `macro m(a: string): untyped = ...`
    ankDefTemplate,  ## template definition `template t() = ...`
    # definitions - routine - end

    # definitions - type - begin
    ankSecType,      ## `type ...` type section either as a single line or
                     ## with indented declarations underneath
    ankDefType,      ## `type Foo = int` the `Foo = int` part is a type def,
                     ## an analog to `ankIdentDefs`

    ankTyEnum,       ## `enum ...body...` or the special type `enum`
    ankDefEnumField, ## `enum Foo, Bar`, `Foo` & `Bar` are `ankTyEnumField`s

    ankTyObject,     ## `Foo = object ...` body or special type `object`
    ankTyTuple,      ## tuple body
    ankTyProc,       ##  in a type `proc (): int` or special type `proc`
    ankTyIterator,   ## `iterator` when used as part of a type param

    ankTyRecList,    ## list of object parts
    ankTyRecCase,    ## case section of object
    ankTyRecWhen,    ## when section of object

    ankTyConst,      ## `const T`
    ankTyRef,        ## `ref T`
    ankTyVar,        ## `var T`
    ankTyPtr,        ## `ptr T`
    ankTyStatic,     ## `static T`
    ankTyDistinct,   ## `distinct T`
    ankTyMutable,    ## `mutable T` when used for out params?
                     ## xxx: this is unlikely to be fully implemented
    
    ankTyClsTuple,    ## `tuple` type class
    ankTyClsUser,     ## user-defined type class

    ankInherit,      ## `Foo = object of Bar`, the `of Bar` is inherited

    ankArgList,      ## argument list to a type class parameter

    ankDefWith,      ## distinct with `foo`
    ankDefWithout,   ## distinct without `foo`
    # definitions - type - finish

    # definitions - finish

    # misc - begin
    ankAsm,
    ankComment,

    ankUsing,        ## allows consuming common parameters from proc defs
                     ## xxx: this should be removed
    # misc - finish

  TokenIndex* = distinct int32
    ## used to point to a token from the token list
  AstIndex* = distinct uint32
    ## used to point to additional information for an AST node
  AstLeft* = distinct uint32
    ## might be an `AstIndex`, an int value, or interpretted some other way
    ## based on the `AstNode.kind`
  AstRight* = distinct uint32
    ## might be an `AstIndex`, an int value, or interpretted some other way
    ## based on the `AstNode.kind`
  ModuleId* = int32
    ## id for this module within a project compilation
    ## XXX: refactor along with `ModuleAst` as this is mostly to shim with the
    ##      existing compiler instead of properly handling module vs module in
    ##      a package vs during a specific compilation.

const assumedAstLen: Natural = 100

proc initAst(hintAstLen = assumedAstLen): Ast =
  ## initialize an `Ast`, pre-allocate storage based on the `hintAstLen`
  Ast(nodes: newSeqOfCap[AstNode](hintAstLen))

proc `[]`(a: var Ast; i: AstIndex): var AstNode =
  ## get the `AstNode` corresponding to the `AstIndex` `i`.
  # since `i` is discriminated by type, we know what to index against, fun!
  a.nodes[i.uint32]

proc initTokenList(hintListLen = assumedAstLen): TokenList =
  ## initialize a `TokenList`, pre-allocate storage based on the 'hintListLen`
  TokenList(list: newSeqOfCap[Token](hintListLen))

proc initModuleAst*(id: ModuleId, hintAstLen = assumedAstLen): ModuleAst =
  ## initialize a ModuleAst, pre-allocate storage based on the `hintAstLen` to
  ## excessive allocations/moves.
  ModuleAst(
    id:     id,
    ast:    initAst(hintAstLen),
    tokens: initTokenList(hintAstLen)
  )

proc legacyInitModuleAst*(id: int): ModuleAst =
  ## creates `ModuleAst` with the legacy `id` of the module, in order to keep
  ## it in sync with the legacy `ModuleGraph`.
  initModuleAst(ModuleId id)

proc legacyAdd(tokens: var TokenList, n: PNode): TokenIndex =
  ## add this `PNode` from the parser to the list of tokens (yess very weird)
  ## and return the `TokenIndex` corresponding to the fresh token.
  tokens.list.add Token(node: n)
  result = TokenIndex(tokens.list.len - 1)

func legacyNodeToAstKind(n: PNode): AstNodeKind =
  let
    kind = n.kind
    childCount = n.safeLen
  
  result = case kind
    of nkError:        ankError
    of nkEmpty:        ankEmpty

    of nkIdent:        ankIdent

    of nkCharLit:      ankLitChar

    of nkIntLit:       ankLitInt
    of nkInt8Lit:      ankLitInt8
    of nkInt16Lit:     ankLitInt16
    of nkInt32Lit:     ankLitInt32
    of nkInt64Lit:     ankLitInt64

    of nkUIntLit:      ankLitUInt
    of nkUInt8Lit:     ankLitUInt8
    of nkUInt16Lit:    ankLitUInt16
    of nkUInt32Lit:    ankLitUInt32
    of nkUInt64Lit:    ankLitUInt64

    of nkFloatLit:     ankLitFloat
    of nkFloat32Lit:   ankLitFloat32
    of nkFloat64Lit:   ankLitFloat64
    of nkFloat128Lit:  ankLitFloat128

    of nkStrLit:       ankLitStr
    of nkRStrLit:      ankLitRawStr
    of nkTripleStrLit: ankLitTripleStr

    of nkNilLit:       ankLitNil

    of nkCall:         ankCall
    of nkCommand:      ankCallCmd
    of nkCallStrLit:   ankCallRawStr
    of nkInfix:        ankCallInfix
    of nkPrefix:       ankCallPrefix
    of nkPostfix:      ankCallPostfix

    of nkExprEqExpr:   ankExprEqExpr
    of nkExprColonExpr:ankExprColonExpr
    of nkIdentDefs, nkConstDef:
      # not much point differentiating between the two right now
      ankIdentDefs
    of nkVarTuple:     ankUnpack
    of nkPar:          ankPar
    of nkBracket:      ankBracket
    of nkCurly:        ankCurly
    of nkTupleConstr:  ankTupleConstr
    of nkObjConstr:    ankObjConstr
    of nkTableConstr:  ankTblConstr
    of nkBracketExpr:  ankBracketExpr
    of nkCurlyExpr:    ankCurlyExpr

    of nkPragmaExpr:   ankPragmaExpr
    of nkPragma:       ankPragmaStmt
    of nkPragmaBlock:  ankPragmaBlock

    of nkDotExpr:      ankDotExpr
    of nkAccQuoted:    ankAccQuoted

    of nkIfExpr, nkIfStmt:       ankIf
    of nkElifBranch, nkElifExpr: ankElif
    of nkElse, nkElseExpr:       ankElse

    of nkCaseStmt:     ankCase
    of nkOfBranch:     ankOf

    of nkWhenStmt:     ankWhen

    of nkForStmt:      ankFor
    of nkWhileStmt:    ankWhile

    of nkBlockExpr, nkBlockStmt:
      let
        namePos = 0
        hasName = n[namePos].kind != nkEmpty
      if hasName: ankBlockNamed else: ankBlock
    
    of nkDiscardStmt:
      if childCount > 0: ankDiscardSome else: ankDiscard
    
    of nkContinueStmt: ankContinue

    of nkBreakStmt:
      if childCount > 0: ankBreakLabel else: ankBreak
    
    of nkReturnStmt:
      if childCount > 0: ankReturnExpr else: ankReturn
    
    of nkRaiseStmt:
      # no child means just a `raise`, which raises the current exception
      if childCount == 0: ankReRaise else: ankRaise
    
    of nkYieldStmt:    ankYield

    of nkTryStmt:      ankTry
    of nkExceptBranch:
      if childCount > 0: ankExcept else: ankExceptAny
    of nkFinally:      ankFinally

    of nkDefer:        ankDefer

    of nkLambda:       ankLambda
    of nkDo:           ankDo

    of nkBind, nkBindStmt: ankBind
    of nkMixinStmt:        ankMixin

    of nkCast:         ankCast
    of nkStaticStmt:   ankStatic

    of nkAsgn:         ankAssign

    of nkGenericParams:ankParamsGeneric
    of nkFormalParams: ankParamsFormal

    of nkStmtList:     ankStmtList
    of nkStmtListExpr: ankStmtListExpr

    of nkImportStmt:      ankImport
    of nkImportExceptStmt:ankImportExcept
    of nkImportAs:        ankImportAs
    of nkFromStmt:        ankImportFrom

    of nkIncludeStmt:     ankInclude
    
    of nkExportStmt:      ankExport
    of nkExportExceptStmt:ankExportExcept

    of nkConstSection:    ankSecConst
    of nkLetSection:      ankSecLet
    of nkVarSection:      ankSecVar

    of nkProcDef:         ankDefProc
    of nkFuncDef:         ankDefFunc
    of nkMethodDef:       ankDefMethod
    of nkConverterDef:    ankDefConverter
    of nkIteratorDef:     ankDefIterator
    of nkMacroDef:        ankDefMacro
    of nkTemplateDef:     ankDefTemplate

    of nkTypeSection:     ankSecType
    of nkTypeDef:         ankDefType

    of nkEnumTy:          ankTyEnum
    of nkEnumFieldDef:    ankDefEnumField

    of nkObjectTy:        ankTyObject
    of nkTupleTy:         ankTyTuple
    of nkProcTy:          ankTyProc
    of nkIteratorTy:      ankTyIterator

    of nkRecList:         ankTyRecList
    of nkRecCase:         ankTyRecCase
    of nkRecWhen:         ankTyRecWhen

    of nkConstTy:         ankTyConst
    of nkRefTy:           ankTyRef
    of nkVarTy:           ankTyVar
    of nkPtrTy:           ankTyPtr
    of nkStaticTy:        ankTyStatic
    of nkDistinctTy:      ankTyDistinct
    of nkMutableTy:       ankTyMutable

    of nkTupleClassTy:    ankTyClsTuple
    of nkTypeClassTy:     ankTyClsUser

    of nkOfInherit:       ankInherit

    of nkArgList:         ankArgList

    of nkWith:            ankDefWith
    of nkWithout:         ankDefWithout

    of nkAsmStmt:         ankAsm
    of nkCommentStmt:     ankComment

    of nkUsingStmt:       ankUsing
    else: raise newException(ValueError, "No mapping for: " & $kind)

const
  emptyAstLeft = AstLeft 0
    ## empty based on zero initialization of memory
  emptyAstRight = AstRight 0
    ## empty based on zero initialization of memory
  unknownAstLeft = emptyAstLeft
    ## empty and unkonwn are ambiguous, aliased to communicate intent
  unknownAstRight = emptyAstRight
    ## empty and unkonwn are ambiguous, aliased to communicate intent

func getNextAstIndex(a: Ast): AstIndex =
  ## gets the `AstIndex` that would be generated if a node was inserted
  AstIndex a.nodes.len

proc `$`*(i: AstLeft): string {.borrow.}
proc `$`*(i: AstRight): string {.borrow.}
proc `$`*(i: AstIndex): string {.borrow.}
proc `$`*(i: TokenIndex): string {.borrow.}
proc `==`(a, b: TokenIndex): bool {.borrow.}
proc `==`(a, b: AstLeft): bool {.borrow.}
proc `==`(a, b: AstRight): bool {.borrow.}

func isUninitialized(nodes: seq[AstNode]; idx: AstIndex): bool =
  ## whether the given position has an existing AstNode or is it set to zero
  ## values as if freshly zero'd out memory.
  doAssert idx.uint32 < nodes.len.uint32, "idx is out of range: " & $idx
  let
    node = nodes[idx.uint32]
    kind = node.kind == AstNodeKind.low # `low` will be 0, default init
    token = node.token == 0.TokenIndex
    left = node.left == unknownAstLeft
    right = node.right == unknownAstRight

  result = kind and token and left and right

proc reserveAstNode(
    m: var ModuleAst;
    kind: AstNodeKind,
    token: TokenIndex
  ): AstIndex =
  ## append an ast node based on `kind` and `token`, with `left` and `right` to
  ## be updated later, as they're often unknown until further processing and
  ## addition of ast nodes.
  
  m.ast.nodes.add AstNode(
    kind: kind,
    token: token,
    left: emptyAstLeft,
    right: emptyAstRight
  )

  result = AstIndex m.ast.nodes.len - 1

type
  NodeProcessingKind {.pure.} = enum
    ## xxx: the name leaves much to be desired -- to be renamed.
    ## The intention is to indicate further processing of child nodes based
    ## upon the kind. This further processing usually involves extra data, see:
    ## `Ast.extra` and `AstExtra`
    npkNoExtraData,
    npkIndexAllButFirstChild

proc legacyAppendPNode*(m: var ModuleAst; n: PNode): AstIndex =
  ## take `n` the `PNode`, from parsing, and append it to `m` the `ModuleAst`.
  ## tracks the `AstIndex` of various nodes being appended, this allows to have
  ## a depth first construction of the AST -- when traversed linearly.

  let
    kind = legacyNodeToAstKind(n)
    token = m.tokens.legacyAdd(n)
    nodeIdx = reserveAstNode(m, kind, token)
    leftAstIdx = AstLeft m.ast.getNextAstIndex()
    (left, right, extraProcessing) = case kind
      of ankStmtList: (leftAstIdx, emptyAstRight, npkNoExtraData)
      of ankCallCmd:
        # Q: Why don't we store the callee's index?
        # A: command node is immediately followed by the callee ast node, we
        #    assume the next idx to be left and don't need to encode it, as
        #    it's implied by the `kind`
        let
          extraDataStart = m.ast.extra.len # where we start insert indices
          childCount = n.safeLen           # callee and then one or more args
          argsCount = childCount - 1       # total ast indices in extra data

        (AstLeft extraDataStart, AstRight argsCount, npkIndexAllButFirstChild)
      else:
        (emptyAstLeft, emptyAstRight, npkNoExtraData)

  m.ast[nodeIdx].left = left
  m.ast[nodeIdx].right = right

  for i, child in n.pairs:
    let
      firstChild = i == 0
      idx = m.legacyAppendPNode(child) # append the node

    # if we have to do extra process like remember extra data, do that here
    case extraProcessing
    of npkNoExtraData:
      # we already appended it, ignore everything else
      discard
    of npkIndexAllButFirstChild:
      # this is for cases like `ankCallCmd` where the first child is
      # immediately following and doesn't need to be indexed, but the args are
      # varying distances apart and indexed as extra data.
      if not firstChild:
        m.ast.extra.add idx

  result = nodeIdx


