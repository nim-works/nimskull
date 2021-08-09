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

from ".." / ast import PNode, safeLen

type
  ModuleAst* = object
    ## AST for an module
    id: ModuleId
    ast: Ast
    tokens: TokenList

  Ast* = object
    ## AST of some source code fragment or possibly entire module
    nodes: seq[AstNode]

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
    
    ankError,        ## ast error of some sort
    ankEmpty,        ## for optional parts of the AST, eg: inferred return type

    ankIdent,        ## identifier of some sort, excluding literals

    # Literals - Begin
    ankLitChar,      ## character literal `''`, `'a'`, etc
    
    # Literals - Int
    ankLitInt,       ## signed int literal `1`, platform determines bits
    ankLitInt8,      ## signed 8-bit int literal `1'int8`
    ankLitInt16,     ## signed 16-bit int literal `1'int16`
    ankLitInt32,     ## signed 32-bit int literal `1'int32`
    ankLitInt64,     ## signed 64-bit int literal `1'int64`
    
    # Literals - Unsight Int
    ankLitUInt,      ## unsigned int literal `1`, platform determines bits
    ankLitUInt8,     ## unsigned 8-bit int literal `1'uint8`
    ankLitUInt16,    ## unsigned 16-bit int literal `1'uint16`
    ankLitUInt32,    ## unsigned 32-bit int literal `1'uint32`
    ankLitUInt64,    ## unsigned 64-bit int literal `1'uint64`
    
    # Literals - Float
    ankLitFloat,     ## signed float literal `1.0`, platform determines bits
    ankLitFloat32,   ## signed 32-bit float literal `1'float32`
    ankLitFloat64,   ## signed 64-bit float literal `1'float64`
    ankLitFloat128,  ## signed 128-bit float literal `1'float128`

    # Literals - String
    ankLitStr,       ## string literal
    ankLitRawStr,    ## raw string literal
    ankLitTripleStr, ## triple quoted string literal """foo"""

    # Literals - Misc
    ankLitNil,       ## `nil` literal
    # Literals - Finish

    # Calls - Begin

    # Calls - Command
    ankCallCmdOne,   ## call without parens and one arg `echo foo`
    ankCallCmdTwo,   ## call without parens and two args `echo foo, bar`
    ankCallCmdN,     ## call without parens and 3+ args `echo 1, 2, 3`

    # Calls - Call
    ankCallZero,     ## call with parens and no args `rand()`
    ankCallOne,      ## call with parens and one arg `sqrt(x)`
    ankCallTwo,      ## call with parens and two args `+(x, y)`
    ankCallN,        ## call with parens and 3+ args `sum(x, y, z)`

    # Calls - Call String Literals
    ankCallRawStr,   ## call with a raw or triplequoted string literal `r"foo"`
                     ## or `x"""foobar"""`

    # Calls - Pre/Post/In-fix
    ankCallInfix,    ## call like `a + b`
    ankCallPrefix,   ## call like `!p`
    # ankCallPostfix,  ## only postfix operation is on symbols for exports eg:
    #                  ## `type Point* = ...`
    #                  ## xxx: bring this back if postfix calls are required
    # Calls - Finish

    # Arguments, Ident Defintions, & Parameters - Begin

    ankExprEqExpr,   ## name arg with equals: `arg = val`
    ankExprColonExpr,## name arg with colon: `arg: val`, pragma calls, etc
    ankIdentDefs,    ## identifiers, type, and default value, used in params,
                     ## const, let, var declarations,
                     ## eg: `a, b: typeDesc = expr`
    ankUnpackOne,    ## `let (a,) = expr`, unpack a 1-tuple, const/for/let/var
    ankUnpackTwo,    ## `const (a, b) = expr`, unpack a pair, const/for/let/var
    ankUnpackN,      ## `for a, b, c in expr:`, unpack an n > 2 tuple    

    # Arguments, Ident Defintions, & Parameters - Finish

    # Braces, Brackets, Parentheses, and Constructors - Begin
    ankParZero,      ## empty parens `()`, maybe tuple constructor
    ankParOne,       ## one child parens `(1)`, maybe tuple constructor
    ankParTwo,       ## two child parens `(1, b)`, maybe tuple constructor
    ankParN,         ## 3+ child parens `(1, b, 'c'), maybe tuple constructor

    ankObjConstrZero,## null-ary object constructor: `T()`
    ankObjConstrOne, ## unary object constructor: `T(a: 1)`
    ankObjConstrTwo, ## binary object constructor: `T(a: 1, b: 2)`
    ankObjConstrN,   ## n-ary, n > 2, object constructor: `T(a: 1, b: 2, c: 3)`

    ankCurlyZero,    ## `{}` often a set, empty or zero items
    ankCurlyOne,     ## `{ankParN}`, often a set, one item
    ankCurlyTwo,     ## `{ankParN, ankCallN}`, often a set, two items
    ankCurlyN,       ## `{ankParN, ankCallN, ankCurlyN}`, often a set, 3+ items

    ankCurlyExprZero,## expression `a{}`, zero args
    ankCurlyExprOne, ## expression `a{i}`, one arg
    ankCurlyExprTwo, ## expression `a{i, j}`, two args
    ankCurlyExprN,   ## expression `a{i, j, k}`, 3+ args

    # there is no `ankTblConstrZero` because it is the same as `ankCurlyZero`
    ankTblConstrZero,## null-ary table constructor: `{:}`
    ankTblConstrOne, ## unary table constructor: `{a: 1}`
    ankTblConstrTwo, ## binary table constructor: `{a: 1, b: 2}`
    ankTblConstrN,   ## n-ary, n > 2, table constructor: `{a: 1, b: 2, c: 3}`

    ankBracketZero,  ## eg: array construct `[]`, zero args
    ankBracketOne,   ## eg: array construct `[1]`, one arg
    ankBracketTwo,   ## eg: array construct `[1, 2]`, two args
    ankBracketN,     ## eg: array construct `[1, 2, 3]`, 3+ args

    ankBracketExprZero,## expression `a[]`, zero args
    ankBracketExprOne, ## expression `a[i]`, one arg
    ankBracketExprTwo, ## expression `a[i, j]`, two args
    ankBracketExprN,   ## expression `a[i, j, k]`, 3+ args

    ankPragmaExprZero,## expression `a {.}`, zero args
                      ## xxx: `a {..}` is not legal, this is like a lexing bug
    ankPragmaExprOne,## expression `a {.i.}`, one arg
    ankPragmaExprTwo,## expression `a {.i, j.}`, two args
    ankPragmaExprN,  ## expression `a {.i, j, k.}`, 3+ args
    # Braces, Brackets, Parentheses, and Constructors - Finish

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
    ankParamsGenericZero,## null-ary generic params, etc `[]`
    ankParamsGenericOne, ## unary generic param, etc `[T]`
    ankParamsGenericTwo, ## binary generic params, etc `[T, R]`
    ankParamsGenericN,   ## n-ary, n is 3+, generic params, etc `[T, R, S]`

    ankParamsFormalZero, ## formal params, no return or args `() =`
    ankParamsFormalOne,  ## formal params, return only, no args `(): int`
    ankParamsFormalTwo,  ## formal params, return & one arg, `(i: int): int` or
                         ## `(i: int)` as the return can be empty
    ankParamsFormalN,    ## formal params, `(i: int, j: int): int` or
                         ## `(i: int, j: int)` as the return can be empty
    # params - finish

    # import - begin
    ankImport,       ## `import` statement
    ankImportAs,     ## `a as b` in an import statement
    # import - finish

    # definitions - begin
    # definitions - variables - begin
    ankDefConstOne,  ## `const a = 1`, has one identdefs
    ankDefConstTwo,  ## `const: a = 1, b = 2`, has two identdefs
    ankDefConstN,    ## const section with n identdefs, where n is 3+

    ankDefLetOne,    ## `let a = 1`, has one identdefs
    ankDefLetTwo,    ## `let: a = 1, b = 2`, has two identdefs
    ankDefLetN,      ## let section with n identdefs, where n > 2
    
    ankDefVarOne,    ## `var a = 1`, has one identdefs
    ankDefVarTwo,    ## `var: a = 1, b = 2`, has two identdefs
    ankDefVarN,      ## var section with n identdefs, where n > 2
    # definitions - variables - finish

    # definitions - routine - begin
    ankDefProc,      ## procedure definition `proc f() = ...`
    ankDefFunc,      ## function definition `func f(): int = ...`
    ankDefMethod,    ## method  definition `method m(i: int) = ...`
    ankDefConverter, ## converter definition `converter c(i: int): uint = ...`
    ankDefIterator,  ## iterator definition `iterator i(a: string): int = ...`
    ankDefMacro,     ## macro definition `macro m(a: string): untyped = ...`
    ankDefTemplate,  ## template definition `template t() = ...`
    # definitions - routine - end

    # definitions - finish

    # definitions - 

    # misc - begin
    ankAsm,
    # misc - finish

    # xxx: keep adding the rest of the nodes

    nkArgList,
    nkCommentStmt,
    nkConstDef,
    nkConstSection,
    nkDistinctTy,
    nkEnumFieldDef,
    nkEnumTy,
    nkExportStmt,
    nkFromStmt,
    nkIncludeStmt,
    nkIteratorTy,
    nkLetSection,
    nkMutableTy,
    nkObjectTy,
    nkOfInherit,
    nkPragma,
    nkPragmaBlock,
    nkPragmaExpr,
    nkProcTy,
    nkPtrTy,
    nkRecCase,
    nkRecList,
    nkRecWhen,
    nkRefTy,
    nkStaticTy,
    nkStmtList,
    nkStmtListExpr,
    nkTryStmt,
    nkTupleClassTy,
    nkTupleConstr,
    nkTupleTy,
    nkTypeClassTy,
    nkTypeDef,
    nkTypeOfExpr,
    nkTypeSection,
    nkUsingStmt,
    nkVarSection,
    nkVarTy,
    nkWith,
    nkWithout,
    # nkBlockStmt,
    # nkBreakStmt,
    # nkContinueStmt,
    # nkDefer,
    # nkDiscardStmt,
    # nkExceptBranch,
    # nkIfExpr,
    # nkIfStmt,
    # nkImportAs,
    # nkImportStmt,
    # nkProcDef,
    # nkOfBranch,
    # nkRaiseStmt,
    # nkReturnStmt,
    # nkTemplateDef,
    # nkWhileStmt,
    # nkYieldStmt

    # nkAccQuoted,
    # nkAsgn,
    # nkAsmStmt,
    # nkBind,
    # nkBindStmt,
    # nkBracket,
    # nkBracketExpr,
    # nkCall,
    # nkCallStrLit,
    # nkCaseStmt,
    # nkCast,
    # nkCharLit,
    # nkCommand,
    # nkConverterDef,
    # nkCurly,
    # nkCurlyExpr,
    # nkDo,
    # nkDotExpr,
    # nkElifBranch,
    # nkElifExpr,
    # nkElse,
    # nkElseExpr,
    # nkEmpty,
    # nkExprColonExpr,
    # nkExprEqExpr,
    # nkFinally,
    # nkFloat128Lit,
    # nkFloat32Lit,
    # nkFloat64Lit,
    # nkFloatLit,
    # nkFormalParams,
    # nkForStmt,
    # nkFuncDef,
    # nkGenericParams,
    # nkIdent,
    # nkIdentDefs,
    # nkInfix,
    # nkInt16Lit,
    # nkInt32Lit,
    # nkInt64Lit,
    # nkInt8Lit,
    # nkIntLit,
    # nkIteratorDef,
    # nkLambda,
    # nkMacroDef,
    # nkMethodDef,
    # nkMixinStmt,
    # nkNilLit,
    # nkObjConstr,
    # nkPar,
    # nkPostfix,
    # nkPrefix,
    # nkRStrLit,
    # nkStaticStmt,
    # nkStrLit,
    # nkTableConstr,
    # nkTripleStrLit,
    # nkUInt16Lit,
    # nkUInt32Lit,
    # nkUInt64Lit,
    # nkUInt8Lit,
    # nkUIntLit,
    # nkVarTuple,
    # nkWhenExpr,
    # nkWhenStmt,
    # psuedo elements
    # nkCallKinds,

  TokenIndex* = distinct int32
    ## used to point to a token from the token list
  AstIndex* = distinct int32
    ## used to point to additional information for an AST node
  AstLeft* = distinct uint32
    ## might be an `AstIndex`, an int value, or interpretted some other way
    ## based on the `AstNode.kind`
  AstRight* = distinct uint32
    ## might be an `AstIndex`, an int value, or interpretted some other way
    ## based on the `AstNode.kind`
  ModuleId* = int32
    ## id for this module within a compilation
    ## XXX: refactor along with `ModuleAst` as this is mostly to shim with the
    ##      existing compiler instead of properly handling module vs module in
    ##      a package vs during a specific compilation.

const assumedAstLen: Natural = 100

proc initAst(hintAstLen = assumedAstLen): Ast =
  ## initialize an `Ast`, pre-allocate storage based on the `hintAstLen`
  Ast(nodes: newSeqOfCap[AstNode](hintAstLen))

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
  
  result = ankEmpty

proc legacyAppendPNode*(m: var ModuleAst; n: PNode) =
  ## take `n` the `PNode`, from parsing, and append it to `m` the `ModuleAst`.
  let
    kind = legacyNodeToAstKind(n)
    token = m.tokens.legacyAdd(n)

  # xxx: implement left and right properly, see below

  m.ast.nodes.add AstNode(
    kind: kind,
    token: token,
    left: AstLeft 1,
    right: AstRight 2
  )