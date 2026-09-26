## Provides all magic and built-in symbols exported by the compiler.

# TODO: move all magic types and routine declarations into this module

include system/inclrtl # currently needed for the `.benign` pragma

# TODO: make `NimNodeKind`, `NimTypeKind`, and `NimSymKind` magic types (or
#       have them be defined in the compiler)

template skipEnumValue(define: untyped, predecessor: untyped; gap = 1): untyped =
  ## This template is used to keep the ordinal values of the ``TNodeKind``
  ## enum in sync with the ``NimNodeKind`` enum.
  ##
  ## It is expected that each ``nkX`` has the same underlying value as the
  ## corresponding ``nnkX``, but this expectation is violated when removing
  ## a ``TNodeKind`` entry (plus the corresponding ``NimNodeKind``) and then
  ## compiling with a compiler that still has the removed ``TNodeKind`` entry
  ## (this can happen when bootstrapping, for example).
  ##
  ## To keep the required compatibility, when a ``TNodeKind`` and corresponding
  ## ``NimNodeKind`` are removed, the successor of the removed enum entry uses
  ## ``skipEnumValue`` to leave a gap in the case that `define`, which is used
  ## to indicate that the enum entry is not present in the compiler, is not
  ## defined.
  ##
  ## `gap` specifies the amount of enum fields to skip.
  when defined(define):
    ord(predecessor) + 1
  else:
    # leave a gap where the removed node kinds are located
    ord(predecessor) + gap + 1

type
  NimNodeKind* = enum
    nnkError,  ## erroneous AST node
    nnkEmpty, nnkIdent, nnkSym,
    nnkType, nnkCharLit, nnkIntLit, nnkInt8Lit,
    nnkInt16Lit, nnkInt32Lit, nnkInt64Lit, nnkUIntLit, nnkUInt8Lit,
    nnkUInt16Lit, nnkUInt32Lit, nnkUInt64Lit, nnkFloatLit,
    nnkFloat32Lit, nnkFloat64Lit,
    nnkStrLit = skipEnumValue(nimskullNoFloat128, nnkFloat64Lit)
    nnkRStrLit, nnkTripleStrLit, nnkNilLit,
    nnkDotCall = skipEnumValue(nimHasNkComesFromNodeRemoved, nnkNilLit)
    nnkCommand, nnkCall, nnkCallStrLit, nnkInfix,
    nnkPrefix, nnkPostfix, nnkHiddenCallConv,
    nnkExprEqExpr,
    nnkExprColonExpr, nnkIdentDefs, nnkVarTuple,
    nnkPar, nnkObjConstr, nnkCurly, nnkCurlyExpr,
    nnkBracket, nnkBracketExpr, nnkPragmaExpr, nnkRange,
    nnkDotExpr, nnkCheckedFieldExpr, nnkDerefExpr, nnkIfExpr,
    nnkElifExpr, nnkElseExpr, nnkLambda, nnkDo, nnkAccQuoted,
    nnkTableConstr, nnkBind,
    nnkClosedSymChoice,
    nnkOpenSymChoice,
    nnkHiddenStdConv,
    nnkHiddenSubConv, nnkConv, nnkCast, nnkStaticExpr,
    nnkAddr, nnkHiddenAddr, nnkHiddenDeref, nnkObjDownConv,
    nnkObjUpConv, nnkChckRangeF, nnkChckRange64, nnkChckRange,
    nnkStringToCString, nnkCStringToString, nnkAsgn,
    nnkFastAsgn, nnkGenericParams, nnkFormalParams, nnkOfInherit,
    nnkImportAs, nnkProcDef, nnkMethodDef, nnkConverterDef,
    nnkMacroDef, nnkTemplateDef, nnkIteratorDef, nnkOfBranch,
    nnkElifBranch, nnkExceptBranch, nnkElse,
    nnkAsmStmt, nnkPragma, nnkPragmaBlock, nnkIfStmt, nnkWhenStmt,
    nnkForStmt,
    nnkWhileStmt = skipEnumValue(nimHasNkParForStmtNodeRemoved, nnkForStmt)
    nnkCaseStmt,
    nnkTypeSection, nnkVarSection, nnkLetSection, nnkConstSection,
    nnkConstDef, nnkTypeDef,
    nnkYieldStmt, nnkDefer, nnkTryStmt, nnkFinally, nnkRaiseStmt,
    nnkReturnStmt, nnkBreakStmt, nnkContinueStmt, nnkBlockStmt, nnkStaticStmt,
    nnkDiscardStmt, nnkStmtList,
    nnkImportStmt,
    nnkImportExceptStmt,
    nnkExportStmt,
    nnkExportExceptStmt,
    nnkFromStmt,
    nnkIncludeStmt,
    nnkBindStmt, nnkMixinStmt, nnkUsingStmt,
    nnkCommentStmt, nnkStmtListExpr, nnkBlockExpr,
    nnkWith = skipEnumValue(nimskullNoNkStmtListTypeAndNkBlockType, nnkBlockExpr, 2),
    nnkWithout,
    nnkTypeOfExpr, nnkObjectTy,
    nnkTupleTy, nnkTupleClassTy, nnkTypeClassTy, nnkStaticTy,
    nnkRecList, nnkRecCase, nnkRecWhen,
    nnkRefTy, nnkPtrTy, nnkVarTy,
    nnkConstTy, nnkMutableTy,
    nnkDistinctTy,
    nnkProcTy,
    nnkIteratorTy,         # iterator type
    nnkSharedTy,           # 'shared T'
    nnkEnumTy,
    nnkEnumFieldDef,
    nnkArgList, nnkPattern
    nnkHiddenTryStmt,
    nnkClosure,
    nnkGotoState,
    nnkFuncDef = skipEnumValue(nimHasNkBreakStateNodeRemoved, nnkGotoState, 2),
    nnkTupleConstr,
    nnkNimNodeLit = skipEnumValue(nimskullNoNkNone, nnkTupleConstr)

  NimTypeKind* = enum  # some types are no longer used, see ast.nim
    ntyNone, ntyBool, ntyChar, ntyEmpty,
    ntyAlias, ntyNil, ntyExpr, ntyStmt,
    ntyTypeDesc, ntyGenericInvocation, ntyGenericBody, ntyGenericInst,
    ntyGenericParam, ntyDistinct, ntyEnum, ntyOrdinal,
    ntyArray, ntyObject, ntyTuple, ntySet,
    ntyRange, ntyPtr, ntyRef, ntyVar,
    ntySequence, ntyProc, ntyPointer, ntyOpenArray,
    ntyString, ntyCString, ntyForward, ntyInt,
    ntyInt8, ntyInt16, ntyInt32, ntyInt64,
    ntyFloat, ntyFloat32, ntyFloat64,
    ntyUInt = skipEnumValue(nimskullNoFloat128, ntyFloat64)
    ntyUInt8, ntyUInt16, ntyUInt32, ntyUInt64,
    ntyUnused1 = skipEnumValue(nimHasTyOwnedRemoved, ntyUInt64),
    ntyUnused2,
    ntyVarargs,
    ntyUncheckedArray,
    ntyError,
    ntyBuiltinTypeClass, ntyUserTypeClass, ntyUserTypeClassInst,
    ntyCompositeTypeClass, ntyInferred, ntyAnd, ntyOr, ntyNot,
    ntyAnything, ntyStatic, ntyFromExpr,
    ntyVoid = skipEnumValue(nimHasTyConceptRemoved, ntyFromExpr)

  NimSymKind* = enum
    nskUnknown, nskConditional, nskDynLib, nskParam,
    nskGenericParam, nskTemp, nskModule, nskType, nskVar, nskLet,
    nskConst, nskResult,
    nskProc, nskFunc, nskMethod, nskIterator,
    nskConverter, nskMacro, nskTemplate, nskField,
    nskEnumField, nskForVar, nskLabel,
    nskStub, nskGenerated

proc `==`*(a, b: NimNode): bool {.magic: "EqNimrodNode", noSideEffect.}
  ## Compare two Nim nodes. Return true if nodes are structurally
  ## equivalent. This means two independently created nodes can be equal.

proc sameType*(a, b: NimNode): bool {.magic: "SameNodeType", noSideEffect.}
  ## Compares two Nim nodes' types. Return true if the types are the same,
  ## e.g. true when comparing alias with original type.

proc len*(n: NimNode): int {.magic: "NLen", noSideEffect.}
  ## Returns the number of children of `n`.

proc `[]`*(n: NimNode, i: int): NimNode {.magic: "NChild", noSideEffect.}
  ## Get `n`'s `i`'th child.

proc `[]=`*(n: NimNode, i: int, child: NimNode) {.magic: "NSetChild",
  noSideEffect.}
  ## Set `n`'s `i`'th child to `child`.

proc add*(father, child: NimNode): NimNode {.magic: "NAdd", discardable,
  noSideEffect, locks: 0.}
  ## Adds the `child` to the `father` node. Returns the
  ## father node so that calls can be nested.

proc add*(father: NimNode, children: varargs[NimNode]): NimNode {.
  magic: "NAddMultiple", discardable, noSideEffect, locks: 0.}
  ## Adds each child of `children` to the `father` node.
  ## Returns the `father` node so that calls can be nested.

proc del*(father: NimNode, idx = 0, n = 1) {.magic: "NDel", noSideEffect.}
  ## Deletes `n` children of `father` starting at index `idx`.

proc kind*(n: NimNode): NimNodeKind {.magic: "NKind", noSideEffect.}
  ## Returns the `kind` of the node `n`.

proc intVal*(n: NimNode): BiggestInt {.magic: "NIntVal", noSideEffect.}
  ## Returns an integer value from any integer literal or enum field symbol.

proc floatVal*(n: NimNode): BiggestFloat {.magic: "NFloatVal", noSideEffect.}
  ## Returns a float from any floating point literal.

proc symKind*(symbol: NimNode): NimSymKind {.magic: "NSymKind", noSideEffect.}
proc getImpl*(symbol: NimNode): NimNode {.magic: "GetImpl", noSideEffect.}
  ## Returns a copy of the declaration of a symbol or `nil`.
proc strVal*(n: NimNode): string  {.magic: "NStrVal", noSideEffect.}
  ## Returns the string value of an identifier, symbol, comment, or string literal.
  ##
  ## See also:
  ## * `strVal= proc<#strVal=,NimNode,string>`_ for setting the string value.

proc getImplTransformed*(symbol: NimNode): NimNode {.magic: "GetImplTransf", noSideEffect.}
  ## For a typed proc returns the AST after transformation pass; this is useful
  ## for debugging how the compiler transforms code (e.g.: `defer`, `for`) but
  ## note that code transformations are implementation dependent and subject to change.
  ## See an example in `tests/macros/tmacros_various.nim`.

proc owner*(sym: NimNode): NimNode {.magic: "SymOwner", noSideEffect.}
  ## Accepts a node of kind `nnkSym` and returns its owner's symbol.
  ## The meaning of 'owner' depends on `sym`'s `NimSymKind` and declaration
  ## context. For top level declarations this is an `nskModule` symbol,
  ## for proc local variables an `nskProc` symbol, for enum/object fields an
  ## `nskType` symbol, etc. For symbols without an owner, `nil` is returned.
  ##
  ## See also:
  ## * `symKind proc<#symKind,NimNode>`_ to get the kind of a symbol
  ## * `getImpl proc<#getImpl,NimNode>`_ to get the declaration of a symbol

proc isInstantiationOf*(instanceProcSym, genProcSym: NimNode): bool {.magic: "SymIsInstantiationOf", noSideEffect.}
  ## Checks if a proc symbol is an instance of the generic proc symbol.
  ## Useful to check proc symbols against generic symbols
  ## returned by `bindSym`.

proc getType*(n: NimNode): NimNode {.magic: "NGetType", noSideEffect.}
  ## With 'getType' you can access the node's `type`:idx:. A Nim type is
  ## mapped to a Nim AST too, so it's slightly confusing but it means the same
  ## API can be used to traverse types. Recursive types are flattened for you
  ## so there is no danger of infinite recursions during traversal. To
  ## resolve recursive types, you have to call 'getType' again. To see what
  ## kind of type it is, call `typeKind` on getType's result.

proc getType*(n: typedesc): NimNode {.magic: "NGetType", noSideEffect.}
  ## Version of `getType` which takes a `typedesc`.

proc typeKind*(n: NimNode): NimTypeKind {.magic: "NGetType", noSideEffect.}
  ## Returns the type kind of the node 'n' that should represent a type, that
  ## means the node should have been obtained via `getType`.

proc getTypeInst*(n: NimNode): NimNode {.magic: "NGetType", noSideEffect.} =
  ## Returns the `type`:idx: of a node in a form matching the way the
  ## type instance was declared in the code.
  runnableExamples:
    type
      Vec[N: static[int], T] = object
        arr: array[N, T]
      Vec4[T] = Vec[4, T]
      Vec4f = Vec4[float32]
    var a: Vec4f
    var b: Vec4[float32]
    var c: Vec[4, float32]
    macro dumpTypeInst(x: typed): untyped =
      newLit(x.getTypeInst.repr)
    doAssert(dumpTypeInst(a) == "Vec4f")
    doAssert(dumpTypeInst(b) == "Vec4[float32]")
    doAssert(dumpTypeInst(c) == "Vec[4, float32]")

proc getTypeInst*(n: typedesc): NimNode {.magic: "NGetType", noSideEffect.}
  ## Version of `getTypeInst` which takes a `typedesc`.

proc getTypeImpl*(n: NimNode): NimNode {.magic: "NGetType", noSideEffect.} =
  ## Returns the `type`:idx: of a node in a form matching the implementation
  ## of the type. Any intermediate aliases are expanded to arrive at the final
  ## type implementation. You can instead use `getImpl` on a symbol if you
  ## want to find the intermediate aliases.
  runnableExamples:
    type
      Vec[N: static[int], T] = object
        arr: array[N, T]
      Vec4[T] = Vec[4, T]
      Vec4f = Vec4[float32]
    var a: Vec4f
    var b: Vec4[float32]
    var c: Vec[4, float32]
    macro dumpTypeImpl(x: typed): untyped =
      newLit(x.getTypeImpl.repr)
    let t = """
object
  arr: array[0 .. 3, float32]
"""
    doAssert(dumpTypeImpl(a) == t)
    doAssert(dumpTypeImpl(b) == t)
    doAssert(dumpTypeImpl(c) == t)

proc signatureHash*(n: NimNode): string {.magic: "NSigHash", noSideEffect.}
  ## Returns a stable identifier derived from the signature of a symbol.
  ## The signature combines many factors such as the type of the symbol,
  ## the owning module of the symbol and others. The same identifier is
  ## used in the back-end to produce the mangled symbol name.

proc getTypeImpl*(n: typedesc): NimNode {.magic: "NGetType", noSideEffect.}
  ## Version of `getTypeImpl` which takes a `typedesc`.

proc `intVal=`*(n: NimNode, val: BiggestInt) {.magic: "NSetIntVal", noSideEffect.}
proc `floatVal=`*(n: NimNode, val: BiggestFloat) {.magic: "NSetFloatVal", noSideEffect.}

proc `strVal=`*(n: NimNode, val: string) {.magic: "NSetStrVal", noSideEffect.}
  ## Sets the string value of a string literal or comment.
  ## Setting `strVal` is disallowed for `nnkIdent` and `nnkSym` nodes; a new node
  ## must be created using `ident` or `bindSym` instead.
  ##
  ## See also:
  ## * `strVal proc<#strVal,NimNode>`_ for getting the string value.
  ## * `ident proc<#ident,string>`_ for creating an identifier.
  ## * `bindSym proc<#bindSym%2C%2CBindSymRule>`_ for binding a symbol.

proc newNimNode*(kind: NimNodeKind,
                 lineInfoFrom: NimNode = nil): NimNode
  {.magic: "NNewNimNode", noSideEffect.}
  ## Creates a new AST node of the specified kind.
  ##
  ## The `lineInfoFrom` parameter is used for line information when the
  ## produced code crashes. You should ensure that it is set to a node that
  ## you are transforming.

proc copyNimNode*(n: NimNode): NimNode {.magic: "NCopyNimNode", noSideEffect.}
proc copyNimTree*(n: NimNode): NimNode {.magic: "NCopyNimTree", noSideEffect.}

proc error*(msg: string, n: NimNode = nil) {.magic: "NError", benign.}
  ## Writes an error message at compile time. The optional `n: NimNode`
  ## parameter is used as the source for file and line number information in
  ## the compilation error message.

proc warning*(msg: string, n: NimNode = nil) {.magic: "NWarning", benign.}
  ## Writes a warning message at compile time.

proc hint*(msg: string, n: NimNode = nil) {.magic: "NHint", benign.}
  ## Writes a hint message at compile time.

proc newIdentNode*(i: string): NimNode {.magic: "StrToIdent", noSideEffect.}
  ## Creates an identifier node from `i`. It is simply an alias for
  ## `ident(string)`. Use that, it's shorter.

proc ident*(name: string): NimNode {.magic: "StrToIdent", noSideEffect.}
  ## Create a new ident node from a string.

type
  BindSymRule* = enum    ## Specifies how `bindSym` behaves. The difference
                         ## between open and closed symbols can be found in
                         ## `<manual.html#symbol-lookup-in-generics-open-and-closed-symbols>`_
    brClosed,            ## only the symbols in current scope are bound
    brOpen,              ## open for overloaded symbols, but may be a single
                         ## symbol if not ambiguous (the rules match that of
                         ## binding in generics)
    brForceOpen          ## same as brOpen, but it will always be open even
                         ## if not ambiguous (this cannot be achieved with
                         ## any other means in the language currently)

proc bindSym*(ident: string | NimNode, rule: BindSymRule = brClosed): NimNode {.
              magic: "NBindSym", noSideEffect.}
  ## Creates a node that binds `ident` to a symbol node. The bound symbol
  ## may be an overloaded symbol.
  ## if `ident` is a NimNode, it must have `nnkIdent` kind.
  ## If `rule == brClosed` either an `nnkClosedSymChoice` tree is
  ## returned or `nnkSym` if the symbol is not ambiguous.
  ## If `rule == brOpen` either an `nnkOpenSymChoice` tree is
  ## returned or `nnkSym` if the symbol is not ambiguous.
  ## If `rule == brForceOpen` always an `nnkOpenSymChoice` tree is
  ## returned even if the symbol is not ambiguous.
  ##
  ## See the `manual <manual.html#macros-bindsym>`_ for more details.

when defined(nimskullHasUnaryGenSym):
  proc genSym*(ident = ""): NimNode {.magic: "NGenSym", noSideEffect.}
    ## Generates a fresh symbol that is guaranteed to be unique. The symbol
    ## needs to occur in a declaration context.
else:
  proc genSym*(kind: NimSymKind = nskLet; ident = ""): NimNode {.
    magic: "NGenSym", noSideEffect.}

proc callsite*(): NimNode {.magic: "NCallSite", benign, deprecated:
  "Deprecated since v0.18.1; use `varargs[untyped]` in the macro prototype instead".}
  ## Returns the AST of the invocation expression that invoked this macro.
  # see https://github.com/nim-lang/RFCs/issues/387 as candidate replacement.

proc getLine*(arg: NimNode): int {.magic: "NLineInfo", noSideEffect.}
proc getColumn*(arg: NimNode): int {.magic: "NLineInfo", noSideEffect.}
proc getFile*(arg: NimNode): string {.magic: "NLineInfo", noSideEffect.}

proc copyLineInfo*(arg: NimNode, info: NimNode) {.magic: "NLineInfo", noSideEffect.}
  ## Copy lineinfo from `info`.

proc parseExpr*(s: string, err: var string): NimNode {.
  magic: "ParseExprToAst", noSideEffect.}

proc parseStmt*(s: string, err: var string): NimNode {.
  magic: "ParseStmtToAst", noSideEffect.}

proc getAst*(macroOrTemplate: untyped): NimNode {.magic: "ExpandToAst", noSideEffect.}
  ## Obtains the AST nodes returned from a macro or template invocation.
  ## See also `genasts.genAst`.
  ## Example:
  ##
  ## .. code-block:: nim
  ##
  ##   macro FooMacro() =
  ##     var ast = getAst(BarTemplate())

proc quote*(bl: typed, op = "``"): NimNode {.magic: "QuoteAst", noSideEffect.} =
  ## Quasi-quoting operator.
  ## Accepts an expression or a block and returns the AST that represents it.
  ## Within the quoted AST, you are able to interpolate NimNode expressions
  ## from the surrounding scope. If no operator is given, quoting is done using
  ## backticks. Otherwise, the given operator must be used as a prefix operator
  ## for any interpolated expression. The original meaning of the interpolation
  ## operator may be obtained by escaping it (by prefixing it with itself) when used
  ## as a unary operator:
  ## e.g. `@` is escaped as `@@`, `&%` is escaped as `&%&%` and so on; see examples.
  ##
  ## A custom operator interpolation needs accent quoted (``) whenever it resolves
  ## to a symbol.
  ##
  ## See also:
  ## * `genasts <genasts.html>`_
  runnableExamples:
    macro check(ex: untyped) =
      # this is a simplified version of the check macro from the
      # unittest module.

      # If there is a failed check, we want to make it easy for
      # the user to jump to the faulty line in the code, so we
      # get the line info here:
      var info = ex.lineinfo

      # We will also display the code string of the failed check:
      var expString = ex.toStrLit

      # Finally we compose the code to implement the check:
      result = quote do:
        if not `ex`:
          echo `info` & ": Check failed: " & `expString`
    check 1 + 1 == 2

  runnableExamples:
    # example showing how to define a symbol that requires backtick without
    # quoting it.
    var destroyCalled = false
    macro bar() =
      let s = newTree(nnkAccQuoted, ident"=destroy")
      # let s = ident"`=destroy`" # this would not work
      result = quote do:
        type Foo = object
        # proc `=destroy`(a: var Foo) = destroyCalled = true # this would not work
        proc `s`(a: var Foo) = destroyCalled = true
        block:
          let a = Foo()
    bar()
    doAssert destroyCalled

  runnableExamples:
    # custom `op`
    var destroyCalled = false
    macro bar(ident) =
      var x = 1.5
      result = quote("@") do:
        type Foo = object
        let `@ident` = 0 # custom op interpolated symbols need quoted (``)
        proc `=destroy`(a: var Foo) =
          doAssert @x == 1.5
          doAssert compiles(@x == 1.5)
          let b1 = @[1,2]
          let b2 = @@[1,2]
          doAssert $b1 == "[1, 2]"
          doAssert $b2 == "@[1, 2]"
          destroyCalled = true
        block:
          let a = Foo()
    bar(someident)
    doAssert destroyCalled

    proc `&%`(x: int): int = 1
    proc `&%`(x, y: int): int = 2

    macro bar2() =
      var x = 3
      result = quote("&%") do:
        var y = &%x # quoting operator
        doAssert &%&%y == 1 # unary operator => need to escape
        doAssert y &% y == 2 # binary operator => no need to escape
        doAssert y == 3
    bar2()

proc evalToAst*[T](x: T): NimNode {.magic: "EvalToAst".}
  ## Leaked implementation detail. **Do not use**.

proc eqIdent*(a: string; b: string): bool {.magic: "EqIdent", noSideEffect.}
  ## Style insensitive comparison.

proc eqIdent*(a: NimNode; b: string): bool {.magic: "EqIdent", noSideEffect.}
  ## Style insensitive comparison.  `a` can be an identifier or a
  ## symbol. `a` may be wrapped in an export marker
  ## (`nnkPostfix`) or quoted with backticks (`nnkAccQuoted`),
  ## these nodes will be unwrapped.

proc eqIdent*(a: string; b: NimNode): bool {.magic: "EqIdent", noSideEffect.}
  ## Style insensitive comparison.  `b` can be an identifier or a
  ## symbol. `b` may be wrapped in an export marker
  ## (`nnkPostfix`) or quoted with backticks (`nnkAccQuoted`),
  ## these nodes will be unwrapped.

proc eqIdent*(a: NimNode; b: NimNode): bool {.magic: "EqIdent", noSideEffect.}
  ## Style insensitive comparison.  `a` and `b` can be an
  ## identifier or a symbol. Both may be wrapped in an export marker
  ## (`nnkPostfix`) or quoted with backticks (`nnkAccQuoted`),
  ## these nodes will be unwrapped.

when defined(nimMacrosGetNodeId):
  proc nodeID*(n: NimNode): int {.magic: "NodeId".}
    ## Returns the id of `n`. This proc is for the purpose to debug the
    ## compiler only.

proc getSize*(arg: NimNode): int {.magic: "NSizeOf", noSideEffect.}
  ## Returns the same result as `system.sizeof` if the size is
  ## known by the Nim compiler. Returns a negative value if the Nim
  ## compiler does not know the size.
proc getAlign*(arg: NimNode): int {.magic: "NSizeOf", noSideEffect.}
  ## Returns the same result as `system.alignof` if the alignment
  ## is known by the Nim compiler. It works on `NimNode` for use
  ## in macro context. Returns a negative value if the Nim compiler
  ## does not know the alignment.
proc getOffset*(arg: NimNode): int {.magic: "NSizeOf", noSideEffect.}
  ## Returns the same result as `system.offsetof` if the offset is
  ## known by the Nim compiler. It expects a resolved symbol node
  ## from a field of a type. Therefore it only requires one argument
  ## instead of two. Returns a negative value if the Nim compiler
  ## does not know the offset.

proc name*(t: typedesc): string {.magic: "TypeTrait".} =
  ## Returns the name of the given type.
  ##
  ## Alias for `system.\`$\`(t) <dollars.html#$,typedesc>`_ since Nim v0.20.
  runnableExamples:
    doAssert name(int) == "int"
    doAssert name(seq[string]) == "seq[string]"

proc arity*(t: typedesc): int {.magic: "TypeTrait".} =
  ## Returns the arity of the given type. This is the number of "type"
  ## components or the number of generic parameters a given type `t` has.
  runnableExamples:
    doAssert arity(int) == 0
    doAssert arity(seq[string]) == 1
    doAssert arity(array[3, int]) == 2
    doAssert arity((int, int, float, string)) == 4

proc genericHead*(t: typedesc): typedesc {.magic: "TypeTrait".} =
  ## Accepts an instantiated generic type and returns its
  ## uninstantiated form.
  ## A compile-time error will be produced if the supplied type
  ## is not generic.
  ##
  ## **See also:**
  ## * `stripGenericParams proc <#stripGenericParams,typedesc>`_
  runnableExamples:
    type
      Foo[T] = object
      FooInst = Foo[int]
      Foo2 = genericHead(FooInst)

    doAssert Foo2 is Foo and Foo is Foo2
    doAssert genericHead(Foo[seq[string]]) is Foo
    doAssert not compiles(genericHead(int))

    type Generic = concept f
      type _ = genericHead(typeof(f))

    proc bar(a: Generic): typeof(a) = a

    doAssert bar(Foo[string].default) == Foo[string]()
    doAssert not compiles bar(string.default)

    when false: # these don't work yet
      doAssert genericHead(Foo[int])[float] is Foo[float]
      doAssert seq[int].genericHead is seq

proc stripGenericParams*(t: typedesc): typedesc {.magic: "TypeTrait".} =
  ## This trait is similar to `genericHead <#genericHead,typedesc>`_, but
  ## instead of producing an error for non-generic types, it will just return
  ## them unmodified.
  runnableExamples:
    type Foo[T] = object

    doAssert stripGenericParams(Foo[string]) is Foo
    doAssert stripGenericParams(int) is int

proc supportsCopyMem*(t: typedesc): bool {.magic: "TypeTrait".}
  ## This trait returns true if the type `t` is safe to use for
  ## `copyMem`:idx:.
  ##
  ## Other languages name a type like these `blob`:idx:.

proc supportsZeroMem*(t: typedesc): bool {.magic: "TypeTrait".}
  ## This trait returns true if using `zeroMem`:idx: on a location of type `t`
  ## brings the location into its "default-initialized" state. This doesn't
  ## imply that using `zeroMem`:idx: on a location already storing a value is
  ## valid.

proc isNamedTuple*(T: typedesc): bool {.magic: "TypeTrait".} =
  ## Returns true for named tuples, false for any other type.
  runnableExamples:
    doAssert not isNamedTuple(int)
    doAssert not isNamedTuple((string, int))
    doAssert isNamedTuple(tuple[name: string, age: int])

proc distinctBase*(T: typedesc, recursive: static bool = true): typedesc {.magic: "TypeTrait".} =
  ## Returns the base type for distinct types, or the type itself otherwise.
  ## If `recursive` is false, only the immediate distinct base will be returned.
  ##
  ## **See also:**
  ## * `distinctBase template <#distinctBase.t,T,static[bool]>`_
  runnableExamples:
    type MyInt = distinct int
    type MyOtherInt = distinct MyInt
    doAssert distinctBase(MyInt) is int
    doAssert distinctBase(MyOtherInt) is int
    doAssert distinctBase(MyOtherInt, false) is MyInt
    doAssert distinctBase(int) is int

proc tupleLen*(T: typedesc[tuple]): int {.magic: "TypeTrait".} =
  ## Returns the number of elements of the tuple type `T`.
  ##
  ## **See also:**
  ## * `tupleLen template <#tupleLen.t>`_
  runnableExamples:
    doAssert tupleLen((int, int, float, string)) == 4
    doAssert tupleLen(tuple[name: string, age: int]) == 2

proc rangeBase*(t: typedesc): typedesc {.magic: "TypeTrait".} =
  ## Returns the base type of the ``range`` type `t`. Only a single level is
  ## skipped, that is, if the range type's base type is also a range type,
  ## the base type is not skipped.
  runnableExamples:
    type
      Range = range[1..3]
      Nested = range[Range(1)..Range(3)]

    doAssert rangeBase(Range) is int
    doAssert rangeBase(Range) isnot range
    doAssert rangeBase(Nested) is Range

proc isCyclical*(t: typedesc): bool {.magic: "TypeTrait".} =
  ## Returns whether the type `t` is *potentially* able to be part of a
  ## reference cycle when used as the type of a managed heap location.
  runnableExamples:
    type
      NoCycle = object
        x: seq[NoCycle]
      NoCycle2 {.acyclic.} = ref object
        x: NoCycle2
      Cycle = ref object
        x: Cycle

    doAssert not isCyclical(NoCycle)
    doAssert not isCyclical(NoCycle2)
    doAssert isCyclical(Cycle)

type
  CacheSeq* = distinct string
    ## Compile-time sequence of `NimNode`s.
  CacheTable* = distinct string
    ## Compile-time table of key-value pairs.
    ##
    ## Keys are `string`s and values are `NimNode`s.
  CacheCounter* = distinct string
    ## Compile-time counter, uses `int` for storing the count.

# TODO: remove the distinct types for the cache names and have the macrocache
#       magics accept raw strings. Providing a type-safe macro-cache access is
#       the responsibility of the version of the standard library

proc value*(c: CacheCounter): int {.magic: "NccValue".} =
  ## Returns the value of a counter `c`.
  runnableExamples:
    static:
      let counter = CacheCounter"valTest"
      # default value is 0
      assert counter.value == 0

      inc counter
      assert counter.value == 1

proc inc*(c: CacheCounter; by = 1) {.magic: "NccInc".} =
  ## Increments the counter `c` with the value `by`.
  runnableExamples:
    static:
      let counter = CacheCounter"incTest"
      inc counter
      inc counter, 5

      assert counter.value == 6

proc add*(s: CacheSeq; value: NimNode) {.magic: "NcsAdd".} =
  ## Adds `value` to `s`.
  runnableExamples:
    import std/macros
    const mySeq = CacheSeq"addTest"

    static:
      mySeq.add(newLit(5))
      mySeq.add(newLit("hello ic"))

      assert mySeq.len == 2
      assert mySeq[1].strVal == "hello ic"

proc incl*(s: CacheSeq; value: NimNode) {.magic: "NcsIncl".} =
  ## Adds `value` to `s`.
  ##
  ## .. hint:: This doesn't do anything if `value` is already in `s`.
  runnableExamples:
    import std/macros
    const mySeq = CacheSeq"inclTest"

    static:
      mySeq.incl(newLit(5))
      mySeq.incl(newLit(5))

      # still one element
      assert mySeq.len == 1

proc len*(s: CacheSeq): int {.magic: "NcsLen".} =
  ## Returns the length of `s`.
  runnableExamples:
    import std/macros

    const mySeq = CacheSeq"lenTest"
    static:
      let val = newLit("helper")
      mySeq.add(val)
      assert mySeq.len == 1

      mySeq.add(val)
      assert mySeq.len == 2

proc `[]`*(s: CacheSeq; i: int): NimNode {.magic: "NcsAt".} =
  ## Returns the `i`th value from `s`.
  runnableExamples:
    import std/macros

    const mySeq = CacheSeq"subTest"
    static:
      mySeq.add(newLit(42))
      assert mySeq[0].intVal == 42

proc `[]=`*(t: CacheTable; key: string, value: NimNode) {.magic: "NctPut".} =
  ## Inserts a `(key, value)` pair into `t`.
  ##
  ## .. warning:: `key` has to be unique! Assigning `value` to a `key` that is already
  ##   in the table will result in a compiler error.
  runnableExamples:
    import std/macros

    const mcTable = CacheTable"subTest"
    static:
      # assign newLit(5) to the key "value"
      mcTable["value"] = newLit(5)

      # check that we can get the value back
      assert mcTable["value"].kind == nnkIntLit

proc len*(t: CacheTable): int {.magic: "NctLen".} =
  ## Returns the number of elements in `t`.
  runnableExamples:
    import std/macros

    const dataTable = CacheTable"lenTest"
    static:
      dataTable["key"] = newLit(5)
      assert dataTable.len == 1

proc `[]`*(t: CacheTable; key: string): NimNode {.magic: "NctGet".} =
  ## Retrieves the `NimNode` value at `t[key]`.
  runnableExamples:
    import std/macros

    const mcTable = CacheTable"subTest"
    static:
      mcTable["toAdd"] = newStmtList()

      # get the NimNode back
      assert mcTable["toAdd"].kind == nnkStmtList

proc hasNext*(t: CacheTable; iter: int): bool {.magic: "NctHasNext".}
proc next*(t: CacheTable; iter: int): (string, NimNode, int) {.magic: "NctNext".}

proc privateAccess*(t: typedesc) {.magic: "PrivateAccess".}
  ## Enables access to private fields of `t` in current scope.
