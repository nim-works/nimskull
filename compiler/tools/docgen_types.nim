import
  ast/[ast, renderer],
  experimental/dod_helpers,
  std/[
    options,
    macros,
    intsets,
    tables,
    hashes,
    strformat,
    sequtils,
    strutils
  ]

type
  DocEntryKind* = enum
    ## - NOTE :: Different procedure kinds are also used to describe
    ##   operator implementations.
    # procedure kinds start
    ndkProc = "proc" ## Procedure definition
    ndkFunc = "func" ## Procedure definition

    ndkMacro = "macro" ## Macro
    ndkMethod = "method" ## Method
    ndkTemplate = "template" ## \
    ndkIterator = "iterator" ## \
    ndkConverter = "converter" ## User-defined implicit conversion
    # procedure kinds end


    ndkParam = "param" ## Generic parameters
    ndkArg = "arg" ## Entry (function, procedure, macro, template) arguments
    ndkInject = "inject" ## Variable injected into the scope by
    ## template/macro instantiation.
    ndkVar = "var"
    ndkLet = "let"
    ndkConst = "const"
    ndkPragma = "pragma" ## Compiler-specific directives `{.pragma.}` in
    ## nim, `#pragma` in C++ and `#[(things)]` from rust.

    # new type kinds start
    ndkBuiltin = "builtin" ## Builtin type, not defined using any other types
    ndkObject = "object"
    ndkException = "exception" ## Exception object
    ndkDefect = "defect" ## Nim defect object
    ndkConcept = "concet" ## General concept
    ndkTypeclass = "typeclass"
    ndkUnion = "union"
    ndkEnum = "enum" ## Enumeration
    ndkEffect = "effect" ## Side effect tag
    ndkAlias = "alias" ## Typedef
    ndkDistinctAlias = "distinct" ## strong typedef


    # variable-like entries
    ndkCompileDefine = "define" ## Compile-time `define` that might affect
    ## compilation of the program.

    ndkGlobalConst = "globalconst" ## Global immutable compile-time constant
    ndkGlobalVar = "globalvar" ## Global mutable variable
    ndkGlobalLet = "globallet" ## Global immutable variable
    ndkField = "field" ## object/struct field
    ndkTupleField = "tupleField"
    ndkEnumField = "enumfield" ## Enum field/constant
    # end

    # merging all toplevel documentation elemetns into parent module is a
    # wrong approach, because we are loosing the ordering provided by the
    # programmer, and won't be able to recover it later on without
    # implementing annoying location-dependent ordering hacks.
    ndkComment = "comment" ## Toplevel documentation comment that is not
    ## directly attached to any entry (first 'comment' entries in the
    ## module are attached to said module, but it is also possible to place
    ## ones in the middle of the module - those will go into nested comment
    ## entries)
    ndkModule = "module" ## Module (C header file, nim/python/etc. module)
    ndkFile = "file" ## Global or local file
    ndkPackage = "package" ## System or programming language package
    ## (library). If present used as toplevel grouping element.

    ndkLiteralEnum = "enumlit" ## Literal enum value. Nested under enum
    ## field and used to represent specific usages of the enum throughout
    ## the code, including cases where enum set/range is used.
    ndkLiteralString = "strlit" ## Literal string value.

  DocProcKind* = enum
    dpkRegular = "regular" ## Regular user-defined procedure
    dpkOperator = "operator" ## Infix/prefix operator
    dpkConstructor = "=init" ## Constructor procedure like `init()`,
                             ## `initT()` or `newT()`
    dpkDestructor = "=destroy" ## Explicitly declared destructor
    dpkMoveOverride = "=sink"
    dpkCopyOverride = "=copy"
    dpkAsgnOverride = "asgn"
    dpkPropertyGet = "getter"
    dpkPropertySet = "setter"
    dpkPredicate = "predicate"



const
  ndkStructKinds* = { ndkObject, ndkDefect, ndkException, ndkEffect }
  ndkLocalKinds* = { ndkArg, ndkInject, ndkVar, ndkParam, ndkLet, ndkConst }
  ndkGlobalKinds* = { ndkModule, ndkFile }
  ndkNewtypeKinds* = { ndkObject .. ndkDistinctAlias }
  ndkProcKinds* = { ndkProc .. ndkConverter }
  ndkAliasKinds* = { ndkTypeclass, ndkAlias, ndkDistinctAlias }

type
  DocOccurKind* = enum
    dokNone

    dokTypeDirectUse = "typeDirect" ## Direct use of non-generic type
    dokTypeAsParameterUse = "typeAsParam" ## Use as a parameter in generic
                                          ## specialization
    dokTypeSpecializationUse = "typeAsSpecialization" ## Specialization of
                             ## generic type using other types
    dokParametrizationWithArg = "parametrizationWithArg" ## Using
    ## template/macro parameter as a type in nested expression.
    dokTypeAsArgUse = "typeasArg"
    dokTypeAsReturnUse = "typeAsReturn"
    dokTypeAsFieldUse = "typeAsField"
    dokTypeConversionUse = "typeconv"

    dokUsage = "usage"
    dokCall = "call"
    dokExpansion = "expansion"

    dokInheritFrom = "inheritFrom"
    dokOverride = "override"
    dokMacroUsage = "macroUse"
    dokAnnotationUsage = "annotation"

    # local start
    dokVarWrite = "write"
    dokVarRead = "read"
    dokPassTo = "pass"

    dokFieldUse = "fieldUse"
    dokFieldSet = "fieldSet"
    dokEnumFieldUse = "enumFieldUse"

    dokDefineCheck

    dokImported = "imported"
    dokExported = "exported"
    dokIncluded = "included"

    dokInMacroExpansion = "inExpansion"
    dokLiteralUse = "literal"


const
  dokVarUse* = { dokVarWrite, dokVarRead }

declareIdType(Expansion, addHash = true)
declareIdType(DocOccur, addHash = true)
declareIdType(DocEntry, addHash = true)
declareIdType(DocLocation, addHash = true)
declareIdType(DocExtent, addHash = true)


type
  Expansion* = object
    ## Information about macro or template expansion
    expansionOf*: PSym ## Expanded symbol
    expandDepth*: int ## Current active macro/template expansion depth
    expandedFrom*: PNode ## Original expression that node expanded from
    immediateResult*: PNode
    resultNode*: PNode ## Resulting expanded node
    expansionUser*: DocEntryId ## Parent documentable entry that contained macro
    ## expansion (for toplevel entries it is a module)
    nested*: seq[ExpansionId] ## List of the nested expansions in
    resolveMap*: Table[int, PSym] ## Map form node ids generated in the
    ## immediate macro expansion, to their final symbols.

declareStoreType(Expansion)

type
  DocLocation* = object
    ## Single continious slice of the code in file -
    ## file/line:col-start:col-end information
    file*: FileIndex
    line*: int ## Code slice line /index/
    column*: Slice[int] ## Column slice - start and stop position

  DocExtent* = object
    file*: FileIndex
    start*, finish*: tuple[line, column: int]

declareStoreType(DocLocation)
declareStoreType(DocExtent)

type
  DocOccur* = object
    ## Single occurence of documentable entry. When DOD AST and token
    ## storage is implemented
    ## (https://github.com/nim-works/nimskull/discussions/113) this will be
    ## replaced by extra data table associated with each token.
    user* {.requiresInit.}: DocEntryId ## For occurence of global
    ## documentable entry - lexically scoped parent (for function call -
    ## callee, for type - parent composition).
    ##
    ## For toplevel occurence of the global documentable entry (first
    ## encounter of the procedure declaration) - main module. This
    ## conflates with 'nested' relation a little, but only for explicitly
    ## declared entries.
    localUser*: Option[DocEntryId]
    inExpansionOf*: Option[ExpansionId]

    loc*: DocLocationId ## Position of the occurence in the project
    ## files.
    node*: PNode ## Node that occurence happened in. In the future this
    ## should be converted to node IDs
    kind*: DocOccurKind ## Type of entry occurence
    refid*: DocEntryId ## Documentable entry id


declareStoreType(DocOccur)

declareIdType(DocText, addHash = true)

type
  DocTextTreeKind* = enum
    dttStmtList ## Zero or more toplevel statements
    dttParagraph ## Zero or more words, joined in paragraph
    dttText ## Single string - word
    dttCode ## Literal/processed code block/inline. Literal code block
    ## subnode is a single multiline `Text`, processed code block subnode
    ## is a `StmtList[StmtList[]]` of tokens and words (for
    ## punctuation/spacing and registered entries). Code is used for both
    ## rendering back code elements in defintions *and* regular code blocks
    ## supplied in `runnableExamples`.
    dttBold ## Bold text
    dttOverline ## Overline text
    dttItalic ## Italic text
    dttRefDocId ## Reference to the documentable entry
    dttUrl ## Literal standalone URL
    dttSignature ## Link to the element using it's signature
    dttSubtreeLink ## Link to the subtree in the same document
    dttIndexLink ## `:idx`. Ideally this link should be resolved into
    ## reference to the
    dttFootnoteRef ## Footnote reference symbol
    dttLink ## Link to external or internal entry - `Link[<description>,
    ## <target>]` where `description` can be paragraph and target can be a
    ## `RefDocId`, `Url`, `Index` entry and any other kind of link target
    ## subnodes. This kind provides normalized representation of all the
    ## different link kinds that current RST parser has - `:idx:`, raw URL,
    ## formatted URL, (ab)use of link syntax to refer to the documentable
    ## entries in the database.
    dttTable
    dttTableRow
    dttFootnote
    dttTableHeaderCell
    dttTableDataCell
    dttDefList
    dttDefinitionList
    dttUnorderedList
    dttOrderedList
    dttOptionsList
    dttAdmonition
    dttListItem
    dttEmpty ## Empty node
    dttPass ## Raw passthrough
    dttDirArg ## Directive argument
    dttContents ## `.. contents` directive
    dttBlockQuote
    dttHeadline
    dttSubstitute
    dttEmoji


const dttTextKinds* = {
  dttText, dttUrl, dttIndexLink, dttSignature,
  dttSubtreeLink, dttEmoji, dttEmoji,
  dttSubstitute,
  dttDirArg,
  dttFootnoteRef
}

type
  DocTextTree* = ref object
    ## AST of the parsed documentation tree. Closely maps to the RST AST,
    ## but allows embedding metadata directly associated with
    ## documentation, such as resolved link IDs.
    case kind*: DocTextTreeKind
      of dttRefDocId:
        docId*: DocEntryId

      of dttHeadline:
        # REFACTOR this is not necessary, headline levels can be determined
        # via nesting. For now this is kept around to make translation from
        # the RST easier.
        level*: int

      of dttTextKinds, dttFootnote:
        str*: string

      of dttCode:
        code*: seq[DocTextTree]
        isInline*: bool ## whether code block was written as inline, or
                        ## this is a free standalone section
        lang*: string
        # interpretation*: string
        properties*: seq[(string, string)] ## Associated code block
                                           ## property fields

      of dttAdmonition:
        adType*: string ## Admonition type - note, tip, warning and so on.
        # TODO replace with enumeration

      else:
        discard

    subnodes*: seq[DocTextTree]

  DocText* = object
    ## Single chunk of documentation text, can be either source code, or
    ## runnable examples.
    text*: string
    location*: DocLocationId
    tree*: DocTextTree
    case isRunnable*: bool
      of true:
        implicit*: DocEntryId ## Current module id, implicitly
        ## available in the runnable examples execution environment.

      of false:
        discard

declareStoreType(DocText)


type
  DocEntrySet* = object
    ids*: IntSet

  DocIdTableN* = object
    table*: Table[DocEntryId, DocEntrySet]

  DocEntryGroup* = ref object
    ## Arbitrary grouping of the documentable entries
    entries*: seq[DocEntryId]
    nested*: seq[DocEntryGroup]

  DocVisibilityKind* = enum
    dvkPrivate = "private" ## Not exported
    dvkInternal = "internal" ## Exported, but only for internal use
    dvkPublic = "public" ## Exported, available for public use

  DocRequires* = object
    name*: string
    version*: string # TODO expand
    resolved*: Option[DocEntryId]

type
  DocDeclarationContext* = object
    preSem*: bool ## Whether documentation entry was registered before
                  ## semantic pass or after.
    ## Active context of the documentable entry declarations
    whenConditions*: seq[PNode] ## List of nested 'when' statements that were
    ## encountered during recursive visitation.
    whenConditionText*: string

  DocPotentialDependency* = object
    context*: DocDeclarationContext

  DocEntry* = object
    ## Common data for a single documentable entry.
    sym*: PSym ## Symbol that documentable entry was generated from.
    ## Not all entries have that - `define()` targets, projects, libraries
    ## and other elements might be constructed only during pre-sem analysis
    ## and as a result might not have the symbols available.
    node*: PNode ## Node that documentable entry was generated from

    context*: DocDeclarationContext ## Entry declaration context.
    extent*: Option[DocExtentId] ## Full extent of the documentable entry
    ## from first to the last node. Most entries have extents, so field is
    ## densely filled and added directly to an object.
    location*: Option[DocLocationId] ## Source code extent for documentable
    ## entry 'head'. Points to single identifier - entry name in
    ## declaration. Most documentable entries have location data, so this
    ## field is contained directly in the object
    nested*: seq[DocEntryId] ## Nested documentable entries. Use to store
    ## information about fields of a type (for variant fields this might have
    ## more nested fields), arguments of a procedure, enum values and so on.
    ## Module has all elements declared in it listed here, project has a list of
    ## modules
    parent*: Option[DocEntryId] ## Parent declaration entry
    name*: string ## Original identifier name, not disambiguated, without any
    ## extra information about owner/type/generic parameters and so on.
    visibility*: DocVisibilityKind ## Entry visibility from the
    ## documentation reader prespective
    docs*: seq[DocTextId] ## Sequence of the associated documentation chunk
    ## IDs.
    nodeStr*: Option[string]

    case kind*: DocEntryKind
      of ndkPackage:
        version*: string ## Textual version of the package
        author*: string ## Package author name
        license*: string ## Package license as written in the manifest file
        requires*: seq[DocRequires] ## List of required packages

      of ndkStructKinds:
        superTypes*: seq[DocEntryId] ## Parent types for the object or
                                     ## structure definitions

      of ndkEnumField:
        enumValueOverride*: Option[PNode] ## Expression for the enum value override
        enumStringOverride*: Option[string] ## String representation override text

      of ndkArg:
        argType*: PNode ## Argument type description
        argDefault*: PNode ## Expression for argument default value.

      of ndkField:
        fieldType*: PNode
        switchesInto*: seq[tuple[
          expr: seq[PNode], subfields: seq[DocEntryId]]] ## Identifiers for
        ## nested fields that can be accessed based on this field's value

      of ndkAliasKinds:
        baseType*: PNode ## Base type /expression/ of the alias. Might contain
        ## generic type with multiple parameters, so `PNode` is used here
        ## instead of `DocEntryId`
        baseTypeId*: Option[DocEntryId]

      of ndkProcKinds:
        procKind*: DocProcKind ## Procedure declaration kind
        returnType*: Option[PNode]
        wrapOf*: Option[string] ## Optional C, C++ or JS pattern used
        ## in the `.importX` pragma annotation
        dynlibOf*: Option[string] ## Dynamic library pattern for the
        ## procedures
        calls*: DocEntrySet ## Procedures called by entry, for callgraph
        ## construction.
        raises*: DocEntrySet ## Full list of potential raises of a procedure
        ## including both direct and indirect ones
        effects*: DocEntrySet ## All effects for procedure body, including
        ## direct and indirect ones.
        raisesVia*: Table[DocEntryId, DocEntrySet] ## Mapping between
        ## particular raise and called procedure. Direct raises via `raise`
        ## statement are not listed here.
        raisesDirect*: DocEntrySet ## List of exception types that
        ## can be directly raised by the body
        effectsVia*: Table[DocEntryId, DocEntrySet] ## Effect -> called
        ## procMapping. Allows to provide information about causes of the
        ## particular effects in the procedure.
        globalIO*: DocEntrySet ## Global variables that procedure reads from
        ## or writes into.

      else:
        discard

declareStoreType(DocEntry)

type
  ApproximateSymbolLocation* = tuple[nameId, file, col, line: int]

  DocDb* = ref object of RootRef
    deprecatedMsg*: Table[DocEntryId, string]

    entries*: DocEntryStore ## Full list of the documentable entries processed
    currentTop*: DocEntry ## Current toplevel, 'parent' documentable entry
    top*: seq[DocEntryId] ## Full list of the 'top' entries - ones that are
                          ## not nested in anything else
    fileModules*: Table[FileIndex, DocEntryId] ## Mapping between file ID
    ## and the ID of the documentable entry generated from this module's
    ## file
    named*: Table[string, DocEntryId] ## Mapping between named documentable
    ## entries and their respective names. This is used for special
    ## occasions like `--define`-introduced elements, that are not scoped,
    ## global, can be used but cannot be defined.
    expandedNodes*: Table[int, ExpansionId] ## Map between original node
    ## id and the registered expansion
    extents*: DocExtentStore ## Full list of the extent information
    locations*: DocLocationStore ## All locations
    docs*: DocTextStore ## All pieces of the documentation
    expansions*: ExpansionStore ## List of known expansion bettween
    ## open/close for module
    occurencies*: DocOccurStore ## Every tracked occurence
    sigmap*: Table[PSym, DocEntryId] ## Map from symbol to the documentable
    ## entry ID
    enumValueMap*: Table[PSym, DocEntryid] ## Map from the symbol of the
    ## enum field to a enum *value* object.
    strLitMap*: Table[string, DocEntryId] ## Map from the enum string
    ## literal value to the string literal value object.
    locationSigmap*: Table[ApproximateSymbolLocation, DocEntryId] ##[HACK

This field maps declarations only based on the /identifier location/ - this
is necessary becase even after *sem* layer you can get /untyped/ fields on
objects and similar instances of absolutely nonsensical behavior. This
field needs to be removed in the future, but for now this is a workaround.

Related handing logic is in the `[]` for `DocDb` (file tracking logic) and
`addSigmap` updates `.sigmap` when necessary, performing translation of the
symbol nodes into known identifiers.

Example on how this currenly works. The type definition is first
encountered by the declaration registration, and `field` is untyped. We add
it to location sigmap, pointing to the correspoding documentable entry.

.. code-block:: NimMajor

    type
      Other = object
        field: int

When the actual usage of the field is encountered, for example
`Other().field`, `[]` call with `field` will see the `Sym` node, poining to
the same location. It will update the database as needed (put the symbol in
the sigmap) and return associated entry id.

This hack also has a very unfortunate consequence of every `[]` potentially
mutating the DB state.

]##
    toplevelExpansions*: seq[tuple[
      module: DocEntryId, expansions: seq[ExpansionId]]] ## List of
    ## toplevel macro or template expansions



# DOD helper function declarations for the documentable entry database.
declareStoreField(DocDb, entries, DocEntry)
declareStoreField(DocDb, expansions, Expansion)
declareStoreField(DocDb, occurencies, DocOccur)
declareStoreField(DocDb, locations, DocLocation)
declareStoreField(DocDb, extents, DocExtent)
declareStoreField(DocDb, docs, DocText)

declareStoredTableField(DocDb, deprecatedMsg, DocEntry, string)

func add*(de: var DocEntry, id: DocEntryId) =
  de.nested.add id

func addTop*(db: var DocDb, entry: DocEntry): DocEntryId =
  result = db.add entry
  db.top.add result

func len*(s: DocEntrySet): int = s.ids.len
func incl*(s: var DocEntrySet, id: DocEntryId) =
  if id.int != 0:
    s.ids.incl id.int

func `*`*(s1, s2: DocEntrySet): DocEntrySet = DocEntrySet(ids: s1.ids * s2.ids)
func `-`*(s1, s2: DocEntrySet): DocEntrySet = DocEntrySet(ids: s1.ids - s2.ids)

func excl*(s: var DocEntrySet, id: DocEntryId) = s.ids.excl id.int
func contains*(s: DocEntrySet, id: DocEntryId): bool = id.int in s.ids
iterator items*(s: DocEntrySet): DocEntryId =
  for i in s.ids:
    yield DocEntryId(i)

func pop*(s: var DocEntrySet): DocEntryId =
  for it in s:
    result = it
    s.excl result

func incl*(table: var DocIdTableN, idKey, idVal: DocEntryId) =
  table.table.mgetOrPut(idKey, DocEntrySet()).incl idVal

func incl*(s: var DocEntrySet, entry: DocEntry) =
  s.incl entry


proc newDocEntry*(
    db: var DocDb, kind: DocEntryKind, name: string,
    context: DocDeclarationContext = DocDeclarationContext()
  ): DocEntryId =
  ## Create new toplevel entry (package, file, module) directly using DB.
  result = db.add(DocEntry(name: name, kind: kind, context: context))

proc newDocEntry*(
    db: var DocDb,
    parent: DocEntryId, kind: DocEntryKind, name: string,
    context: DocDeclarationContext = DocDeclarationContext()
  ): DocEntryId =
  ## Create new nested document entry. Add it to subnode of `parent` node.
  result = db.add DocEntry(
    name: name, kind: kind, context: context, parent: some parent)
  db[parent].nested.add result

proc getOrNewNamed*(
    db: var DocDb, kind: DocEntryKind, name: string): DocEntryId =
  ## Get new documentable entry or construct a new one using kind and name.
  ## Used primarilily for documentable entries such as `define()` flags,
  ## that have no definition itself, and need to be added when first used.
  if name in db.named:
    result = db.named[name]

  else:
    result = db.newDocEntry(kind, name)
    db.named[name] = result

proc getOrNewStrLit*(db: var DocDb, lit: string): DocEntryId =
  ## Either return or create new documentable entry for the string literal.
  if lit in db.strLitMap:
    result = db.strLitMap[lit]

  else:
    # String literal uses it's value as a name
    result = db.newDocEntry(ndkLiteralString, lit)
    db.strLitMap[lit] = result

func isFromMacro*(db: DocDb, node: PNode): bool =
  ## Check if the node node was created during macro expansion
  assert not node.isNil()
  node.id in db.expandedNodes


proc getExpansion*(db: DocDb, node: PNode): ExpansionId =
  ## Get expansion tha tnode was generated from
  return db.expandedNodes[node.id]

proc getExpansionOriginal*(db: DocDb, node: PNode): PNode =
  db[db.getExpansion(node)].expandedFrom


func `$`*(slice: DocLocation): string =
  &"{slice.file.int}/{slice.line}:{slice.column.a}..{slice.column.b}"

proc `$`*(db: DocDb, id: DocEntryId): string

proc procSignature*(
    db: DocDb, id: DocEntryId, withReturn: bool = true): string =
  assert db[id].kind in ndkProcKinds
  result.add "("
  for idx, arg in db[id].nested:
    if db[arg].kind != ndkArg:  continue
    if 0 < idx: result.add ", "
    result.add db[arg].name
    result.add ": "
    if db[arg].argType.isNil():
      result.add db[arg].nodeStr.get("")

    else:
      result.add $db[arg].argType

  result.add ")"
  if withReturn and db[id].returnType.isSome():
    result.add ": "
    result.add $db[id].returnType.get()

proc fullName*(db: DocDb, id: DocEntryId): string =
  result.add db[id].name
  case db[id].kind:
    of ndkProcKinds:
      result.add procSignature(db, id)

    of ndkField:
      result.add ": "
      result.add $db[id].fieldType

    else:
      discard

proc `$`*(db: DocDb, id: DocLocationId): string = $db[id]

proc `$`*(db: DocDb, id: DocOccurId): string =
  var r: string
  template e(): untyped = db[id]

  r.add &"[{id.int}]: {e().kind} of [{e().refid.int}]"

  if not e().refid.isNil():
    r.add &" ({db[e().refid].kind} '{db.fullName(e().refid)}')"

  r.add &" at {db $ e().loc} user #{e().user.int}"

  if e().inExpansionOf.isSome():
    r.add &" expansion: {e().inExpansionOf.get().int}"

  return r


proc `$`*(db: DocDb, id: DocEntryId): string =
  var r: string
  template e(): untyped = db[id]

  r.add &"[{id.int}]: {e().visibility} {e().kind} '{db.fullName(id)}'"

  if e().parent.isSome():
    r.add &" parent [{e().parent.get.int}]"

  if e().location.isSome():
    r.add &" in {db$e().location.get()}"

  if e().context.preSem:
    r.add " (presem)"

  if e().context.whenConditions.len > 0:
    let conds = e().context.whenConditions.mapIt(
      "(" & $it & ")").join(" and ")

    r.add " available when "
    r.add conds

  if db.hasDeprecatedMsg(id):
    r.add " deprecated"
    let msg = db.getDeprecatedMsg(id)
    if msg.len != 0:
      r.add ": '"
      r.add msg
      r.add "'"

  return r

proc getOptSub*(db: DocDb, parent: DocEntryId, subName: string):
  Option[DocEntryId] =

  ## Get nested entry by name. Might return empty doc entry id if name is
  ## not found.
  var res: DocEntryId
  proc aux(parent: DocEntryId): Option[DocEntryId] =
    for sub in db[parent].nested:
      if db[sub].name == subName:
        return some sub

    if db[parent].kind in ndkStructKinds:
      for super in db[parent].superTypes:
        result = aux(super)
        if result.isSome():
          return

  result = aux(parent)


proc getSub*(db: DocDb, parent: DocEntryId, subName: string): DocEntryId =
  let tmp = getOptSub(db, parent, subName)
  assert tmp.isSome(), "no nested documentable entry '" &
    subName & "' for " & (db$parent)

  return tmp.get()



proc initDocLocation*(
    line, startCol, endCol: int, file: FileIndex): DocLocation =
  if endCol == -1:
    DocLocation(
      line: line, column: Slice[int](a: -1, b: -1), file: file)

  else:
    assert startCol <= endCol, &"{startCol} <= {endCol}"
    DocLocation(
      line: line, column: Slice[int](a: startCol, b: endCol), file: file)

func newTree*(
    kind: DocTextTreeKind, subnodes: varargs[DocTextTree]): DocTextTree =
  result = DocTextTree(kind: kind)
  if 0 < len(subnodes):
    result.subnodes = @subnodes

func newTree*(kind: DocTextTreeKind, str: string): DocTextTree =
  result = DocTextTree(kind: kind)
  result.str = str


func add*(tree: var DocTextTree, other: DocTextTree) =
  tree.subnodes.add other

iterator items*(tree: DocTextTree): DocTextTree =
  for item in tree.subnodes:
    yield item

func `[]`*(tree: DocTextTree, idx: int): DocTextTree = tree.subnodes[idx]
