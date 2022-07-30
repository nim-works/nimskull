import
  std/[
    sequtils,
    with,
    tables,
    strformat,
    os
  ],
  ast/[
    lineinfos,
    renderer
  ],
  front/[
    options,
  ],
  utils/[
    pathutils
  ],
  ./docgen_types

import std/options as std_options

type
  StdVector*[T] {.importcpp: "std::vector", header: "<vector>"} = object

proc pushBack*[T](vec: var StdVector[T], item: T)
  {.importcpp: "#.push_back(@)", header: "<vector>".}

type
  StdString* {.importcpp: "std::string", header: "<string>"} = object

proc initStdString*(str: cstring, count: cint): StdString
  {.importcpp: "std::string(@)", header: "<string>".}

proc initStdString*(str: string): StdString {.inline.} =
  initStdString(str.cstring, str.len.cint)

converter toStdString*(str: string): StdString {.inline.} =
  initStdString(str)

type
  SourcetrailNameHierarchy* {.importcpp: r"sourcetrail::NameHierarchy",
                              header: r"<NameHierarchy.h>".} = object
    nameDelimiter* {.importc: r"nameDelimiter".}: StdString
    nameElements* {.importc: r"nameElements".}: StdVector[SourcetrailNameElement]

  SourcetrailNameElement* {.importcpp: r"sourcetrail::NameElement",
                            header: r"<NameHierarchy.h>".} = object
    prefix* {.importc: r"prefix".}: StdString
    name* {.importc: r"name".}: StdString
    postfix* {.importc: r"postfix".}: StdString


  SourcetrailDatabaseStorage* {.importcpp: r"sourcetrail::DatabaseStorage",
                                header: r"<SourcetrailDBWriter.h>".} = object

  SourcetrailDBWriter* {.importcpp: r"sourcetrail::SourcetrailDBWriter",
                         header: r"<SourcetrailDBWriter.h>".} = object

  SourcetrailSourceRange* {.importcpp: r"sourcetrail::SourceRange",
                            header: "<SourceRange.h>".} = object
    fileId*: cint
    startLine*: cint
    startColumn*: cint
    endLine*: cint
    endColumn*: cint

  SourcetrailDefinitionKind* {.
    importcpp: "sourcetrail::DefinitionKind", header: r"<DefinitionKind.h>".} = enum

    sdkImplicit = 1
    sdkExplicit = 2

  SourcetrailSymbolKind* {.
    importcpp: "sourcetrail::SymbolKind", header: r"<SymbolKind.h>".} = enum

    sskType = 0
    sskBuiltinType = 1
    sskModule = 2
    sskNamespace = 3
    sskPackage = 4
    sskStruct = 5
    sskClass = 6
    sskInterface = 7
    sskAnnotation = 8
    sskGlobalVariable = 9
    sskField = 10
    sskFunction = 11
    sskMethod = 12
    sskEnum = 13
    sskEnumConstant = 14
    sskTypedef = 15
    sskTypeParameter = 16
    sskMacro = 17
    sskUnion = 18

  SourcetrailReferenceKind* {.
    importcpp: "sourcetrail::ReferenceKind", header: r"<ReferenceKind.h>".} = enum

    srkTypeUsage = 0
    srkUsage = 1
    srkCall = 2
    srkInheritance = 3
    srkOverride = 4
    srkTypeArgument = 5
    srkTemplateSpecialization = 6
    srkInclude = 7
    srkImport = 8
    srkMacroUsage = 9
    srkAnnotationUsage = 10



proc initSourcetrailNameHierarchy*(nameElements: StdVector[
    SourcetrailNameElement]): SourcetrailNameHierarchy {.
    importcpp: "sourcetrail::NameHierarchy({@})",
    header: r"<NameHierarchy.h>".}

proc serializeNameHierarchyToJson*(nameHierarchy: SourcetrailNameHierarchy): StdString {.
    importcpp: "(sourcetrail::serializeNameHierarchyToJson(@))",
    header: r"<NameHierarchy.h>".}

proc deserializeNameHierarchyFromJson*(serializedNameHierarchy: StdString;
                                       error: ptr StdString): SourcetrailNameHierarchy {.
    importcpp: "(sourcetrail::deserializeNameHierarchyFromJson(@))",
    header: r"<NameHierarchy.h>".}

proc serializeNameHierarchyToDatabaseString*(
    nameHierarchy: SourcetrailNameHierarchy): StdString {.
    importcpp: "(sourcetrail::serializeNameHierarchyToDatabaseString(@))",
    header: r"<NameHierarchy.h>".}

proc getVersionString*(self: SourcetrailDBWriter): StdString {.
    importcpp: "#.getVersionString(@)", header: "<SourcetrailDBWriter.h>".}
proc getSupportedDatabaseVersion*(self: SourcetrailDBWriter): cint {.
    importcpp: "#.getSupportedDatabaseVersion(@)", header: "<SourcetrailDBWriter.h>".}
proc getLastError*(self: SourcetrailDBWriter): StdString {.
    importcpp: "#.getLastError(@)", header: "<SourcetrailDBWriter.h>".}
proc setLastError*(self: SourcetrailDBWriter; error: StdString): void {.
    importcpp: "#.setLastError(@)", header: "<SourcetrailDBWriter.h>".}
proc clearLastError*(self: var SourcetrailDBWriter): void {.
    importcpp: "#.clearLastError(@)", header: "<SourcetrailDBWriter.h>".}
proc open*(self: var SourcetrailDBWriter; databaseFilePath: StdString): bool {.
    importcpp: "#.open(@)", header: "<SourcetrailDBWriter.h>".}
proc close*(self: var SourcetrailDBWriter): bool {.importcpp: "#.close(@)",
    header: "<SourcetrailDBWriter.h>".}
proc clear*(self: var SourcetrailDBWriter): bool {.importcpp: "#.clear(@)",
    header: "<SourcetrailDBWriter.h>".}
proc isEmpty*(self: SourcetrailDBWriter): bool {.importcpp: "#.isEmpty(@)",
    header: "<SourcetrailDBWriter.h>".}
proc isCompatible*(self: SourcetrailDBWriter): bool {.
    importcpp: "#.isCompatible(@)", header: "<SourcetrailDBWriter.h>".}
proc getLoadedDatabaseVersion*(self: SourcetrailDBWriter): cint {.
    importcpp: "#.getLoadedDatabaseVersion(@)", header: "<SourcetrailDBWriter.h>".}
proc beginTransaction*(self: var SourcetrailDBWriter): bool {.
    importcpp: "#.beginTransaction(@)", header: "<SourcetrailDBWriter.h>".}
proc commitTransaction*(self: var SourcetrailDBWriter): bool {.
    importcpp: "#.commitTransaction(@)", header: "<SourcetrailDBWriter.h>".}
proc rollbackTransaction*(self: var SourcetrailDBWriter): bool {.
    importcpp: "#.rollbackTransaction(@)", header: "<SourcetrailDBWriter.h>".}
proc optimizeDatabaseMemory*(self: var SourcetrailDBWriter): bool {.
    importcpp: "#.optimizeDatabaseMemory(@)", header: "<SourcetrailDBWriter.h>".}
proc recordSymbol*(self: var SourcetrailDBWriter;
                   nameHierarchy: SourcetrailNameHierarchy): cint {.
    importcpp: "#.recordSymbol(@)", header: "<SourcetrailDBWriter.h>".}
proc recordSymbolDefinitionKind*(self: var SourcetrailDBWriter; symbolId: cint;
                                 definitionKind: SourcetrailDefinitionKind): bool {.
    importcpp: "#.recordSymbolDefinitionKind(@)", header: "<SourcetrailDBWriter.h>".}
proc recordSymbolKind*(self: var SourcetrailDBWriter; symbolId: cint;
                       symbolKind: SourcetrailSymbolKind): bool {.
    importcpp: "#.recordSymbolKind(@)", header: "<SourcetrailDBWriter.h>".}
proc recordSymbolLocation*(self: var SourcetrailDBWriter; symbolId: cint;
                           location: SourcetrailSourceRange): bool {.
    importcpp: "#.recordSymbolLocation(@)", header: "<SourcetrailDBWriter.h>".}
proc recordSymbolScopeLocation*(self: var SourcetrailDBWriter; symbolId: cint;
                                location: SourcetrailSourceRange): bool {.
    importcpp: "#.recordSymbolScopeLocation(@)", header: "<SourcetrailDBWriter.h>".}
proc recordSymbolSignatureLocation*(self: var SourcetrailDBWriter;
                                    symbolId: cint;
                                    location: SourcetrailSourceRange): bool {.
    importcpp: "#.recordSymbolSignatureLocation(@)", header: "<SourcetrailDBWriter.h>".}
proc recordReference*(self: var SourcetrailDBWriter; contextSymbolId: cint;
                      referencedSymbolId: cint;
                      referenceKind: SourcetrailReferenceKind): cint {.
    importcpp: "#.recordReference(@)", header: "<SourcetrailDBWriter.h>".}
proc recordReferenceLocation*(self: var SourcetrailDBWriter; referenceId: cint;
                              location: SourcetrailSourceRange): bool {.
    importcpp: "#.recordReferenceLocation(@)", header: "<SourcetrailDBWriter.h>".}
proc recordReferenceIsAmbiuous*(self: var SourcetrailDBWriter; referenceId: cint): bool {.
    importcpp: "#.recordReferenceIsAmbiuous(@)", header: "<SourcetrailDBWriter.h>".}
proc recordReferenceToUnsolvedSymhol*(self: var SourcetrailDBWriter;
                                      contextSymbolId: cint;
                                      referenceKind: SourcetrailReferenceKind;
                                      location: SourcetrailSourceRange): cint {.
    importcpp: "#.recordReferenceToUnsolvedSymhol(@)", header: "<SourcetrailDBWriter.h>".}
proc recordQualifierLocation*(self: var SourcetrailDBWriter;
                              referencedSymbolId: cint;
                              location: SourcetrailSourceRange): bool {.
    importcpp: "#.recordQualifierLocation(@)", header: "<SourcetrailDBWriter.h>".}
proc recordFile*(self: var SourcetrailDBWriter; filePath: StdString): cint {.
    importcpp: "#.recordFile(@)", header: "<SourcetrailDBWriter.h>".}
proc recordFileLanguage*(self: var SourcetrailDBWriter; fileId: cint;
                         languageIdentifier: StdString): bool {.
    importcpp: "#.recordFileLanguage(@)", header: "<SourcetrailDBWriter.h>".}
proc recordLocalSymbol*(self: var SourcetrailDBWriter; name: StdString): cint {.
    importcpp: "#.recordLocalSymbol(@)", header: "<SourcetrailDBWriter.h>".}
proc recordLocalSymbolLocation*(self: var SourcetrailDBWriter;
                                localSymbolId: cint;
                                location: SourcetrailSourceRange): bool {.
    importcpp: "#.recordLocalSymbolLocation(@)", header: "<SourcetrailDBWriter.h>".}
proc recordAtomicSourceRange*(self: var SourcetrailDBWriter;
                              sourceRange: SourcetrailSourceRange): bool {.
    importcpp: "#.recordAtomicSourceRange(@)", header: "<SourcetrailDBWriter.h>".}
proc recordError*(self: var SourcetrailDBWriter; message: StdString;
                  fatal: bool; location: SourcetrailSourceRange): bool {.
    importcpp: "#.recordError(@)", header: "<SourcetrailDBWriter.h>".}

converter toSourcetrailNameElement*(
  structure: tuple[prefix, name, postfix: string]): SourcetrailNameElement =

  result.prefix = structure.prefix
  result.name = structure.name
  result.postfix = structure.postfix

converter initSourcetrailNameHierarchy*(
    args: tuple[
      delimiter: string,
      nameHierarchy: seq[tuple[prefix, name, postfix: string]]
    ]
  ): SourcetrailNameHierarchy =

  result.nameDelimiter = args.delimiter
  for element in args.nameHierarchy:
    result.nameElements.pushBack element

proc recordSymbol*(
  writer: var SourcetrailDBWriter,
  symbol: SourcetrailNameHierarchy,
  symbolKind: SourcetrailSymbolKind,
  definitionKind: SourcetrailDefinitionKind = sdkExplicit): cint =

  result = writer.recordSymbol(symbol)
  discard writer.recordSymbolDefinitionKind(result, definitionKind)
  discard writer.recordSymbolKind(result, symbolKind)

proc recordSymbol*(
    writer: var SourcetrailDBWriter,
    symbolKind: SourcetrailSymbolKind,
    hierarchy: varargs[tuple[prefix, name, postfix: string]]
  ): cint =

  result = writer.recordSymbol((".", toSeq(hierarchy)), symbolKind)


proc toRange(fileId: cint, codeRange: DocLocation): SourcetrailSourceRange =
  with result:
    fileId = fileId
    startLine = codeRange.line.cint
    endLine = codeRange.line.cint
    startColumn = codeRange.column.a.cint + 1
    endColumn = codeRange.column.b.cint + 1

proc toRange(fileId: cint, extent: DocExtent): SourcetrailSourceRange =
  with result:
    fileId = fileId
    startLine = extent.start.line.cint
    startColumn = extent.start.column.cint + 1
    endLine = extent.finish.line.cint
    endColumn = extent.finish.column.cint + 1

proc getFile(writer: var SourcetrailDBWriter, path, lang: string): cint =
  result = writer.recordFile(path)
  discard writer.recordFileLanguage(result, lang)

type
  IdMap* = object
    docToTrail: Table[DocEntryId, cint]
    localToTrail: Table[DocEntryId, cint]
    fileToTrail: Table[FileIndex, cint]
    db: DocDb

proc toTrail(kind: DocOccurKind): SourcetrailReferenceKind =
  case kind:
    of dokTypeAsFieldUse, dokTypeAsReturnUse, dokTypeDirectUse,
      dokTypeAsParameterUse, dokTypeAsArgUse, dokTypeConversionUse:
      result = srkTypeUsage

    of dokTypeSpecializationUse:
      # sourcetrail 'template specialization' relations is used in order to
      # show that one type is a generic specialization of another type. In
      # haxdoc 'generic specialization' is used that in this particular
      # case generic type was specialized with some parameter - without
      # describing /context/ in which declaration ocurred. Maybe later I
      # will add support for 'context ranges' in annotation sources and
      # differentiate between 'specialized generic used as a field' and
      # 'inherited from specialized generic'
      result = srkTypeUsage

    of dokInheritFrom:
      result = srkInheritance

    of dokCall:
      result = srkCall

    of dokEnumFieldUse, dokVarRead, dokVarWrite,
       dokFieldUse, dokFieldSet:
      result = srkUsage

    of dokAnnotationUsage, dokDefineCheck, dokExpansion:
      result = srkMacroUsage

    else:
      assert false, $kind


proc nonlocalUser(db: DocDb, occur: DocOccur): DocEntryId =
  # For `proc new(finalizer: proc(x: ref T))` user of `x` is another local
  # documentable entry. Sourcetrail does not register these kind of
  # relations so we redirect it to the first parent that can process this.
  result = occur.user
  while db[result].kind in ndkLocalKinds:
    result = db[result].parent.get()

      
proc registerUses*(
    writer: var SourcetrailDBWriter,
    idMap: IdMap, db: DocDb, conf: ConfigRef) =

  for id, occur in db.occurencies:
    if db[occur.loc].file notin idMap.fileToTrail:
      continue

    let fileId = idMap.fileToTrail[db[occur.loc].file]
    if db[occur.refid].kind in ndkLocalKinds:
      # Register location of the local variable use -
      # var/let/const/parameter.
      discard writer.recordLocalSymbolLocation(
        idMap.localToTrail[occur.refid], toRange(fileId, db[occur.loc]))

    else:
      let user = db.nonlocalUser(occur)
      let userId = idMap.docToTrail[user]
      if occur.kind in {dokImported}:
        if true:
          # Record module imports as file-file relations
          if db[occur.refid].location.isSome():
            let loc = db[occur.refid].location.get()
            discard writer.recordReferenceLocation(
              writer.recordReference(
                fileId,
                writer.getFile(conf[db[loc].file].fullPath.string, "nim"),
                srkInclude
              ), toRange(fileId, db[occur.loc]))

        elif false:
          # Record module relationship as 'include' between file and
          # module inside another file.
          discard writer.recordReferenceLocation(
            writer.recordReference(
              fileId,
              idMap.docToTrail[occur.refid],
              srkInclude
            ), toRange(fileId, db[occur.loc]))

        elif false:
          # Record relations betwen modules as imports
          discard writer.recordReferenceLocation(
            writer.recordReference(
              idMap.docToTrail[occur.user],
              idMap.docToTrail[occur.refid],
              srkImport
            ), toRange(fileId, db[occur.loc]))

      elif occur.kind notin {dokExported}:
        let refSym = writer.recordReference(
          userId,
          idMap.docToTrail[occur.refid],
          occur.kind.toTrail())

        discard writer.recordReferenceLocation(
          refSym, toRange(fileId, db[occur.loc]))


iterator parents(db: DocDb, id: DocEntryId): DocEntryId =
  var buf: seq[DocEntryId] = @[id]
  var curr = id
  while db[curr].parent.isSome():
    curr = db[curr].parent.get()
    buf.add curr

  for idx in countdown(buf.high, 0):
    yield buf[idx]

proc toTrailName(db: DocDb, id: DocEntryId): SourcetrailNameHierarchy =
  var parts: seq[tuple[prefix, name, postfix: string]]
  for part in db.parents(id):
    if db[part].kind in ndkProcKinds:
      var ret = ""
      if db[part].returnType.isSome():
        ret = ": "
        ret.add $db[part].returnType.get()

      parts.add (ret, db[part].name, db.procSignature(part, false))

    else:
      parts.add ("", db[part].name, "")

  return initSourcetrailNameHierarchy(("::", parts))


proc toTrail(kind: DocEntryKind): SourcetrailSymbolKind =
  case kind:
   of ndkNewtypeKinds - { ndkAlias, ndkDistinctAlias, ndkEnum }:
     sskStruct

   of ndkProc, ndkFunc, ndkConverter, ndkIterator:
     sskFunction

   of ndkMacro, ndkTemplate:
     sskMacro

   of ndkAlias, ndkDistinctAlias:
     sskTypedef

   of ndkGlobalConst, ndkGlobalVar, ndkGlobalLet:
     sskGlobalVariable

   of ndkCompileDefine:
     # compile-time defines might be treated as macros or as global
     # varibles. I'm not exactly sure how to classify them, but for
     # now I think global variable describes sematics a little better.
     sskGlobalVariable

   of ndkEnum:       sskEnum
   of ndkField:      sskField
   of ndkTupleField: sskField
   of ndkEnumField:  sskEnumConstant
   of ndkBuiltin:    sskBuiltinType
   of ndkPragma:     sskAnnotation
   of ndkModule:     sskModule
   of ndkPackage:    sskPackage
   of ndkMethod:     sskMethod
   of ndkFile:       sskModule

   else:
     assert false, $kind
     sskMethod


proc registerDb*(
    writer: var SourcetrailDBWriter, idMap: IdMap, db: DocDb): IdMap =
  result = idMap
  proc toRange(loc: DocLocationId): auto =
    let
      loc = db[loc]
      fileId = idMap.fileToTrail[loc.file]

    return toRange(fileId, loc)

  proc toRange(loc: DocExtentId): auto =
    let
      loc = db[loc]
      fileId = idMap.fileToTrail[loc.file]

    return toRange(fileId, loc)

  for id, entry in db.entries:
    if entry.kind in ndkLocalKinds:
      result.localToTrail[id] = writer.recordLocalSymbol($id.int)

    elif (entry.kind in {ndkPackage} and entry.name == ""):
      continue

    else:
      let symId = writer.recordSymbol(db.toTrailName(id), entry.kind.toTrail())
      result.docToTrail[id] = symId
      if entry.location.isSome():
        discard writer.recordSymbolLocation(symId, toRange(entry.location.get()))
        if entry.kind notin {ndkModule, ndkFile}:
          discard writer.recordSymbolScopeLocation(
            symId, toRange(entry.extent.get()))


proc open*(writer: var SourcetrailDBWriter, file: AbsoluteFile) =
  discard writer.open(file.string)

proc registerFiles*(writer: var SourcetrailDBWriter, conf: ConfigRef): IdMap =
  for id, info in conf.m.fileInfos:
    result.fileToTrail[FileIndex(id)] =
      writer.getFile(info.fullPath.string, "nim")

proc registerFullDb*(
    conf: ConfigRef, writer: var SourcetrailDBWriter, db: DocDb) =
  discard writer.beginTransaction()
  let idMap = writer.registerDb(writer.registerFiles(conf), db)
  discard writer.commitTransaction()

  discard writer.beginTransaction()
  writer.registerUses(idMap, db, conf)
  discard writer.commitTransaction()

const
  sourcetrailDbExt* = "srctrldb"
  sourcetrailProjectExt* = "srctrlprj"



proc writeSourcetrail*(conf: ConfigRef, db: DocDb, outFile: AbsoluteFile) =
  var writer: SourcetrailDBWriter
  let outFile = outFile.changeFileExt(sourcetrailDbExt)
  echo outFile
  assert existsDir parentDir(outFile.string), outFile.string
  removeFile outFile.string
  writer.open(outFile)
  registerFullDb(conf, writer, db)
  discard writer.close()

