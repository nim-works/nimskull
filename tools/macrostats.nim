#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Find information about macro and template usage in the nim code base.
## Temporary util to hack to your needs, not meant for anything long term.
##
## Presently it finds all nim files in the compiler code base and checks for
## macro and template definitions. Along with any converters, procs, etc that
## might be part of overload resolution. The stats are meant to determine
## how feasible it would be to:
## 1. force untyped macros and templates to only receive untyped ast
## 2. remove typed macros and templates

import ../compiler / [
    parser, llstream, idents, options, pathutils, ast, lineinfos, msgs, renderer
  ]

import std/os
from std/strutils import startsWith, endsWith, join
from std/sequtils import anyIt

import std/tables

type
  RoutineKind {.pure.} = enum
    rkProc,
    rkFunc,
    rkMethod,
    rkIterator,
    rkConverter,
    rkMacro,
    rkTemplate

  RoutineStat = object
    file: string
    info: TLineInfo
    kind: RoutineKind
    exported: bool
    name: string
    argNames: seq[string]
    argTypes: seq[string]
    argDefaults: seq[string]
    genericArgs: seq[string]
  
  RoutineName = string
  Stats = TableRef[RoutineName, seq[RoutineStat]]

const
  ignoredDirs = [
    # specific exclusions
    "nimskull/bin",
    "nimskull/dist",
    "nimskull/csources",
    "nimskull/build",
    "nimskull/web/upload",
    "nimskull/nimdoc/testproject",

     # likely safe to ignore in all cases
    "/tests",
    "/nimcache",
  ]

proc toRoutineKind(kind: TNodeKind): RoutineKind =
  case kind
  of nkProcDef: rkProc
  of nkFuncDef: rkFunc
  of nkMethodDef: rkMethod
  of nkIteratorDef: rkIterator
  of nkConverterDef: rkConverter
  of nkMacroDef: rkMacro
  of nkTemplateDef: rkTemplate
  else:
    raise newException(ValueError, "failed to convert: " & $kind)

proc onError(conf: ConfigRef, info: TLineInfo, msg: TMsgKind, arg: string) =
  if msg in {errMin .. errMax}:
    let file = conf.toFullPath(info.fileIndex)
    echo "bad things happened at",
          " msg: ", msg,
          " file: ", file, 
          " line: ", info.line, 
          " column: ", info.col,
          " arg: ", arg

proc parseFile(file: string): PNode =
  let
    absFile = AbsoluteFile(file)
    identCache: IdentCache = newIdentCache()
    compConfig: ConfigRef = newConfigRef()
  var p: Parser

  p.lex.errorHandler = onError
  compConfig.verbosity = 0
  compConfig.options.excl optHints

  try:
    openParser(
      p = p,
      filename = absFile,
      inputStream = llStreamOpen(absFile, fmRead),
      cache = identCache,
      config = compConfig
    )
  except:
    echo "butts parser open, file: ", file
    return nil

  try:
    result = p.parseAll()
  except:
    echo "butts parse, file: ", file
    return nil
  finally:
    p.closeParser()

proc collectStats(stats: Stats, file: string, info: TLineInfo, n: PNode) =
  case n.kind
  of nkStmtList:
    for child in n.items:
      collectStats(stats, file, child.info, child)
  of routineDefs:
    let
      nameNode = n[namePos]
      (name, exported) =
        case nameNode.kind
        of nkPostfix:
          ($nameNode[1], true)
        else:
          ($nameNode, false)
      argsNode = n[paramsPos]
      genericArgsNode = n[genericParamsPos]
      (argNames, argTypes, argDefaults) =
        case argsNode.kind
        of nkEmpty: (@[""], @[""], @[""])
        of nkFormalParams:
          if argsNode.len > 1:
            let reserveSpace = argsNode.len - 1
            var
              names = newSeqOfCap[string](reserveSpace)
              types = newSeqOfCap[string](reserveSpace)
              defaults = newSeqOfCap[string](reserveSpace)
            for argIdx in 1..<argsNode.len:
              let
                a = argsNode[argIdx]
                typ = $a[^2]
                defaultValue = $a[^1]
                lastIdentPos = a.safeLen - 3
                  ## skip last items, which are type & default value
              for i in 0..lastIdentPos:
                names.add $a[i]
                types.add typ
                defaults.add defaultValue
            (names, types, defaults)
          else:
            (@[""], @[""], @[""])
        else:
          echo "formal args suck???"
          raise newException(
              ValueError,
              "args didn't work, kind: " & $argsNode.kind &
              " file: " & file &
              " line: " & $info.line &
              " column: " & $info.col
            )
      genericArgs =
        case genericArgsNode.kind
        of nkEmpty: @[""]
        of nkGenericParams:
          var res = newSeqOfCap[string](genericArgsNode.len * 3)
          for g in genericArgsNode.items:
            res.add $g
          res
        else:
          echo "generics suck"
          raise newException(
              ValueError,
              "generic args didn't work, kind: " & $argsNode.kind &
              " file: " & file &
              " line: " & $info.line &
              " column: " & $info.col
            )

    if not stats.hasKey(name):
      stats[name] = newSeqOfCap[RoutineStat](1)
    stats[name].add:
      RoutineStat(
        file: file,
        info: info,
        kind: n.kind.toRoutineKind,
        name: name,
        exported: exported,
        argNames: argNames,
        argTypes: argTypes,
        argDefaults: argDefaults,
        genericArgs: genericArgs
      )
  else:
    discard

proc handleFile(stats: Stats, file: string) =
  let (_, _, extension) = file.splitFile
  if extension == ".nim":
    # echo "file: ", file
    let n = parseFile(file)
    case n.kind
    of nkStmtList, nkWhen:
      for child in n.items:
        collectStats(stats, file, child.info, child)
    of routineDefs:
      collectStats(stats, file, n.info, n)
    else:
      discard
  else:
    # ignore non nim files
    discard

proc handleDir(stats: Stats, dir: string) =
  let name = dir.lastPathPart
  if name.startsWith(".") or ignoredDirs.anyIt(dir.endsWith(it)):
    # ignore .git and other such directories
    discard
  else:
    # echo "dir: ", dir
    discard

    for kind, dirfile in walkDir(dir):
      case kind
      of pcFile:
        handleFile(stats, dirfile)
      of pcDir:
        handleDir(stats, dirfile)
      of pcLinkToDir, pcLinkToFile:
        # ignore symlinks
        discard

var stats: Stats = newTable[RoutineName, seq[RoutineStat]]()

handleDir(stats, getCurrentDir())

proc print(stats: Stats) =
  let cwdLen = getCurrentDir().len

  var
    overloadInstances = 0
    macroCount = 0
    templateCount = 0
    noArgMacros = 0
    noArgTemplates = 0

  for name, routines in stats.pairs:
    # ignore the stats for any that don't include a macro or template
    if routines.len > 1:
      inc overloadInstances
    if routines.anyIt(it.kind in {rkMacro, rkTemplate}):
      echo "routine: ", name, " count: ", routines.len
      for r in routines.items:
        echo "\tkind: ", r.kind,
             ", exported: ", r.exported,
             ", arg types: (", r.argTypes.join(", "), ")",
             ", file: ", r.file.substr(cwdLen)
      
        case r.kind
        of rkMacro:
          inc macroCount
          if r.argTypes.len == 0:
            inc noArgMacros
        of rkTemplate:
          inc templateCount
          if r.argTypes.len == 0:
            inc noArgTemplates
        else:
          discard
  
  echo "Summary:",
       "\nOverloads: ", overloadInstances,
       "\nMacro Count: ", macroCount,
       "\nTemplate Count: ", templateCount,
       "\nNo argument macros: ", noArgMacros,
       "\nNo argument templates: ", noArgTemplates
    
print stats