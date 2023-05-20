#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the style checker.
## 
## Directionally, style will become part of the language, think `go fmt` where
## violations are errors.
## 
## Currently focused on names, but should cover spacing, and more.
## 
## ## General rules of style:
## - Saem decides style matters
## - lower/upper camel case good; snake case bad
## - lower camel case for most things
## - upper camel case for non-runtime things (types, some consts)
## - single letter names are ok, should be lower; tolerate upper
## - all-caps snake case is occassionally tolerated

import
  std/[
    strutils
  ],
  compiler/ast/[
    ast,
    lineinfos,
    wordrecg,
    reports
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/utils/[
    idioms,
  ]

# TODO: linter should have its own diag/event/telemetry types
from compiler/ast/reports_sem import SemReport

const
  Letters* = {'a'..'z', 'A'..'Z', '0'..'9', '\x80'..'\xFF', '_'}

proc identLen*(line: string, start: int): int =
  while start+result < line.len and line[start+result] in Letters:
    inc result

proc `=~`(s: string, a: openArray[string]): bool =
  for x in a:
    if s.startsWith(x): return true

const 
  skContainers = {skModule, skPackage}
  skSingletonDef = {skType, skGenericParam}
  skLiteralValue = {skEnumField, skLabel}
  skOverloadableDef = routineKinds
  skLocals = {skLet, skVar, skParam, skForVar, skField}
  skIgnore = {skUnknown, skConditional, skDynLib, skStub, skTemp}
  # skResult needs to be accounted for here

type
  NamingStyle = enum
    ## list of various styles of naming that can occur, note many of these are
    ## present not because they're good, but because the legacy nim compiler
    ## and standard library are a mess
    upperCamelStyle       ## UpperCamelCase, tolerate underscores
    lowerCamelStyle       ## lowerCamelCase, tolerate underscores
    mixedCamelStyle       ## lower/UpperCamelCase, tolerate underscores
    lowerCamelStrict      ## lowerCamelCase, do not tolerate underscores
    atomStyle             ## single lower/upper case letter
    shoutingStyle         ## SHOUTING_STYLE; disgusting

proc beautifyName(s: string, k: TSymKind): string =
  # xxx: these style rules are a mess, any changes to the actual rules require
  #      Saem's approval in the PR.

  let
    allUpper = allCharsInSet(s, {'A'..'Z', '0'..'9', '_'})
    hasUnderscore = '_' in s
  if allUpper and k in {skType, skConst, skEnumField}: return s

  # fast tracked handling
  case k
  of skType:
    # Types should start with a capital unless builtins like 'int' etc.:
    # TODO: this should be defined elsewhere, `wordrecg`?
    const builtIns = ["int", "int8", "int16", "int32", "int64",
      "uint", "uint8", "uint16", "uint32", "uint64",
      "float", "float32", "float64",
      "cint", "cuint", "clong", "cstring",
      "string", "char", "byte", "bool", "openArray", "seq", "array", "void",
      "pointer", "csize", "csize_t", "cdouble", "cchar", "cschar",
      "cshort", "cu", "nil", "typedesc", "auto", "any",
      "range", "openarray", "varargs", "set", "cfloat", "ref", "ptr",
      "untyped", "typed", "static", "sink", "lent", "type"]
    for b in builtIns.items:
      if s == b:
        result = b
  of skResult:
    result = "result"
  of skIgnore:
    unreachable("got an impossible symbol kind: " & $k)
  else:
    discard

  if result != "":
    return # we got a fast tracked result

  result = newStringOfCap(s.len)
  var style: NamingStyle
  case k
  of skContainers:
    style = lowerCamelStyle
  of skSingletonDef:
    style =
      if allUpper: shoutingStyle
      else:        upperCamelStyle
  of skConst:
    style = mixedCamelStyle
  of skLiteralValue:
    style = mixedCamelStyle
  of skOverloadableDef:
    style =
      if hasUnderscore: mixedCamelStyle # `GC_` in `gc_interface`
      elif s.len == 1:  atomStyle
      else:             lowerCamelStyle
  of skLocals:
    style =
      if s.len == 1:     atomStyle        # `L` in `lexer`
      elif k == skParam: mixedCamelStyle  # xxx: legacy?
      else:              lowerCamelStrict
  of skResult, skIgnore:
    unreachable("can never get here")

  var i = 0
  while i < s.len:
    if i == 0:    # first
      result.add:
        case style
        of upperCamelStyle, shoutingStyle:    toUpperAscii(s[0])
        of lowerCamelStyle, lowerCamelStrict: toLowerAscii(s[0])
        of mixedCamelStyle, atomStyle:        s[0]
    elif i+1 < s.len:
      case s[i]
      of '_':
        case style
        of upperCamelStyle, shoutingStyle, lowerCamelStyle, mixedCamelStyle:
          result.add '_'
          inc i
          result.add s[i]
        of lowerCamelStrict:
          inc i
          result.add toUpperAscii(s[i])
        of atomStyle:
          unreachable("cannot have more than one identifier")
      else:
        result.add s[i]
    else:         # last
      case s[i]
      of '_':   discard "ignore trailing underscores"
      else:     result.add s[i]
    inc i

proc differ(line: string, a, b: int, x: string): string =
  proc substrEq(s: string, pos, last: int, substr: string): bool =
    result = true
    for i in 0..<substr.len:
      if pos+i > last or s[pos+i] != substr[i]: return false

  result = ""
  if not substrEq(line, a, b, x):
    let y = line[a..b]
    if cmpIgnoreStyle(y, x) == 0:
      result = y

proc differs(conf: ConfigRef; info: TLineInfo; newName: string): string =
  let line = sourceLine(conf, info)
  var first = min(info.col.int, line.len)
  if first < 0: return
  #inc first, skipIgnoreCase(line, "proc ", first)
  while first > 0 and line[first-1] in Letters: dec first
  if first < 0: return
  if first+1 < line.len and line[first] == '`': inc first

  let last = first+identLen(line, first)-1
  result = differ(line, first, last, newName)

proc checkDefImpl(conf: ConfigRef; info: TLineInfo; s: PSym; k: TSymKind) =
  # operators stay as they are:
  if k in {skResult, skTemp} or s.name.s[0] notin Letters: return
  if k in {skType, skGenericParam} and sfAnon in s.flags: return
  if s.typ != nil and s.typ.kind == tyTypeDesc: return
  if {sfImportc, sfExportc} * s.flags != {}: return
  if k == skParam and {sfImportc, sfExportc} * s.owner.flags != {}: return
  if optStyleCheck notin s.options: return # xxx: invert this option so the
                                           #      default is to check
  let wanted = beautifyName(s.name.s, k)
  if s.name.s != wanted:
    conf.localReport(info, SemReport(
      sym: s,
      kind: rsemLinterReport,
      linterFail: (wanted, s.name.s)))

template styleCheckDef*(conf: ConfigRef; info: TLineInfo; s: PSym; k: TSymKind) =
  if {optStyleHint, optStyleError} * conf.globalOptions != {}:
    checkDefImpl(conf, info, s, k)

template styleCheckDef*(conf: ConfigRef; info: TLineInfo; s: PSym) =
  styleCheckDef(conf, info, s, s.kind)
template styleCheckDef*(conf: ConfigRef; s: PSym) =
  styleCheckDef(conf, s.info, s, s.kind)

proc styleCheckUse*(conf: ConfigRef; info: TLineInfo; s: PSym) =
  if info.fileIndex.int < 0: return
  # we simply convert it to what it looks like in the definition
  # for consistency

  # operators stay as they are:
  if s.kind == skTemp or s.name.s[0] notin Letters or sfAnon in s.flags:
    return

  let newName = s.name.s
  let badName = differs(conf, info, newName)
  if badName.len > 0:
    # special rules for historical reasons
    let forceHint =
      (badName == "nnkArgList" and newName == "nnkArglist") or
      (badName == "nnkArglist" and newName == "nnkArgList")
    conf.localReport(info, SemReport(
      sym: s,
      info: info,
      kind: rsemLinterReportUse,
      linterFail: (wanted: newName, got: badName)
    ))

proc checkPragmaUse*(conf: ConfigRef; info: TLineInfo; w: TSpecialWord; pragmaName: string) =
  let wanted = $w
  if pragmaName != wanted:
    conf.localReport(info, SemReport(
      kind: rsemLinterReport,
      linterFail: (wanted: wanted, got: pragmaName)
    ))
