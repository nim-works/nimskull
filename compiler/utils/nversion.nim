#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module contains Nim's version. It is the only place where it needs
# to be changed.

import std/[strscans, strutils]

type
  Version* = object
    ## An object describing the compiler version
    suffix*: string ## Optional suffix
    major*, minor*, patch*: int

func isValidSuffix(s: string): bool =
  ## Return whether `s` represents a valid version suffix.
  # The validation rules are derived from SemVer's grammar, albeit relaxed.
  result = true
  if s.len > 0 and s[0] notin {'+', '-'}:
    return false

  for ch in s.items:
    if ch notin {'a'..'z', 'A'..'Z', '0'..'9', '+', '-', '.'}:
      return false

func `$`*(v: Version): string =
  ## Return a string describing `v`.
  result.addInt v.major
  result.add '.'
  result.addInt v.minor
  result.add '.'
  result.addInt v.patch
  result.add v.suffix

func parse*(s: string): Version =
  ## Parse the string `s` to create `Version`.
  if not scanf(
    s, "$i.$i.$i$*$.",
    result.major, result.minor, result.patch, result.suffix
  ) or not result.suffix.isValidSuffix:
    raise newException(ValueError):
      "Invalid version string: " & s.escape()

const CompilerVersionSuffix* {.strdefine.} = ""
  ## The suffix to attach to the compiler version. This is meant to be
  ## declared by build tools to signify development version for example.

proc parseStatic(): Version {.compileTime.} =
  ## Obtain the compiler version from the version file.
  result = parse(staticRead("../version.txt").strip())
  doAssert result.suffix == "", "Compiler version in `version.txt' should not have any suffix"
  doAssert isValidSuffix(CompilerVersionSuffix)
  result.suffix = CompilerVersionSuffix

const
  MaxSetElements* = 1 shl 16  # (2^16) to support unicode character sets?
  CompilerVersion* = parseStatic()
    ## The compiler version.
  VersionAsString* = $CompilerVersion
  RodFileVersion* = "1223"       # modify this if the rod-format changes!
  NimCompilerApiVersion* = 3 ## Check for the existence of this before accessing it
                             ## as older versions of the compiler API do not
                             ## declare this.
