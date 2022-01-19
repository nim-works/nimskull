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

import std/strscans

type
  Version* = object
    ## An object describing the compiler version
    suffix*: string ## Optional suffix
    major*, minor*, patch*: int

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
  ):
    raise newException(ValueError):
      "Invalid version string: " & s

const
  MaxSetElements* = 1 shl 16  # (2^16) to support unicode character sets?
  CompilerVersionSuffix* {.strdefine.} = ""
    ## The suffix to attach to the compiler version. This is meant to be
    ## declared by build tools to signify development version for example.
  CompilerVersion* = Version(
    major: 0, minor: 1, patch: 0,
    suffix: CompilerVersionSuffix
  )
    ## The compiler version.
  VersionAsString* = $CompilerVersion
  RodFileVersion* = "1223"       # modify this if the rod-format changes!
  NimCompilerApiVersion* = 3 ## Check for the existence of this before accessing it
                             ## as older versions of the compiler API do not
                             ## declare this.
