#
#
#           The Nim Compiler
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the generation of ``.ndi`` files for better debugging
## support of Nim code. "ndi" stands for "Nim debug info".

import
  compiler/ast/[
    ast,
    lineinfos
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/utils/[
    ropes,
    pathutils
  ]

type
  Mapping = tuple
    info: TLineInfo ## the source code position where the symbol is defined
    orig: PIdent ## the user-defined name
    name: string ## the mangled name

  NdiFile* = object
    enabled: bool
    f: File
    buf: string
    filename: AbsoluteFile
    mappings: seq[Mapping]

proc doWrite(f: var NdiFile; m: Mapping, conf: ConfigRef) =
  f.buf.setLen 0
  f.buf.addInt m.info.line.int
  f.buf.add "\t"
  f.buf.addInt m.info.col.int
  f.f.write(m.orig.s, "\t")
  f.f.write(m.name)
  f.f.writeLine("\t", toFullPath(conf, m.info), "\t", f.buf)

template writeMangledName*(f: NdiFile; info: TLineInfo, orig: PIdent, n: string,
                           conf: ConfigRef) =
  ## If `f` is enabled, registers a symbol-to-name mapping entry where
  ## `info` is the definition's source position, `orig` the user-provided
  ## symbol name, and `n` the mangled name. Nothing is written to disk yet.
  if f.enabled: f.mappings.add (info, orig, n)

template writeMangledName*(f: NdiFile; s: PSym; n: string, conf: ConfigRef) =
  ## Same as the other ``writeMangledName`` overload, but takes the
  ## ``TLineInfo`` and ``PIdent`` from the symbol `s`.
  writeMangledName(f, s.info, s.name, n, conf)

proc open*(f: var NdiFile; filename: AbsoluteFile; conf: ConfigRef) =
  f.enabled = not filename.isEmpty
  if f.enabled:
    f.filename = filename
    f.buf = newStringOfCap(20)

proc close*(f: var NdiFile, conf: ConfigRef) =
  if f.enabled:
    f.f = open(f.filename.string, fmWrite, 8000)
    doAssert f.f != nil, f.filename.string
    for m in f.mappings.items:
      doWrite(f, m, conf)
    close(f.f)
    f.mappings.reset
    f.filename.reset
