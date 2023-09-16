#
#
#           The Nim Compiler
#        (c) Copyright 2017 Contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  std/[
    strutils,
    os,
  ],
  compiler/ast/[
    ast,
    lineinfos,
    renderer,
  ],
  compiler/utils/[
    pathutils,
  ],
  compiler/front/[
    msgs,
    options,
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import reportAst
from compiler/ast/reports_internal import InternalReport
from compiler/ast/report_enums import ReportKind

proc getModuleName*(conf: ConfigRef; n: PNode): string =
  # This returns a short relative module name without the nim extension
  # e.g. like "system", "importer" or "somepath/module"
  # The proc won't perform any checks that the path is actually valid
  case n.kind
  of nkStrLit, nkRStrLit, nkTripleStrLit:
    try:
      result = pathSubs(conf, n.strVal, toFullPath(conf, n.info).splitFile().dir)
    except ValueError:
      conf.localReport(n.info, reportAst(rsemInvalidModulePath, n))
      result = n.strVal

  of nkIdent:
    result = n.ident.s
  of nkSym:
    result = n.sym.name.s
  of nkInfix:
    let n0 = n[0]
    let n1 = n[1]
    let modname = getModuleName(conf, n[2])
    # hacky way to implement 'x / y /../ z':
    result = getModuleName(conf, n1)
    result.add renderTree(n0, {renderNoComments}).replace(" ")
    result.add modname
  of nkPrefix:
    # hacky way to implement 'x / y /../ z':
    result = renderTree(n, {renderNoComments}).replace(" ")
  of nkDotExpr:
    conf.localReport(n.info, reportAst(rsemDotForModuleImport, n))
    result = renderTree(n, {renderNoComments}).replace(".", "/")
  of nkImportAs:
    result = getModuleName(conf, n[0])
  else:
    conf.localReport(n.info, reportAst(rsemInvalidModuleName, n))
    result = ""

proc checkModuleName*(conf: ConfigRef; n: PNode; doLocalError=true): FileIndex =
  # This returns the full canonical path for a given module import
  let modulename = getModuleName(conf, n)
  let fullPath = findModule(conf, modulename, toFullPath(conf, n.info))
  if fullPath.isEmpty:
    if doLocalError:
      let m = if modulename.len > 0: modulename else: $n
      conf.localReport(n.info, InternalReport(
        kind: rintCannotOpenFile, file: m))
    result = InvalidFileIdx
  else:
    result = fileInfoIdx(conf, fullPath)
