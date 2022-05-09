#
#
#           The Nim Compiler
#        (c) Copyright 2018 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements helpers for the macro cache.

import
  compiler/ast/[
    lineinfos,
    ast
  ],
  compiler/vm/[
    vmdef
  ]


func append(c: var TCtx; n: PNode) =
  c.vmstateDiff.add((c.module, n))

proc recordInc*(c: var TCtx; info: TLineInfo; key: string; by: BiggestInt) =
  var recorded = newNodeI(nkReplayAction, info)
  recorded.add newStrNode("inc", info)
  recorded.add newStrNode(key, info)
  recorded.add newIntNode(nkIntLit, by)
  c.append(recorded)

proc recordPut*(c: var TCtx; info: TLineInfo; key: string; k: string; val: PNode) =
  var recorded = newNodeI(nkReplayAction, info)
  recorded.add newStrNode("put", info)
  recorded.add newStrNode(key, info)
  recorded.add newStrNode(k, info)
  recorded.add copyTree(val)
  c.append(recorded)

proc recordAdd*(c: var TCtx; info: TLineInfo; key: string; val: PNode) =
  var recorded = newNodeI(nkReplayAction, info)
  recorded.add newStrNode("add", info)
  recorded.add newStrNode(key, info)
  recorded.add copyTree(val)
  c.append(recorded)

proc recordIncl*(c: var TCtx; info: TLineInfo; key: string; val: PNode) =
  var recorded = newNodeI(nkReplayAction, info)
  recorded.add newStrNode("incl", info)
  recorded.add newStrNode(key, info)
  recorded.add copyTree(val)
  c.append(recorded)
