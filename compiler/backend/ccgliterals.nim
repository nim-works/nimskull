#
#
#           The Nim Compiler
#        (c) Copyright 2018 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from cgen.nim

## This include file contains the logic to produce constant string
## and seq literals. The code here is responsible that
## ``const x = ["a", "b"]`` works without hidden runtime creation code.
## The price is that seqs and strings are not purely a library
## implementation.

# ------ Version 2: destructor based strings and seqs -----------------------

proc genStringLiteralDataOnlyV2(m: BModule, s: string; result: Rope; isConst: bool) =
  m.s[cfsData].addf("static $4 struct {$n" &
       "  NI cap; NIM_CHAR data[$2+1];$n" &
       "} $1 = { $2 | NIM_STRLIT_FLAG, $3 };$n",
       [result, rope(s.len), makeCString(s),
       rope(if isConst: "const" else: "")])

proc genStringLiteralV2(m: BModule; n: PNode; isConst: bool): Rope =
  let id = getOrPut(m.dataCache, n, m.labels)
  if id == m.labels:
    let pureLit = getTempName(m)
    genStringLiteralDataOnlyV2(m, n.strVal, pureLit, isConst)
    result = getTempName(m)
    discard cgsym(m, "NimStrPayload")
    discard cgsym(m, "NimStringV2")
    # string literal not found in the cache:
    m.s[cfsData].addf("static $4 NimStringV2 $1 = {$2, (NimStrPayload*)&$3};$n",
          [result, rope(n.strVal.len), pureLit, rope(if isConst: "const" else: "")])
  else:
    result = getTempName(m)
    m.s[cfsData].addf("static $4 NimStringV2 $1 = {$2, (NimStrPayload*)&$3};$n",
          [result, rope(n.strVal.len), m.tmpBase & rope(id),
          rope(if isConst: "const" else: "")])

proc genStringLiteralV2Const(m: BModule; n: PNode; isConst: bool): Rope =
  let id = getOrPut(m.dataCache, n, m.labels)
  var pureLit: Rope
  if id == m.labels:
    pureLit = getTempName(m)
    discard cgsym(m, "NimStrPayload")
    discard cgsym(m, "NimStringV2")
    # string literal not found in the cache:
    genStringLiteralDataOnlyV2(m, n.strVal, pureLit, isConst)
  else:
    pureLit = m.tmpBase & rope(id)
  result = "{$1, (NimStrPayload*)&$2}" % [rope(n.strVal.len), pureLit]

# ------ Version selector ---------------------------------------------------

proc genNilStringLiteral(m: BModule; info: TLineInfo): Rope =
  result = ropecg(m, "((#NimStringDesc*) NIM_NIL)", [])

proc genStringLiteral(m: BModule; n: PNode): Rope =
  result = genStringLiteralV2(m, n, isConst = true)