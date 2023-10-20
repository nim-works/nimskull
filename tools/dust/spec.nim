import std/strutils
import std/sets

import
  compiler / modules / modulegraphs,
  compiler / ast / [ lineinfos, renderer, ast ]

import hashing

type
  Attempt = object
    node: PNode
    sig: SigHash
    size: int

  Remains* = object
    signatures: HashSet[SigHash]
    attempts: seq[Attempt]

  DustContext* = ref object of PPassContext
    mainIndex*: FileIndex
    ignore*: bool

proc len*(r: Remains): int = len(r.attempts)

proc count*(r: Remains): int = len(r.signatures)

proc pop*(remains: var Remains): PNode =
  assert len(remains) > 0, "pop from empty remains"
  result = pop(remains.attempts).node

proc size*(n: PNode): int =
  assert not n.isNil
  result = 1
  if n.kind in nkWithSons:
    for child in items(n.sons):
      inc result, size(child)

proc contains*(remains: Remains; h: SigHash): bool =
  h in remains.signatures

proc contains*(remains: Remains; n: PNode): bool =
  hashNode(n) in remains

proc newAttempt(n: PNode): Attempt =
  var n = copyTree(n)
  result = Attempt(node: n, sig: hashNode(n), size: size(n))

proc newAttempt(n: PNode; sig: SigHash): Attempt =
  var n = copyTree(n)
  assert hashNode(n) == sig
  result = Attempt(node: n, sig: sig, size: size(n))

proc add(remains: var Remains; a: Attempt) =
  if a.size > 1:
    if not containsOrIncl(remains.signatures, a.sig):
      remains.attempts.add a

proc add*(remains: var Remains; n: PNode) =
  if not n.isNil:
    remains.add newAttempt(n)

proc add*(remains: var Remains; n: PNode; sig: SigHash) =
  if not n.isNil:
    remains.add newAttempt(n, sig)

proc next*(remains: Remains): PNode =
  if len(remains) > 0:
    result = remains.attempts[^1].node

proc massageMessage*(s: string): string =
  result = s.splitLines()[0]
  for c in [';', ':']:
    let i = find(result, c)
    if i != -1:
      result = result[0 .. i - 1]

export `$`