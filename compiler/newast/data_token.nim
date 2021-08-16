#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains the tokenization memory/data types, it's part of an
## attempt to migrate the compiler towards a set of lowerings. Presently,
## it's focused on bridging AST parsing from the legacy compiler.
##
## Naming Conventions:
## * `legacy` related to bridging from old AST to the new one

from ".." / ast import PNode

type
  TokenIndex* = distinct int32
    ## used to point to a token from the token list

  TokenList* = object
    ## list of tokens, typically for an entire module or fragment of code
    list: seq[Token]

  Token* = object
    ## xxx: should be a single token in a token list from tokenization.
    ##      instead it shims info between the old `ast.TNode` and the new AST.
    node: PNode

const assumedTokenLen*: Natural = 100

proc initTokenList*(hintListLen = assumedTokenLen): TokenList =
  ## initialize a `TokenList`, pre-allocate storage based on the 'hintListLen`
  TokenList(list: newSeqOfCap[Token](hintListLen))

proc legacyAdd*(tokens: var TokenList, n: PNode): TokenIndex =
  ## add this `PNode` from the parser to the list of tokens (yess very weird)
  ## and return the `TokenIndex` corresponding to the fresh token.
  tokens.list.add Token(node: n)
  result = TokenIndex(tokens.list.len - 1)

proc `$`*(i: TokenIndex): string {.borrow.}

proc `==`*(a, b: TokenIndex): bool {.borrow.}