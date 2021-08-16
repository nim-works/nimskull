#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains the core memory/data types for a normalized AST, which
## is very close the the current AST, it does not include syntatic sugar and
## supports grammar attributes.
## 
## This is all part of an attempt to migrate the compiler towards a set of
## lowerings.

import data_ast

type
  NormAst* = object
    ## normalized AST for some AST
    nodes*: seq[NormNode]
  
  NormNode* = object
    ## xxx: turn this into struct-of-arrays
    ##
    ## A normalized AST node, this is similar to the AST grammar, but contains
    ## more precise information and support for attributes.
    ## 
    ## For more on attribute grammars, see:
    ## https://en.wikipedia.org/wiki/Attribute_grammar
    kind: NormNodeKind
      ## the node this represents, more precise version of the written grammar.
    ast: AstIndex
      ## origin AST node that this was derived from
    left: NormLeft
      ## interpretation depends upon `kind`, typically this is an index for
      ## looking up the left child index in the list of `NormNode`.
    right: NormRight
      ## interpretation depends upon `kind`, typically this is an index for
      ## looking up the right child index in the list of `NormNode`.
    # inherit: AttributeSpan
    #   ## derived during the parent node's expansion phase
    # synthesize: AttributeSpan
    #   ## derived during a child node's reduction phase

  NormIndex* = distinct int32
    ## used to point to additional information for an AST node
  NormLeft* = distinct uint32
    ## might be an `NormIndex`, an int value, or interpretted some other way
    ## based on the `NormNode.kind`
  NormRight* = distinct uint32
    ## might be an `NormIndex`, an int value, or interpretted some other way
    ## based on the `NormNode.kind`

  AttributeSpan = object
    start: AttributeIndex
    done: AttributeIndex

  NormNodeKind* {.pure.} = enum # xxx: prefix with `nnk` what could go wrong?
    nnkError,
    nnkEmpty,

  AttributeIndex = distinct uint32

const
  assumedNormLen*: Natural = 100
    ## until we have a heurisitic this is a "reasonable" norm reservation to
    ## assume in order to avoid excessive allow/copy/move operations
  emptyNormLeft* = NormLeft 0
    ## empty based on zero initialization of memory
  emptyNormRight* = NormRight 0
    ## empty based on zero initialization of memory
  unknownNormLeft* = emptyNormLeft
    ## empty and unkonwn are ambiguous, aliased to communicate intent
  unknownNormRight* = emptyNormRight
    ## empty and unkonwn are ambiguous, aliased to communicate intent

proc initNormNode*(kind: NormNodeKind, ast: AstIndex): NormNode {.inline.} =
  ## create a normalized node with only `kind` and `ast` initialized.
  NormNode(
    kind: kind,
    ast: ast,
    left: emptyNormLeft,
    right: emptyNormRight
  )

proc `[]`*(n: var NormAst; i: AstIndex): var NormNode =
  ## get the `NormNode` corresponding to the `NormIndex` `i`.
  # since `i` is discriminated by type, we know what to index against, fun!
  n.nodes[i.uint32]

proc `$`*(i: NormLeft): string {.borrow.}
proc `$`*(i: NormRight): string {.borrow.}
proc `$`*(i: NormIndex): string {.borrow.}

proc `==`*(a, b: NormIndex): bool {.borrow.}
proc `==`*(a, b: NormLeft): bool {.borrow.}
proc `==`*(a, b: NormRight): bool {.borrow.}

func getNextNormIndex*(n: NormAst): NormIndex =
  ## gets the `NormIndex` that would be generated if a node was inserted
  NormIndex n.nodes.len

proc reserveNormNode*(
    norm: var NormAst;
    kind: NormNodeKind,
    ast: AstIndex
  ): NormIndex =
  ## append a norm node based on `kind` and `ast`, with `left` and `right` to
  ## be updated later, as they're often unknown until further processing and
  ## addition of norm nodes.
  
  norm.nodes.add initNormNode(kind, ast)

  result = NormIndex norm.nodes.len - 1