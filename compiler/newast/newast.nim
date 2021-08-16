#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module is the new ast, it's an attempt to migrate the compiler towards
## a set of lowerings.
##
## Naming Conventions:
## * `legacy` related to bridging from old AST to the new one

from ".." / ast import TNodeKind, PNode, safeLen, `[]`, pairs
import data, data_ast, data_token

type
  ModuleAst* = distinct ModuleData

func id*(m: ModuleAst): ModuleId = m.ModuleData.id
func tokens*(m: var ModuleAst): var TokenList = m.ModuleData.tokens
func ast*(m: var ModuleAst): var Ast = m.ModuleData.ast

proc `$`*(m: ModuleAst): string {.inline.} =
  $m.ModuleData

proc initModuleAst*(id: ModuleId, hintAstLen = assumedAstLen): ModuleAst =
  ## initialize a ModuleAst, pre-allocate storage based on the `hintAstLen` to
  ## excessive allocations/moves.
  ModuleAst initModule(id, hintAstLen)

proc legacyInitModuleAst*(id: int): ModuleAst =
  ## creates `ModuleAst` with the legacy `id` of the module, in order to keep
  ## it in sync with the legacy `ModuleGraph`.
  initModuleAst(ModuleId id)

type
  NodeProcessingKind {.pure.} = enum
    ## xxx: the name leaves much to be desired -- to be renamed.
    ## The intention is to indicate further processing of child nodes based
    ## upon the kind. This further processing usually involves extra data, see:
    ## `Ast.extra` and `AstExtra`
    npkNoExtraData,
    npkIndexAllButFirstChild

proc legacyAppendPNode*(m: var ModuleAst; n: PNode): AstIndex =
  ## take `n` the `PNode`, from parsing, and append it to `m` the `ModuleAst`.
  ## tracks the `AstIndex` of various nodes being appended, this allows to have
  ## a depth first construction of the AST -- when traversed linearly.

  let
    kind = legacyNodeToAstKind(n)
    token = m.tokens.legacyAdd(n)
    nodeIdx = m.ast.reserveAstNode(kind, token)
    leftAstIdx = AstLeft m.ast.getNextAstIndex()
    (left, right, extraProcessing) = case kind
      of ankStmtList: (leftAstIdx, emptyAstRight, npkNoExtraData)
      of ankCallCmd:
        # Q: Why don't we store the callee's index?
        # A: command node is immediately followed by the callee ast node, we
        #    assume the next idx to be left and don't need to encode it, as
        #    it's implied by the `kind`
        let
          extraDataStart = m.ast.extra.len # where we start insert indices
          childCount = n.safeLen           # callee and then one or more args
          argsCount = childCount - 1       # total ast indices in extra data

        (AstLeft extraDataStart, AstRight argsCount, npkIndexAllButFirstChild)
      else:
        (emptyAstLeft, emptyAstRight, npkNoExtraData)

  m.ast[nodeIdx].left = left
  m.ast[nodeIdx].right = right

  for i, child in n.pairs:
    let
      firstChild = i == 0
      idx = m.legacyAppendPNode(child) # append the node

    # if we have to do extra process like remember extra data, do that here
    case extraProcessing
    of npkNoExtraData:
      # we already appended it, ignore everything else
      discard
    of npkIndexAllButFirstChild:
      # this is for cases like `ankCallCmd` where the first child is
      # immediately following and doesn't need to be indexed, but the args are
      # varying distances apart and indexed as extra data.
      if not firstChild:
        m.ast.extra.add idx

  result = nodeIdx


