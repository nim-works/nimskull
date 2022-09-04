## Data structure for the parser results

import
  compiler/ast/[
    ast_types, # For the node kinds
    lexer # For the token type definition
  ]

# NOTE further refactoring considerations for the parser:
#
# - store everything in tokens, do not require identifier interning for any
#   purposes during the parsing stage, it must be done later, during
#   conversion to a PNode. This will simplify some parts of the type
#   definition.
# - remove nim"pretty" - this is an absolute joke of implementation and
#   it should not be placed where it is now.

type
  ParsedNode* = ref object
    # NOTE next two fields are very large combined, but further plans will
    # deal with that problem - current implementation is easier to write
    # and it is just a transition point.
    info*: TLineInfo # TODO replace line and separate token with index to
                     # the token, which in turn will store information
                     # about global positioning (tuple made up of a token
                     # id and a file ID)
                     #
                     # NOTE technically this is not really necessary even
                     # with the current implementation, but the parser
                     # consistently copies this information around anyway,
                     # so I will let it stay this way for now.
    token*: Token # TODO Replace full token value with an index information
    kind*: TNodeKind # NOTE/QUESTION - for now the same kind of nodes is
                     # reused as the main parser, to ease the transition,
                     # but in the future two different sets of node kinds
                     # might(?) be introduced.

    # TODO replace `ref` object tree with begin/end ranges for the nested
    # trees in the linearized structure.
    sons*: seq[ParsedNode]
    comment*: string # TODO this should either be a token or a sequence of
                     # tokens.

    # HACK explicit flags in order to track down all 'extra' information
    # that is collected during parsing.
    isBlockArg*: bool # QUESTION add 'nkStmtListBlockArg' or similar node
                      # and convert it to the `nkStmtList` + `nfBlocArg`
                      # flags later on? Why do we need the `nfBlockArg`
                      # flag in the first place?

func len*(node: ParsedNode): int =
  ## Number of sons of the parsed node
  return node.sons.len()

# NOTE added for the sake of API similarity between PNode
proc safeLen*(node: ParsedNode): int = node.len()

proc `[]`*(node: ParsedNode, idx: int | BackwardsIndex): ParsedNode =
  return node.sons[idx]

proc `[]=`*(node: ParsedNode, idx: int | BackwardsIndex, other: ParsedNode) =
  node.sons[idx] = other

iterator items*(node: ParsedNode): ParsedNode =
  for item in node.sons.items():
    yield item

iterator pairs*(node: ParsedNode): (int, ParsedNode) =
  for idx, item in pairs(node.sons):
    yield (idx, item)

proc add*(node: ParsedNode, other: ParsedNode) =
  ## append `other` to `node`'s `sons`
  node.sons.add(other)

proc transitionSonsKind*(n: ParsedNode, kind: TNodeKind) =
  n.kind = kind

proc transitionIntKind*(n: ParsedNode, kind: TNodeKind) =
  n.kind = kind

proc transitionNoneToSym*(n: ParsedNode) =
  n.kind = nkSym

func newParsedNode*(kind: TNodeKind): ParsedNode =
  ## Create a new parsed node without any location or token information
  return ParsedNode(kind: kind, info: unknownLineInfo)

func newParsedNode*(
  kind: TNodeKind, info: TLineInfo, sons: seq[ParsedNode] = @[]): ParsedNode =
  ## Create a new non-leaf parsed node with a specified location
  ## information and sons.
  return ParsedNode(kind: kind, info: info, sons: sons)

func newParsedNode*(kind: TNodeKind, info: TLineInfo, token: Token): ParsedNode =
  ## Create a new leaf parsed node with the specified location information
  ## and token kind.
  return ParsedNode(kind: kind, info: info, token: token)


proc newProcNode*(
    kind: TNodeKind,
    info: TLineInfo,
    body, params, name, pattern, genericParams,
    pragmas, exceptions: ParsedNode
  ): ParsedNode =

  result = newParsedNode(
    kind,
    info,
    @[name, pattern, genericParams, params, pragmas, exceptions, body])
