from std/options import Option
from std/json import JsonNodeObj

## Follows TAP 14 protocol: https://testanything.org/tap-version-14-specification.html
##
## based on the "suggested" EBNF of:
##
## TAPDocument := Version Plan Body | Version Body Plan
## Version     := "TAP version 14\n"
## Plan        := "1.." (Number) (" # " Reason)? "\n"
## Body        := (TestPoint | BailOut | Pragma | Comment | Anything | Empty | Subtest)*
## TestPoint   := ("not ")? "ok" (" " Number)? ((" -")? (" " Description) )? (" " Directive)? "\n" (YAMLBlock)?
## Directive   := " # " ("todo" | "skip") (" " Reason)?
## YAMLBlock   := "  ---\n" (YAMLLine)* "  ...\n"
## YAMLLine    := "  " (YAML)* "\n"
## BailOut     := "Bail out!" (" " Reason)? "\n"
## Reason      := [^\n]+
## Pragma      := "pragma " [+-] PragmaKey "\n"
## PragmaKey   := ([a-zA-Z0-9_-])+
## Subtest     := ("# Subtest" (": " SubtestName)?)? "\n" SubtestDocument TestPoint
## Comment     := ^ (" ")* "#" [^\n]* "\n"
## Empty       := [\s\t]* "\n"
## Anything    := [^\n]+ "\n"

type
  TAPDoc = object
    subtest: Subtest
  
  Subtest = object
    plan: Plan
    body: Body

  Plan = object
    number: Option[uint]
    reason: Option[string]

  BodyPartKind = enum
    bpkTestPoint
    bpkBailOut
    bpkPragma
    bpkComment
    bpkAnything
    bpkEmpty
    bpkSubtest

  BodyPart = object
    case kind: BodyPartKind
    of bpkTestPoint:
      notOk: bool
      number: Option[uint]
      description: string
      directive: Option[Directive]
      yamlBlk: Option[YamlBlock]
    of bpkBailOut:
      reason: string
    of bpkPragma:
      add: bool
      key: string
    of bpkComment, bpkAnything:
      str: string
    of bpkEmpty:
      discard
    of bpkSubtest:
      name: string
      subtest: Subtest # TODO: subtests support fewer parts

  Body = object
    parts: seq[BodyPart]

  DirectiveKind = enum
    dkSkip
    dkTodo

  Directive = object
    kind: DirectiveKind
    reason: string

  YamlBlock = JsonNodeObj
    # TODO: handle with a yaml parser in std

