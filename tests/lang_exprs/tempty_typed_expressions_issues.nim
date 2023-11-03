discard """
  targets: "c js vm"
  knownIssue: '''
    `typeRel` treats an empty array matching against a concrete type as a
    generic match, resulting in no implicit conversion being injected (the
    conversion is required for the proper typing of statement-list
    expressions)
  '''
"""

# XXX: merge into ``tempty_typed_expressions.nim`` once the issue is fixed

proc get(x: array[0, int]): array[0, int] = x

discard get((discard; []))