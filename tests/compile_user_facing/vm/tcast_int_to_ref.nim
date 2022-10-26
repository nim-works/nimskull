discard """
  description: "Casting an int to a ref is disallowed in the VM (for now)"
  action: reject
  errormsg: "VM does not support 'cast' from tyInt to tyRef"
  knownIssue: '''`transf` transforms away the `cast` for literals, preventing
                 the check in vmgen to trigger'''
"""

static:
  let x = cast[ref string](1)