discard """
description: '''
Shows Nimskull's partial case sensitivity at play:
- first letter of an identifier is matched as case sensitive
- remaining are not
- underscores are ignored
'''
"""

# Nimskull's identifier match can be understood as:
# ```
# proc sameIdentifier(a, b: string): bool =
#   a[0] == b[0] and
#     a.replace("_", "").toLowerAscii == b.replace("_", "").toLowerAscii
# ```

doAssert true and true == tRuE an_D t_rue,
  "after first character casing changes and adding underscores (`_`) works"
