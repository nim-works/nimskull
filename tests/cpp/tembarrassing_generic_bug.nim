discard """
  targets: "cpp"
  matrix: "--threads:on"
  knownIssue: '''this should work in CPP, see PR:
https://github.com/nim-works/nimskull/pull/290
'''
"""

# bug #5142

var ci: Channel[int]
ci.open
