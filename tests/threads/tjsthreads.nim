discard """
  targets: "c !cpp js"
  matrix: "--threads"
"""

# xxx: this should work in CPP, it's a knownIssue, see PR:
#      https://github.com/nim-works/nimskull/pull/290

echo 123
