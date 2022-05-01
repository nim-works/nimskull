discard """
  targets: "cpp"
  matrix: "--threads:on"
"""

# bug #5142

var ci: Channel[int]
ci.open
