discard """
  description: "static string bounds error (https://github.com/nim-works/nimskull/issues/1604)"
  errormsg: "index 1 not in 0 .. 0"
  line: 7
"""

discard "a"[1]
