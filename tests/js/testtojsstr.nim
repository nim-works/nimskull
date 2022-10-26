discard """
  targets: "js"
  output = "И\n"
"""

let s: string = "И\n"
let cs = s.cstring

echo $s
