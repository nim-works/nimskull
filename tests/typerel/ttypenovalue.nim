discard """
  targets: native
  errormsg: "invalid type: 'typedesc[seq[tuple[title: string, body: string]]]' for var"
  line: 8
"""

proc crashAndBurn() =
  var stuff = seq[tuple[title, body: string]]


crashAndBurn()
