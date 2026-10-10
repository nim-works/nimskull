discard """
  description: "A signature type may declare zero routines to bind"
"""

type Sig = (signature(Self) do:
  discard
)

var x = Sig(0)
var y = Sig(0.0)
