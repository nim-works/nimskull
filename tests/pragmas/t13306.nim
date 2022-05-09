discard """
  errormsg: "'testEpo' can have side effects"
  targets: "c cpp"
  line: 9
"""

import times

func testEpo(x: float): float = epochTime() + x

echo testEpo(1.0)
