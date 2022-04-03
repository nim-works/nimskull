discard """
  errormsg: "'testEpo' can have side effects"
  target: "js"
  knownIssue: '''JS time procs aren't tagged with `TimeEffect`'''
  line: 10
"""

import times

func testEpo(x: float): float = epochTime() + x

echo testEpo(1.0)
