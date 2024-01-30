discard """
  matrix: "--warningAsError:UnknownHint"
  errormsg: "unknown hint: 'Something' [UnknownHint]"
  line: 7
"""

{.push hint[Something]: on.}
{.pop.}