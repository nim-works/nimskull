discard """
  matrix: "--warningAsError:UnknownWarning"
  errormsg: "unknown warning: 'Something' [UnknownWarning]"
  line: 7
"""

{.push warning[Something]: on.}
{.pop.}