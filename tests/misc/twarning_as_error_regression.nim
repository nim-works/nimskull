discard """
  description: '''
    Regression test for warnings and hints promoted to errors counting as
    reported (and thus leading to compilation aborting) even if disabled
  '''
  matrix: "--warning:Deprecated:on --warningAsError:Deprecated --hint:XDeclaredButNotUsed:on --hintAsError:XDeclaredButNotUsed"
  action: compile
"""

{.push hint[XDeclaredButNotUsed]:off.}
block:
  var x = 0 # must not quit the compiler
{.pop.}

proc p() {.deprecated, used.} =
  discard

{.push warning[Deprecated]:off.}
p() # must not quit the compiler
{.pop.}