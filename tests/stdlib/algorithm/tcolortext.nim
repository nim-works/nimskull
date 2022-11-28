discard """
  targets: "c"
"""

import experimental/colortext
import std/strutils

func reesc(text: string): string =
  replace(text, "\e", "\\e")



proc main() =
  proc check(arg: ColText | ColRuneGrid, str: string) =
    doAssert reesc($arg) == str, "\n" & reesc($arg) & "\n!=\n" & str

  check("@" + fgDefault, "@")
  check("red" + fgRed, r"\e[31mred\e[0m")
  check("italic" + styleItalic, r"\e[3mitalic\e[0m")
  check("underline" + styleUnderscore, r"\e[4munderline\e[0m")
  check(
    "ita" + styleItalic & "under" + styleUnderscore,
    r"\e[3mita\e[23m" & r"\e[4munder\e[0m"
  )

  check("red" + fgRed & "none", r"\e[31mred\e[39mnone")

  block:
    var grid = grid("let exp: int" + fgDefault)
    grid[1, len("let ")] = """
^~~
`let` requires initalization""" + fgRed

    check(grid):
      """
let exp: int
    \e[31m^~~\e[0m
    \e[31m`let` requires initalization\e[0m"""

  block:
    var buf: ColText
    buf.addIndent(2)
    check buf, "    "

  proc colres(): ColText =
    coloredResult()
    add "["
    addIndent(2)
    add "0"
    add "1" + fgRed
    addi 2, "2"
    add "]"
    endResult()

  let col = reesc($colres())
  let want = r"[    0\e[31m1\e[39m    2]"
  doAssert col == want

static: main()
main()
