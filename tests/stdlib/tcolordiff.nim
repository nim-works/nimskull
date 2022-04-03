discard """
  targets: "c cpp"
"""

import experimental/[diff, colordiff, colortext]
import std/[strformat, strutils]

# Configure diff formattter to use no unicode or colors to make testing
# easier. Each inserted/deleted chunk is annotated with `D/I/R/K` for the
# Delete/Insert/Replace/Keep respectively, and wrapped in the `[]`. Line
# split is done on each whitespace and elements are joined using `#`
# character.
#
# These test compare formatted edit operations - for now I've decided it
# is not necessary to factor out it out so much that we would be
# comparing raw layout data here (and considering most formatter procs
# already accept semi-ready input it would be a quite pointless
# indirection only for the purposes of testing)
#
# The strings here do not represent typical (default) formatting of the
# diff - formatting hooks were overriden to make the blocks more explicit
# instead.
var conf = diffFormatter(false)
conf.inlineDiffSeparator = clt("#")
conf.formatChunk = proc(
  word: string, mode, secondary: SeqEditKind, inline: bool
): ColText =
  if inline:
    # Configure inline message diffing separately
    if secondary == sekDelete:
       &"[R/<{word}>-" + fgDefault
    else:
       &"<{word}>]" + fgDefault
  else:
    &"[{($mode)[3]}/{word}]" + fgDefault

proc diff(a, b: string, sideBySide: bool = false): string =
  conf.sideBySide = sideBySide
  return formatDiffed(a, b, conf).toString(false)

proc ediff(a, b: string): string =
  formatInlineDiff(a, b, conf).toString(false)

proc ldiff(a, b: string): (string, string) =
  let (old, new) = formatLineDiff(a, b, conf)
  return (old.toString(false), new.toString(false))

doAssert not hasInvisible(" a")
doAssert hasInvisible("a ")
doAssert hasInvisible("a \n")
doAssert not hasInvisible("a a")
doAssert hasInvisible("a\n")

proc assertEq(found, expected: string) =
  if found != expected:
    assert false, &"expected:\n{expected}\nfound:\n{found}\ndiff:\n{diffText(expected, found, true)}"

proc assertEq(lhs, rhs: (string, string)) =
  assertEq(lhs[0], rhs[0])
  assertEq(lhs[1], rhs[1])

diff("a", "b").assertEq:
  """
[D/- ][R/a]
[I/+ ][R/b]"""
  # `-` and `+` are formatted as delete/insert operations, `a` is formatted
  # as `Replace`

diff("a b", "b b").assertEq:
  """
[D/- ][R/a]#[K/ ]#[K/b]
[I/+ ][R/b]#[K/ ]#[K/b]"""
# `Keep` the space and last `b`, replace first `a -> b`. Space is in
# the middle of the diff, so it is not considered 'invisible' and not
# highlighted explicitly.

diff("", "\n", true).assertEq:
  """
[K/~ ][K/]   [K/~ ][K/][I/[LF]]
[N/? ]       [I/+ ][I/]"""

# Keep the empty line. `[LF]` at the end is not considered for diff
# since it used to *separate* lines, but it is annotated as a
# difference.

diff("a ", "a").assertEq:
  """
[D/- ][K/a]#[D/[SPC]]
[I/+ ][K/a]"""
# Deleted trailing space

# Missing leading whitespace is not considered an 'invisible' character
# for both regular and line diffs.
diff(" a", "a", true).assertEq("[D/- ][D/ ]#[K/a]   [I/+ ][K/a]")
ldiff(" a", "a").assertEq(("[D/ ]#[K/a]", "[K/a]"))
# Intermediate whitespace is not invisible as well
ldiff("a a", "a").assertEq(("[D/a]#[D/ ]#[K/a]", "[K/a]"))
# Trailing whitespace IS invisible
ldiff("a ", "a").assertEq(("[K/a]#[D/[SPC]]", "[K/a]"))

# Control characters ARE invisible, regardless of their position in the
# text, so they are explicitly shown in diffs
ldiff("\ea", "a").assertEq(("[R/[ESC]a]", "[R/a]"))

# Inline edit diff annotations - for spelsuggest, invalid CLI switches,
# misspelled words, spell annotations, high-granularity diff suggestions.
ediff("a", "b").assertEq("[R/<a>-<b>]") # Replace 'a' with 'b'

# Replace first 'a', delete second one. Edit streaks are grouped
ediff("a a", "b").assertEq("[R/<a>-<b>]#[D/ a]")
# Elements between blocks are joined with `#` character, just like
# regular inline diff elements
ediff("w o r d", "w e r d").assertEq("[K/w ]#[R/<o>-<e>]#[K/ r d]")

conf.maxUnchanged = 2

diff("""
*
*
*
*
^
*
*
*
""", """
*
*
*
*
&
*
*
*
""").assertEq("""
[K/~ ][K/*]
[K/~ ][K/*]
[D/- ][R/^]
[I/+ ][R/&]
[K/~ ][K/*]
[K/~ ][K/*]""")

# Show only two unchanged lines before/after the change

conf.maxUnchangedWords = 1
ldiff(
  "@ @ @ @ @ @ @ @ @ @ @ @",
  "@ @ @ @ @ ! @ @ @ @ @ @",
).assertEq((
  # show 'Keep' `@` and `@` for one element around the edit operations,
  # discard everything else.
  "[K/@]#[K/ ]#[R/@]#[K/ ]#[K/@]",
  "[K/@]#[K/ ]#[R/!]#[K/ ]#[K/@]"
))

diff("\n", "").assertEq("""
[K/~ ][K/][D/[LF]]
[D/- ][D/]""")
# The line itself was modified but the newline character at the end was
# removed. This change is not considered as an edit operation

diff("", "\n", true).assertEq("""
[K/~ ][K/]   [K/~ ][K/][I/[LF]]
[N/? ]       [I/+ ][I/]""")

# Inserted newline is not a diff /directly/ as well - the *next* line
# that was modified (inserted). But new trailing newline is shown here

block unified:
  # Test different modes of grouping for unified diffs
  conf.groupLine = false
  diff("""
old
old
old""", """
new
new
new""").assertEq("""
[D/- ][R/old]
[I/+ ][R/new]
[D/- ][R/old]
[I/+ ][R/new]
[D/- ][R/old]
[I/+ ][R/new]""")

  conf.groupLine = true
  diff("""
old
old
old""", """
new
new
new""").assertEq("""
[D/- ][R/old]
[D/- ][R/old]
[D/- ][R/old]
[I/+ ][R/new]
[I/+ ][R/new]
[I/+ ][R/new]""")

  diff("""
old
old
keep
old""", """
new
new
keep
new""").assertEq("""
[D/- ][R/old]
[D/- ][R/old]
[I/+ ][R/new]
[I/+ ][R/new]
[K/~ ][K/keep]
[D/- ][R/old]
[I/+ ][R/new]""")


if false:
  # Debugging rid setup. Code needs to compile, but running not a part of
  # the test.

  echo diff("""
  (User :str "User Hint" :location ("tfile.nim" 8 _))""", """
  (User :severity Hint :str "User hint" :location ("tfile_regular.nim" 8 6))
  (User :severity Hint :str "Another hint" :location ("tfile_regular.nim" 10 6))""")

  block:
    for (l, r) in @[
      ("""
  old text1
  """, """
  old txt1
  """), ("""
  old text1
  old text2
  """, """
  old text1
  old text2
  """), ("""
  (User :str "User Hint" :location ("tfile.nim" 8 _))""", """
  (User :severity Hint :str "User hint" :location ("tfile_regular.nim" 8 6))
  (User :severity Hint :str "Another hint" :location ("tfile_regular.nim" 10 6))""")
    ]:
      for g in [true, false]:
        var fmt = diffFormatter()
        fmt.groupLine = g
        echo ">>>"
        echo formatDiffed(l, r, fmt)

        # fmt.formatChunk = proc(
        #   text: string, mode, secondary: SeqEditKind,
        #   inline: bool
        # ): ColText =
        #   toColText("<span class=\"diff-$1\">$2</span>" % [
        #     substr($mode, 3), text
        #   ])

        # echo ".. raw:: html\n"
        # echo "    <code><pre>"
        # echo formatDiffed(l, r, fmt).indent(4)
        # echo "    </pre></core>"
