#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The system module defines several common functions for working with strings,
## such as:
## * `$` for converting other data-types to strings
## * `&` for string concatenation
## * `add` for adding a new character or a string to the existing one
## * `in` (alias for `contains`) and `notin` for checking if a character
##   is in a string
##
## This module builds upon that, providing additional functionality in form of
## procedures, iterators and templates for strings.

runnableExamples:
  let
    numbers = @[867, 5309]
    multiLineString = "first line\nsecond line\nthird line"

  let jenny = numbers.join("-")
  assert jenny == "867-5309"

  assert splitLines(multiLineString) ==
         @["first line", "second line", "third line"]
  assert split(multiLineString) == @["first", "line", "second",
                                     "line", "third", "line"]
  assert indent(multiLineString, 4) ==
         "    first line\n    second line\n    third line"
  assert 'z'.repeat(5) == "zzzzz"

## The chaining of functions is possible thanks to the
## `method call syntax<manual.html#procedures-method-call-syntax>`_:

runnableExamples:
  from std/sequtils import map

  let jenny = "867-5309"
  assert jenny.split('-').map(parseInt) == @[867, 5309]

  assert "Beetlejuice".indent(1).repeat(3).strip ==
         "Beetlejuice Beetlejuice Beetlejuice"

## This module is available for the `JavaScript target
## <backends.html#backends-the-javascript-target>`_.
##
## ----
##
## **See also:**
## * `strformat module<strformat.html>`_ for string interpolation and formatting
## * `unicode module<unicode.html>`_ for Unicode UTF-8 handling
## * `sequtils module<sequtils.html>`_ for operations on container
##   types (including strings)
## * `parsecsv module<parsecsv.html>`_ for a high-performance CSV parser
## * `parseutils module<parseutils.html>`_ for lower-level parsing of tokens,
##   numbers, identifiers, etc.
## * `parseopt module<parseopt.html>`_ for command-line parsing
## * `pegs module<pegs.html>`_ for PEG (Parsing Expression Grammar) support
## * `strtabs module<strtabs.html>`_ for efficient hash tables
##   (dictionaries, in some programming languages) mapping from strings to strings
## * `ropes module<ropes.html>`_ for rope data type, which can represent very
##   long strings efficiently
## * `re module<re.html>`_ for regular expression (regex) support
## * `strscans<strscans.html>`_ for `scanf` and `scanp` macros, which offer
##   easier substring extraction than regular expressions


import parseutils
from math import pow, floor, log10
from algorithm import fill, reverse
import std/enumutils

from unicode import toLower, toUpper
export toLower, toUpper

include "system/inclrtl"
import std/private/since
from std/private/strimpl import cmpIgnoreStyleImpl, cmpIgnoreCaseImpl, startsWithImpl, endsWithImpl


const
  Whitespace* = {' ', '\t', '\v', '\r', '\l', '\f'}
    ## All the characters that count as whitespace (space, tab, vertical tab,
    ## carriage return, new line, form feed).

  Letters* = {'A'..'Z', 'a'..'z'}
    ## The set of letters.

  Digits* = {'0'..'9'}
    ## The set of digits.

  HexDigits* = {'0'..'9', 'A'..'F', 'a'..'f'}
    ## The set of hexadecimal digits.

  IdentChars* = {'a'..'z', 'A'..'Z', '0'..'9', '_'}
    ## The set of characters an identifier can consist of.

  IdentStartChars* = {'a'..'z', 'A'..'Z', '_'}
    ## The set of characters an identifier can start with.

  Newlines* = {'\13', '\10'}
    ## The set of characters a newline terminator can start with (carriage
    ## return, line feed).

  AllChars* = {'\x00'..'\xFF'}
    ## A set with all the possible characters.
    ##
    ## Not very useful by its own, you can use it to create *inverted* sets to
    ## make the `find func<#find,string,set[char],Natural,int>`_
    ## find **invalid** characters in strings. Example:
    ##
    ## .. code-block:: nim
    ##   let invalid = AllChars - Digits
    ##   doAssert "01234".find(invalid) == -1
    ##   doAssert "01A34".find(invalid) == 2

func isAlphaAscii*(c: char): bool {.rtl, extern: "nsuIsAlphaAsciiChar".} =
  ## Checks whether or not character `c` is alphabetical.
  ##
  ## This checks a-z, A-Z ASCII characters only.
  ## Use `Unicode module<unicode.html>`_ for UTF-8 support.
  runnableExamples:
    doAssert isAlphaAscii('e') == true
    doAssert isAlphaAscii('E') == true
    doAssert isAlphaAscii('8') == false
  return c in Letters

func isAlphaNumeric*(c: char): bool {.rtl, extern: "nsuIsAlphaNumericChar".} =
  ## Checks whether or not `c` is alphanumeric.
  ##
  ## This checks a-z, A-Z, 0-9 ASCII characters only.
  runnableExamples:
    doAssert isAlphaNumeric('n') == true
    doAssert isAlphaNumeric('8') == true
    doAssert isAlphaNumeric(' ') == false
  return c in Letters+Digits

func isDigit*(c: char): bool {.rtl, extern: "nsuIsDigitChar".} =
  ## Checks whether or not `c` is a number.
  ##
  ## This checks 0-9 ASCII characters only.
  runnableExamples:
    doAssert isDigit('n') == false
    doAssert isDigit('8') == true
  return c in Digits

func isSpaceAscii*(c: char): bool {.rtl, extern: "nsuIsSpaceAsciiChar".} =
  ## Checks whether or not `c` is a whitespace character.
  runnableExamples:
    doAssert isSpaceAscii('n') == false
    doAssert isSpaceAscii(' ') == true
    doAssert isSpaceAscii('\t') == true
  return c in Whitespace

func isLowerAscii*(c: char): bool {.rtl, extern: "nsuIsLowerAsciiChar".} =
  ## Checks whether or not `c` is a lower case character.
  ##
  ## This checks ASCII characters only.
  ## Use `Unicode module<unicode.html>`_ for UTF-8 support.
  ##
  ## See also:
  ## * `toLowerAscii func<#toLowerAscii,char>`_
  runnableExamples:
    doAssert isLowerAscii('e') == true
    doAssert isLowerAscii('E') == false
    doAssert isLowerAscii('7') == false
  return c in {'a'..'z'}

func isUpperAscii*(c: char): bool {.rtl, extern: "nsuIsUpperAsciiChar".} =
  ## Checks whether or not `c` is an upper case character.
  ##
  ## This checks ASCII characters only.
  ## Use `Unicode module<unicode.html>`_ for UTF-8 support.
  ##
  ## See also:
  ## * `toUpperAscii func<#toUpperAscii,char>`_
  runnableExamples:
    doAssert isUpperAscii('e') == false
    doAssert isUpperAscii('E') == true
    doAssert isUpperAscii('7') == false
  return c in {'A'..'Z'}


func toLowerAscii*(c: char): char {.rtl, extern: "nsuToLowerAsciiChar".} =
  ## Returns the lower case version of character `c`.
  ##
  ## This works only for the letters `A-Z`. See `unicode.toLower
  ## <unicode.html#toLower,Rune>`_ for a version that works for any Unicode
  ## character.
  ##
  ## See also:
  ## * `isLowerAscii func<#isLowerAscii,char>`_
  ## * `toLowerAscii func<#toLowerAscii,string>`_ for converting a string
  runnableExamples:
    doAssert toLowerAscii('A') == 'a'
    doAssert toLowerAscii('e') == 'e'
  if c in {'A'..'Z'}:
    result = char(uint8(c) xor 0b0010_0000'u8)
  else:
    result = c

template toImpl(call) =
  result = newString(len(s))
  for i in 0..len(s) - 1:
    result[i] = call(s[i])

func toLowerAscii*(s: string): string {.rtl, extern: "nsuToLowerAsciiStr".} =
  ## Converts string `s` into lower case.
  ##
  ## This works only for the letters `A-Z`. See `unicode.toLower
  ## <unicode.html#toLower,string>`_ for a version that works for any Unicode
  ## character.
  ##
  ## See also:
  ## * `normalize func<#normalize,string>`_
  runnableExamples:
    doAssert toLowerAscii("FooBar!") == "foobar!"
  toImpl toLowerAscii

func toUpperAscii*(c: char): char {.rtl, extern: "nsuToUpperAsciiChar".} =
  ## Converts character `c` into upper case.
  ##
  ## This works only for the letters `A-Z`.  See `unicode.toUpper
  ## <unicode.html#toUpper,Rune>`_ for a version that works for any Unicode
  ## character.
  ##
  ## See also:
  ## * `isUpperAscii func<#isUpperAscii,char>`_
  ## * `toUpperAscii func<#toUpperAscii,string>`_ for converting a string
  ## * `capitalizeAscii func<#capitalizeAscii,string>`_
  runnableExamples:
    doAssert toUpperAscii('a') == 'A'
    doAssert toUpperAscii('E') == 'E'
  if c in {'a'..'z'}:
    result = char(uint8(c) xor 0b0010_0000'u8)
  else:
    result = c

func toUpperAscii*(s: string): string {.rtl, extern: "nsuToUpperAsciiStr".} =
  ## Converts string `s` into upper case.
  ##
  ## This works only for the letters `A-Z`.  See `unicode.toUpper
  ## <unicode.html#toUpper,string>`_ for a version that works for any Unicode
  ## character.
  ##
  ## See also:
  ## * `capitalizeAscii func<#capitalizeAscii,string>`_
  runnableExamples:
    doAssert toUpperAscii("FooBar!") == "FOOBAR!"
  toImpl toUpperAscii

func capitalizeAscii*(s: string): string {.rtl, extern: "nsuCapitalizeAscii".} =
  ## Converts the first character of string `s` into upper case.
  ##
  ## This works only for the letters `A-Z`.
  ## Use `Unicode module<unicode.html>`_ for UTF-8 support.
  ##
  ## See also:
  ## * `toUpperAscii func<#toUpperAscii,char>`_
  runnableExamples:
    doAssert capitalizeAscii("foo") == "Foo"
    doAssert capitalizeAscii("-bar") == "-bar"
  if s.len == 0: result = ""
  else: result = toUpperAscii(s[0]) & substr(s, 1)

func nimIdentNormalize*(s: string): string =
  ## Normalizes the string `s` as a Nim identifier.
  ##
  ## That means to convert to lower case and remove any '_' on all characters
  ## except first one.
  runnableExamples:
    doAssert nimIdentNormalize("Foo_bar") == "Foobar"
  result = newString(s.len)
  if s.len == 0:
    return
  result[0] = s[0]
  var j = 1
  for i in 1..len(s) - 1:
    if s[i] in {'A'..'Z'}:
      result[j] = chr(ord(s[i]) + (ord('a') - ord('A')))
      inc j
    elif s[i] != '_':
      result[j] = s[i]
      inc j
  if j != s.len: setLen(result, j)

func normalize*(s: string): string {.rtl, extern: "nsuNormalize".} =
  ## Normalizes the string `s`.
  ##
  ## That means to convert it to lower case and remove any '_'. This
  ## should NOT be used to normalize Nim identifier names.
  ##
  ## See also:
  ## * `toLowerAscii func<#toLowerAscii,string>`_
  runnableExamples:
    doAssert normalize("Foo_bar") == "foobar"
    doAssert normalize("Foo Bar") == "foo bar"
  result = newString(s.len)
  var j = 0
  for i in 0..len(s) - 1:
    if s[i] in {'A'..'Z'}:
      result[j] = chr(ord(s[i]) + (ord('a') - ord('A')))
      inc j
    elif s[i] != '_':
      result[j] = s[i]
      inc j
  if j != s.len: setLen(result, j)

func cmpIgnoreCase*(a, b: string): int {.rtl, extern: "nsuCmpIgnoreCase".} =
  ## Compares two strings in a case insensitive manner. Returns:
  ##
  ## | 0 if a == b
  ## | < 0 if a < b
  ## | > 0 if a > b
  runnableExamples:
    doAssert cmpIgnoreCase("FooBar", "foobar") == 0
    doAssert cmpIgnoreCase("bar", "Foo") < 0
    doAssert cmpIgnoreCase("Foo5", "foo4") > 0
  cmpIgnoreCaseImpl(a, b)

{.push checks: off, line_trace: off.} # this is a hot-spot in the compiler!
                                      # thus we compile without checks here

func cmpIgnoreStyle*(a, b: string): int {.rtl, extern: "nsuCmpIgnoreStyle".} =
  ## Semantically the same as `cmp(normalize(a), normalize(b))`. It
  ## is just optimized to not allocate temporary strings. This should
  ## NOT be used to compare Nim identifier names.
  ## Use `macros.eqIdent<macros.html#eqIdent,string,string>`_ for that.
  ##
  ## Returns:
  ##
  ## | 0 if a == b
  ## | < 0 if a < b
  ## | > 0 if a > b
  runnableExamples:
    doAssert cmpIgnoreStyle("foo_bar", "FooBar") == 0
    doAssert cmpIgnoreStyle("foo_bar_5", "FooBar4") > 0
  cmpIgnoreStyleImpl(a, b)
{.pop.}

# --------- Private templates for different split separators -----------

func substrEq(s: string, pos: int, substr: string): bool =
  var i = 0
  var length = substr.len
  while i < length and pos+i < s.len and s[pos+i] == substr[i]:
    inc i
  return i == length

template stringHasSep(s: string, index: int, seps: set[char]): bool =
  s[index] in seps

template stringHasSep(s: string, index: int, sep: char): bool =
  s[index] == sep

template stringHasSep(s: string, index: int, sep: string): bool =
  s.substrEq(index, sep)

template splitCommon(s, sep, maxsplit, sepLen) =
  ## Common code for split procs
  var last = 0
  var splits = maxsplit

  while last <= len(s):
    var first = last
    while last < len(s) and not stringHasSep(s, last, sep):
      inc(last)
    if splits == 0: last = len(s)
    yield substr(s, first, last-1)
    if splits == 0: break
    dec(splits)
    inc(last, sepLen)

template oldSplit(s, seps, maxsplit) =
  var last = 0
  var splits = maxsplit
  assert(not ('\0' in seps))
  while last < len(s):
    while last < len(s) and s[last] in seps: inc(last)
    var first = last
    while last < len(s) and s[last] notin seps: inc(last)
    if first <= last-1:
      if splits == 0: last = len(s)
      yield substr(s, first, last-1)
      if splits == 0: break
      dec(splits)

template accResult(iter: untyped) =
  result = @[]
  for x in iter: add(result, x)


iterator split*(s: string, sep: char, maxsplit: int = -1): string =
  ## Splits the string `s` into substrings using a single separator.
  ##
  ## Substrings are separated by the character `sep`.
  ## The code:
  ##
  ## .. code-block:: nim
  ##   for word in split(";;this;is;an;;example;;;", ';'):
  ##     writeLine(stdout, word)
  ##
  ## Results in:
  ##
  ## .. code-block::
  ##   ""
  ##   ""
  ##   "this"
  ##   "is"
  ##   "an"
  ##   ""
  ##   "example"
  ##   ""
  ##   ""
  ##   ""
  ##
  ## See also:
  ## * `rsplit iterator<#rsplit.i,string,char,int>`_
  ## * `splitLines iterator<#splitLines.i,string>`_
  ## * `splitWhitespace iterator<#splitWhitespace.i,string,int>`_
  ## * `split func<#split,string,char,int>`_
  splitCommon(s, sep, maxsplit, 1)

iterator split*(s: string, seps: set[char] = Whitespace,
                maxsplit: int = -1): string =
  ## Splits the string `s` into substrings using a group of separators.
  ##
  ## Substrings are separated by a substring containing only `seps`.
  ##
  ## .. code-block:: nim
  ##   for word in split("this\lis an\texample"):
  ##     writeLine(stdout, word)
  ##
  ## ...generates this output:
  ##
  ## .. code-block::
  ##   "this"
  ##   "is"
  ##   "an"
  ##   "example"
  ##
  ## And the following code:
  ##
  ## .. code-block:: nim
  ##   for word in split("this:is;an$example", {';', ':', '$'}):
  ##     writeLine(stdout, word)
  ##
  ## ...produces the same output as the first example. The code:
  ##
  ## .. code-block:: nim
  ##   let date = "2012-11-20T22:08:08.398990"
  ##   let separators = {' ', '-', ':', 'T'}
  ##   for number in split(date, separators):
  ##     writeLine(stdout, number)
  ##
  ## ...results in:
  ##
  ## .. code-block::
  ##   "2012"
  ##   "11"
  ##   "20"
  ##   "22"
  ##   "08"
  ##   "08.398990"
  ##
  ## See also:
  ## * `rsplit iterator<#rsplit.i,string,set[char],int>`_
  ## * `splitLines iterator<#splitLines.i,string>`_
  ## * `splitWhitespace iterator<#splitWhitespace.i,string,int>`_
  ## * `split func<#split,string,set[char],int>`_
  splitCommon(s, seps, maxsplit, 1)

iterator split*(s: string, sep: string, maxsplit: int = -1): string =
  ## Splits the string `s` into substrings using a string separator.
  ##
  ## Substrings are separated by the string `sep`.
  ## The code:
  ##
  ## .. code-block:: nim
  ##   for word in split("thisDATAisDATAcorrupted", "DATA"):
  ##     writeLine(stdout, word)
  ##
  ## Results in:
  ##
  ## .. code-block::
  ##   "this"
  ##   "is"
  ##   "corrupted"
  ##
  ## See also:
  ## * `rsplit iterator<#rsplit.i,string,string,int,bool>`_
  ## * `splitLines iterator<#splitLines.i,string>`_
  ## * `splitWhitespace iterator<#splitWhitespace.i,string,int>`_
  ## * `split func<#split,string,string,int>`_
  splitCommon(s, sep, maxsplit, sep.len)


template rsplitCommon(s, sep, maxsplit, sepLen) =
  ## Common code for rsplit functions
  var
    last = s.len - 1
    first = last
    splits = maxsplit
    startPos = 0
  # go to -1 in order to get separators at the beginning
  while first >= -1:
    while first >= 0 and not stringHasSep(s, first, sep):
      dec(first)
    if splits == 0:
      # No more splits means set first to the beginning
      first = -1
    if first == -1:
      startPos = 0
    else:
      startPos = first + sepLen
    yield substr(s, startPos, last)
    if splits == 0: break
    dec(splits)
    dec(first)
    last = first

iterator rsplit*(s: string, sep: char,
                 maxsplit: int = -1): string =
  ## Splits the string `s` into substrings from the right using a
  ## string separator. Works exactly the same as `split iterator
  ## <#split.i,string,char,int>`_ except in reverse order.
  ##
  ## .. code-block:: nim
  ##   for piece in "foo:bar".rsplit(':'):
  ##     echo piece
  ##
  ## Results in:
  ##
  ## .. code-block:: nim
  ##   "bar"
  ##   "foo"
  ##
  ## Substrings are separated from the right by the char `sep`.
  ##
  ## See also:
  ## * `split iterator<#split.i,string,char,int>`_
  ## * `splitLines iterator<#splitLines.i,string>`_
  ## * `splitWhitespace iterator<#splitWhitespace.i,string,int>`_
  ## * `rsplit func<#rsplit,string,char,int>`_
  rsplitCommon(s, sep, maxsplit, 1)

iterator rsplit*(s: string, seps: set[char] = Whitespace,
                 maxsplit: int = -1): string =
  ## Splits the string `s` into substrings from the right using a
  ## string separator. Works exactly the same as `split iterator
  ## <#split.i,string,char,int>`_ except in reverse order.
  ##
  ## .. code-block:: nim
  ##   for piece in "foo bar".rsplit(WhiteSpace):
  ##     echo piece
  ##
  ## Results in:
  ##
  ## .. code-block:: nim
  ##   "bar"
  ##   "foo"
  ##
  ## Substrings are separated from the right by the set of chars `seps`
  ##
  ## See also:
  ## * `split iterator<#split.i,string,set[char],int>`_
  ## * `splitLines iterator<#splitLines.i,string>`_
  ## * `splitWhitespace iterator<#splitWhitespace.i,string,int>`_
  ## * `rsplit func<#rsplit,string,set[char],int>`_
  rsplitCommon(s, seps, maxsplit, 1)

iterator rsplit*(s: string, sep: string, maxsplit: int = -1,
                 keepSeparators: bool = false): string =
  ## Splits the string `s` into substrings from the right using a
  ## string separator. Works exactly the same as `split iterator
  ## <#split.i,string,string,int>`_ except in reverse order.
  ##
  ## .. code-block:: nim
  ##   for piece in "foothebar".rsplit("the"):
  ##     echo piece
  ##
  ## Results in:
  ##
  ## .. code-block:: nim
  ##   "bar"
  ##   "foo"
  ##
  ## Substrings are separated from the right by the string `sep`
  ##
  ## See also:
  ## * `split iterator<#split.i,string,string,int>`_
  ## * `splitLines iterator<#splitLines.i,string>`_
  ## * `splitWhitespace iterator<#splitWhitespace.i,string,int>`_
  ## * `rsplit func<#rsplit,string,string,int>`_
  rsplitCommon(s, sep, maxsplit, sep.len)

iterator splitLines*(s: string, keepEol = false): string =
  ## Splits the string `s` into its containing lines.
  ##
  ## Every `character literal <manual.html#lexical-analysis-character-literals>`_
  ## newline combination (CR, LF, CR-LF) is supported. The result strings
  ## contain no trailing end of line characters unless parameter `keepEol`
  ## is set to `true`.
  ##
  ## Example:
  ##
  ## .. code-block:: nim
  ##   for line in splitLines("\nthis\nis\nan\n\nexample\n"):
  ##     writeLine(stdout, line)
  ##
  ## Results in:
  ##
  ## .. code-block:: nim
  ##   ""
  ##   "this"
  ##   "is"
  ##   "an"
  ##   ""
  ##   "example"
  ##   ""
  ##
  ## See also:
  ## * `splitWhitespace iterator<#splitWhitespace.i,string,int>`_
  ## * `splitLines func<#splitLines,string>`_
  var first = 0
  var last = 0
  var eolpos = 0
  while true:
    while last < s.len and s[last] notin {'\c', '\l'}: inc(last)

    eolpos = last
    if last < s.len:
      if s[last] == '\l': inc(last)
      elif s[last] == '\c':
        inc(last)
        if last < s.len and s[last] == '\l': inc(last)

    yield substr(s, first, if keepEol: last-1 else: eolpos-1)

    # no eol characters consumed means that the string is over
    if eolpos == last:
      break

    first = last

iterator splitWhitespace*(s: string, maxsplit: int = -1): string =
  ## Splits the string `s` at whitespace stripping leading and trailing
  ## whitespace if necessary. If `maxsplit` is specified and is positive,
  ## no more than `maxsplit` splits is made.
  ##
  ## The following code:
  ##
  ## .. code-block:: nim
  ##   let s = "  foo \t bar  baz  "
  ##   for ms in [-1, 1, 2, 3]:
  ##     echo "------ maxsplit = ", ms, ":"
  ##     for item in s.splitWhitespace(maxsplit=ms):
  ##       echo '"', item, '"'
  ##
  ## ...results in:
  ##
  ## .. code-block::
  ##   ------ maxsplit = -1:
  ##   "foo"
  ##   "bar"
  ##   "baz"
  ##   ------ maxsplit = 1:
  ##   "foo"
  ##   "bar  baz  "
  ##   ------ maxsplit = 2:
  ##   "foo"
  ##   "bar"
  ##   "baz  "
  ##   ------ maxsplit = 3:
  ##   "foo"
  ##   "bar"
  ##   "baz"
  ##
  ## See also:
  ## * `splitLines iterator<#splitLines.i,string>`_
  ## * `splitWhitespace func<#splitWhitespace,string,int>`_
  oldSplit(s, Whitespace, maxsplit)



func split*(s: string, sep: char, maxsplit: int = -1): seq[string] {.rtl,
    extern: "nsuSplitChar".} =
  ## The same as the `split iterator <#split.i,string,char,int>`_ (see its
  ## documentation), but is a func that returns a sequence of substrings.
  ##
  ## See also:
  ## * `split iterator <#split.i,string,char,int>`_
  ## * `rsplit func<#rsplit,string,char,int>`_
  ## * `splitLines func<#splitLines,string>`_
  ## * `splitWhitespace func<#splitWhitespace,string,int>`_
  runnableExamples:
    doAssert "a,b,c".split(',') == @["a", "b", "c"]
    doAssert "".split(' ') == @[""]
  accResult(split(s, sep, maxsplit))

func split*(s: string, seps: set[char] = Whitespace, maxsplit: int = -1): seq[
    string] {.rtl, extern: "nsuSplitCharSet".} =
  ## The same as the `split iterator <#split.i,string,set[char],int>`_ (see its
  ## documentation), but is a func that returns a sequence of substrings.
  ##
  ## See also:
  ## * `split iterator <#split.i,string,set[char],int>`_
  ## * `rsplit func<#rsplit,string,set[char],int>`_
  ## * `splitLines func<#splitLines,string>`_
  ## * `splitWhitespace func<#splitWhitespace,string,int>`_
  runnableExamples:
    doAssert "a,b;c".split({',', ';'}) == @["a", "b", "c"]
    doAssert "".split({' '}) == @[""]
  accResult(split(s, seps, maxsplit))

func split*(s: string, sep: string, maxsplit: int = -1): seq[string] {.rtl,
    extern: "nsuSplitString".} =
  ## Splits the string `s` into substrings using a string separator.
  ##
  ## Substrings are separated by the string `sep`. This is a wrapper around the
  ## `split iterator <#split.i,string,string,int>`_.
  ##
  ## See also:
  ## * `split iterator <#split.i,string,string,int>`_
  ## * `rsplit func<#rsplit,string,string,int>`_
  ## * `splitLines func<#splitLines,string>`_
  ## * `splitWhitespace func<#splitWhitespace,string,int>`_
  runnableExamples:
    doAssert "a,b,c".split(",") == @["a", "b", "c"]
    doAssert "a man a plan a canal panama".split("a ") == @["", "man ", "plan ", "canal panama"]
    doAssert "".split("Elon Musk") == @[""]
    doAssert "a  largely    spaced sentence".split(" ") == @["a", "", "largely",
        "", "", "", "spaced", "sentence"]
    doAssert "a  largely    spaced sentence".split(" ", maxsplit = 1) == @["a", " largely    spaced sentence"]
  doAssert(sep.len > 0)

  accResult(split(s, sep, maxsplit))

func rsplit*(s: string, sep: char, maxsplit: int = -1): seq[string] {.rtl,
    extern: "nsuRSplitChar".} =
  ## The same as the `rsplit iterator <#rsplit.i,string,char,int>`_, but is a func
  ## that returns a sequence of substrings.
  ##
  ## A possible common use case for `rsplit` is path manipulation,
  ## particularly on systems that don't use a common delimiter.
  ##
  ## For example, if a system had `#` as a delimiter, you could
  ## do the following to get the tail of the path:
  ##
  ## .. code-block:: nim
  ##   var tailSplit = rsplit("Root#Object#Method#Index", '#', maxsplit=1)
  ##
  ## Results in `tailSplit` containing:
  ##
  ## .. code-block:: nim
  ##   @["Root#Object#Method", "Index"]
  ##
  ## See also:
  ## * `rsplit iterator <#rsplit.i,string,char,int>`_
  ## * `split func<#split,string,char,int>`_
  ## * `splitLines func<#splitLines,string>`_
  ## * `splitWhitespace func<#splitWhitespace,string,int>`_
  accResult(rsplit(s, sep, maxsplit))
  result.reverse()

func rsplit*(s: string, seps: set[char] = Whitespace,
             maxsplit: int = -1): seq[string]
             {.rtl, extern: "nsuRSplitCharSet".} =
  ## The same as the `rsplit iterator <#rsplit.i,string,set[char],int>`_, but is a
  ## func that returns a sequence of substrings.
  ##
  ## A possible common use case for `rsplit` is path manipulation,
  ## particularly on systems that don't use a common delimiter.
  ##
  ## For example, if a system had `#` as a delimiter, you could
  ## do the following to get the tail of the path:
  ##
  ## .. code-block:: nim
  ##   var tailSplit = rsplit("Root#Object#Method#Index", {'#'}, maxsplit=1)
  ##
  ## Results in `tailSplit` containing:
  ##
  ## .. code-block:: nim
  ##   @["Root#Object#Method", "Index"]
  ##
  ## See also:
  ## * `rsplit iterator <#rsplit.i,string,set[char],int>`_
  ## * `split func<#split,string,set[char],int>`_
  ## * `splitLines func<#splitLines,string>`_
  ## * `splitWhitespace func<#splitWhitespace,string,int>`_
  accResult(rsplit(s, seps, maxsplit))
  result.reverse()

func rsplit*(s: string, sep: string, maxsplit: int = -1): seq[string] {.rtl,
    extern: "nsuRSplitString".} =
  ## The same as the `rsplit iterator <#rsplit.i,string,string,int,bool>`_, but is a func
  ## that returns a sequence of substrings.
  ##
  ## A possible common use case for `rsplit` is path manipulation,
  ## particularly on systems that don't use a common delimiter.
  ##
  ## For example, if a system had `#` as a delimiter, you could
  ## do the following to get the tail of the path:
  ##
  ## .. code-block:: nim
  ##   var tailSplit = rsplit("Root#Object#Method#Index", "#", maxsplit=1)
  ##
  ## Results in `tailSplit` containing:
  ##
  ## .. code-block:: nim
  ##   @["Root#Object#Method", "Index"]
  ##
  ## See also:
  ## * `rsplit iterator <#rsplit.i,string,string,int,bool>`_
  ## * `split func<#split,string,string,int>`_
  ## * `splitLines func<#splitLines,string>`_
  ## * `splitWhitespace func<#splitWhitespace,string,int>`_
  runnableExamples:
    doAssert "a  largely    spaced sentence".rsplit(" ", maxsplit = 1) == @[
        "a  largely    spaced", "sentence"]
    doAssert "a,b,c".rsplit(",") == @["a", "b", "c"]
    doAssert "a man a plan a canal panama".rsplit("a ") == @["", "man ",
        "plan ", "canal panama"]
    doAssert "".rsplit("Elon Musk") == @[""]
    doAssert "a  largely    spaced sentence".rsplit(" ") == @["a", "",
        "largely", "", "", "", "spaced", "sentence"]
  accResult(rsplit(s, sep, maxsplit))
  result.reverse()

func splitLines*(s: string, keepEol = false): seq[string] {.rtl,
    extern: "nsuSplitLines".} =
  ## The same as the `splitLines iterator<#splitLines.i,string>`_ (see its
  ## documentation), but is a func that returns a sequence of substrings.
  ##
  ## See also:
  ## * `splitLines iterator<#splitLines.i,string>`_
  ## * `splitWhitespace func<#splitWhitespace,string,int>`_
  ## * `countLines func<#countLines,string>`_
  accResult(splitLines(s, keepEol = keepEol))

func splitWhitespace*(s: string, maxsplit: int = -1): seq[string] {.rtl,
    extern: "nsuSplitWhitespace".} =
  ## The same as the `splitWhitespace iterator <#splitWhitespace.i,string,int>`_
  ## (see its documentation), but is a func that returns a sequence of substrings.
  ##
  ## See also:
  ## * `splitWhitespace iterator <#splitWhitespace.i,string,int>`_
  ## * `splitLines func<#splitLines,string>`_
  accResult(splitWhitespace(s, maxsplit))

func toBin*(x: BiggestInt, len: Positive): string {.rtl, extern: "nsuToBin".} =
  ## Converts `x` into its binary representation.
  ##
  ## The resulting string is always `len` characters long. No leading `0b`
  ## prefix is generated.
  runnableExamples:
    let
      a = 29
      b = 257
    doAssert a.toBin(8) == "00011101"
    doAssert b.toBin(8) == "00000001"
    doAssert b.toBin(9) == "100000001"
  var
    mask = BiggestUInt 1
    shift = BiggestUInt 0
  assert(len > 0)
  result = newString(len)
  for j in countdown(len-1, 0):
    result[j] = chr(int((BiggestUInt(x) and mask) shr shift) + ord('0'))
    inc shift
    mask = mask shl BiggestUInt(1)

func toOct*(x: BiggestInt, len: Positive): string {.rtl, extern: "nsuToOct".} =
  ## Converts `x` into its octal representation.
  ##
  ## The resulting string is always `len` characters long. No leading `0o`
  ## prefix is generated.
  ##
  ## Do not confuse it with `toOctal func<#toOctal,char>`_.
  runnableExamples:
    let
      a = 62
      b = 513
    doAssert a.toOct(3) == "076"
    doAssert b.toOct(3) == "001"
    doAssert b.toOct(5) == "01001"
  var
    mask = BiggestUInt 7
    shift = BiggestUInt 0
  assert(len > 0)
  result = newString(len)
  for j in countdown(len-1, 0):
    result[j] = chr(int((BiggestUInt(x) and mask) shr shift) + ord('0'))
    inc shift, 3
    mask = mask shl BiggestUInt(3)

func toHexImpl(x: BiggestUInt, len: Positive, handleNegative: bool): string =
  const
    HexChars = "0123456789ABCDEF"
  var n = x
  result = newString(len)
  for j in countdown(len-1, 0):
    result[j] = HexChars[int(n and 0xF)]
    n = n shr 4
    # handle negative overflow
    if n == 0 and handleNegative: n = not(BiggestUInt 0)

func toHex*[T: SomeInteger](x: T, len: Positive): string =
  ## Converts `x` to its hexadecimal representation.
  ##
  ## The resulting string will be exactly `len` characters long. No prefix like
  ## `0x` is generated. `x` is treated as an unsigned value.
  runnableExamples:
    let
      a = 62'u64
      b = 4097'u64
    doAssert a.toHex(3) == "03E"
    doAssert b.toHex(3) == "001"
    doAssert b.toHex(4) == "1001"
    doAssert toHex(62, 3) == "03E"
    doAssert toHex(-8, 6) == "FFFFF8"
  toHexImpl(cast[BiggestUInt](x), len, x < 0)

func toHex*[T: SomeInteger](x: T): string =
  ## Shortcut for `toHex(x, T.sizeof * 2)`
  runnableExamples:
    doAssert toHex(1984'i64) == "00000000000007C0"
    doAssert toHex(1984'i16) == "07C0"
  toHexImpl(cast[BiggestUInt](x), 2*sizeof(T), x < 0)

func toHex*(s: string): string {.rtl.} =
  ## Converts a bytes string to its hexadecimal representation.
  ##
  ## The output is twice the input long. No prefix like
  ## `0x` is generated.
  ##
  ## See also:
  ## * `parseHexStr func<#parseHexStr,string>`_ for the reverse operation
  runnableExamples:
    let
      a = "1"
      b = "A"
      c = "\0\255"
    doAssert a.toHex() == "31"
    doAssert b.toHex() == "41"
    doAssert c.toHex() == "00FF"

  const HexChars = "0123456789ABCDEF"
  result = newString(s.len * 2)
  for pos, c in s:
    var n = ord(c)
    result[pos * 2 + 1] = HexChars[n and 0xF]
    n = n shr 4
    result[pos * 2] = HexChars[n]

func toOctal*(c: char): string {.rtl, extern: "nsuToOctal".} =
  ## Converts a character `c` to its octal representation.
  ##
  ## The resulting string may not have a leading zero. Its length is always
  ## exactly 3.
  ##
  ## Do not confuse it with `toOct func<#toOct,BiggestInt,Positive>`_.
  runnableExamples:
    doAssert toOctal('1') == "061"
    doAssert toOctal('A') == "101"
    doAssert toOctal('a') == "141"
    doAssert toOctal('!') == "041"

  result = newString(3)
  var val = ord(c)
  for i in countdown(2, 0):
    result[i] = chr(val mod 8 + ord('0'))
    val = val div 8

func fromBin*[T: SomeInteger](s: string): T =
  ## Parses a binary integer value from a string `s`.
  ##
  ## If `s` is not a valid binary integer, `ValueError` is raised. `s` can have
  ## one of the following optional prefixes: `0b`, `0B`. Underscores within
  ## `s` are ignored.
  ##
  ## Does not check for overflow. If the value represented by `s`
  ## is too big to fit into a return type, only the value of the rightmost
  ## binary digits of `s` is returned without producing an error.
  runnableExamples:
    let s = "0b_0100_1000_1000_1000_1110_1110_1001_1001"
    doAssert fromBin[int](s) == 1216933529
    doAssert fromBin[int8](s) == 0b1001_1001'i8
    doAssert fromBin[int8](s) == -103'i8
    doAssert fromBin[uint8](s) == 153
    doAssert s.fromBin[:int16] == 0b1110_1110_1001_1001'i16
    doAssert s.fromBin[:uint64] == 1216933529'u64

  let p = parseutils.parseBin(s, result)
  if p != s.len or p == 0:
    raise newException(ValueError, "invalid binary integer: " & s)

func fromOct*[T: SomeInteger](s: string): T =
  ## Parses an octal integer value from a string `s`.
  ##
  ## If `s` is not a valid octal integer, `ValueError` is raised. `s` can have
  ## one of the following optional prefixes: `0o`, `0O`. Underscores within
  ## `s` are ignored.
  ##
  ## Does not check for overflow. If the value represented by `s`
  ## is too big to fit into a return type, only the value of the rightmost
  ## octal digits of `s` is returned without producing an error.
  runnableExamples:
    let s = "0o_123_456_777"
    doAssert fromOct[int](s) == 21913087
    doAssert fromOct[int8](s) == 0o377'i8
    doAssert fromOct[int8](s) == -1'i8
    doAssert fromOct[uint8](s) == 255'u8
    doAssert s.fromOct[:int16] == 24063'i16
    doAssert s.fromOct[:uint64] == 21913087'u64

  let p = parseutils.parseOct(s, result)
  if p != s.len or p == 0:
    raise newException(ValueError, "invalid oct integer: " & s)

func fromHex*[T: SomeInteger](s: string): T =
  ## Parses a hex integer value from a string `s`.
  ##
  ## If `s` is not a valid hex integer, `ValueError` is raised. `s` can have
  ## one of the following optional prefixes: `0x`, `0X`, `#`. Underscores within
  ## `s` are ignored.
  ##
  ## Does not check for overflow. If the value represented by `s`
  ## is too big to fit into a return type, only the value of the rightmost
  ## hex digits of `s` is returned without producing an error.
  runnableExamples:
    let s = "0x_1235_8df6"
    doAssert fromHex[int](s) == 305499638
    doAssert fromHex[int8](s) == 0xf6'i8
    doAssert fromHex[int8](s) == -10'i8
    doAssert fromHex[uint8](s) == 246'u8
    doAssert s.fromHex[:int16] == -29194'i16
    doAssert s.fromHex[:uint64] == 305499638'u64

  let p = parseutils.parseHex(s, result)
  if p != s.len or p == 0:
    raise newException(ValueError, "invalid hex integer: " & s)

func intToStr*(x: int, minchars: Positive = 1): string {.rtl,
    extern: "nsuIntToStr".} =
  ## Converts `x` to its decimal representation.
  ##
  ## The resulting string will be minimally `minchars` characters long. This is
  ## achieved by adding leading zeros.
  runnableExamples:
    doAssert intToStr(1984) == "1984"
    doAssert intToStr(1984, 6) == "001984"
  result = $abs(x)
  for i in 1 .. minchars - len(result):
    result = '0' & result
  if x < 0:
    result = '-' & result

func parseInt*(s: string): int {.rtl, extern: "nsuParseInt".} =
  ## Parses a decimal integer value contained in `s`.
  ##
  ## If `s` is not a valid integer, `ValueError` is raised.
  runnableExamples:
    doAssert parseInt("-0042") == -42
  result = 0
  let L = parseutils.parseInt(s, result, 0)
  if L != s.len or L == 0:
    raise newException(ValueError, "invalid integer: " & s)

func parseBiggestInt*(s: string): BiggestInt {.rtl,
    extern: "nsuParseBiggestInt".} =
  ## Parses a decimal integer value contained in `s`.
  ##
  ## If `s` is not a valid integer, `ValueError` is raised.
  result = BiggestInt(0)
  let L = parseutils.parseBiggestInt(s, result, 0)
  if L != s.len or L == 0:
    raise newException(ValueError, "invalid integer: " & s)

func parseUInt*(s: string): uint {.rtl, extern: "nsuParseUInt".} =
  ## Parses a decimal unsigned integer value contained in `s`.
  ##
  ## If `s` is not a valid integer, `ValueError` is raised.
  result = uint(0)
  let L = parseutils.parseUInt(s, result, 0)
  if L != s.len or L == 0:
    raise newException(ValueError, "invalid unsigned integer: " & s)

func parseBiggestUInt*(s: string): BiggestUInt {.rtl,
    extern: "nsuParseBiggestUInt".} =
  ## Parses a decimal unsigned integer value contained in `s`.
  ##
  ## If `s` is not a valid integer, `ValueError` is raised.
  result = BiggestUInt(0)
  let L = parseutils.parseBiggestUInt(s, result, 0)
  if L != s.len or L == 0:
    raise newException(ValueError, "invalid unsigned integer: " & s)

func parseFloat*(s: string): float {.rtl, extern: "nsuParseFloat".} =
  ## Parses a decimal floating point value contained in `s`.
  ##
  ## If `s` is not a valid floating point number, `ValueError` is raised.
  ##`NAN`, `INF`, `-INF` are also supported (case insensitive comparison).
  runnableExamples:
    doAssert parseFloat("3.14") == 3.14
    doAssert parseFloat("inf") == 1.0/0
  result = 0.0
  let L = parseutils.parseFloat(s, result, 0)
  if L != s.len or L == 0:
    raise newException(ValueError, "invalid float: " & s)

func parseBinInt*(s: string): int {.rtl, extern: "nsuParseBinInt".} =
  ## Parses a binary integer value contained in `s`.
  ##
  ## If `s` is not a valid binary integer, `ValueError` is raised. `s` can have
  ## one of the following optional prefixes: `0b`, `0B`. Underscores within
  ## `s` are ignored.
  runnableExamples:
    let
      a = "0b11_0101"
      b = "111"
    doAssert a.parseBinInt() == 53
    doAssert b.parseBinInt() == 7

  result = 0
  let L = parseutils.parseBin(s, result, 0)
  if L != s.len or L == 0:
    raise newException(ValueError, "invalid binary integer: " & s)

func parseOctInt*(s: string): int {.rtl, extern: "nsuParseOctInt".} =
  ## Parses an octal integer value contained in `s`.
  ##
  ## If `s` is not a valid oct integer, `ValueError` is raised. `s` can have one
  ## of the following optional prefixes: `0o`, `0O`.  Underscores within
  ## `s` are ignored.
  result = 0
  let L = parseutils.parseOct(s, result, 0)
  if L != s.len or L == 0:
    raise newException(ValueError, "invalid oct integer: " & s)

func parseHexInt*(s: string): int {.rtl, extern: "nsuParseHexInt".} =
  ## Parses a hexadecimal integer value contained in `s`.
  ##
  ## If `s` is not a valid hex integer, `ValueError` is raised. `s` can have one
  ## of the following optional prefixes: `0x`, `0X`, `#`.  Underscores
  ## within `s` are ignored.
  result = 0
  let L = parseutils.parseHex(s, result, 0)
  if L != s.len or L == 0:
    raise newException(ValueError, "invalid hex integer: " & s)

func generateHexCharToValueMap(): string =
  ## Generates a string to map a hex digit to uint value.
  result = ""
  for inp in 0..255:
    let ch = chr(inp)
    let o =
      case ch
      of '0'..'9': inp - ord('0')
      of 'a'..'f': inp - ord('a') + 10
      of 'A'..'F': inp - ord('A') + 10
      else: 17 # indicates an invalid hex char
    result.add chr(o)

const hexCharToValueMap = generateHexCharToValueMap()

func parseHexStr*(s: string): string {.rtl, extern: "nsuParseHexStr".} =
  ## Converts hex-encoded string to byte string, e.g.:
  ##
  ## Raises `ValueError` for an invalid hex values. The comparison is
  ## case-insensitive.
  ##
  ## See also:
  ## * `toHex func<#toHex,string>`_ for the reverse operation
  runnableExamples:
    let
      a = "41"
      b = "3161"
      c = "00ff"
    doAssert parseHexStr(a) == "A"
    doAssert parseHexStr(b) == "1a"
    doAssert parseHexStr(c) == "\0\255"

  if s.len mod 2 != 0:
    raise newException(ValueError, "Incorrect hex string len")
  result = newString(s.len div 2)
  var buf = 0
  for pos, c in s:
    let val = hexCharToValueMap[ord(c)].ord
    if val == 17:
      raise newException(ValueError, "Invalid hex char `" &
                         c & "` (ord " & $c.ord & ")")
    if pos mod 2 == 0:
      buf = val
    else:
      result[pos div 2] = chr(val + buf shl 4)

func parseBool*(s: string): bool =
  ## Parses a value into a `bool`.
  ##
  ## If `s` is one of the following values: `y, yes, true, 1, on`, then
  ## returns `true`. If `s` is one of the following values: `n, no, false,
  ## 0, off`, then returns `false`.  If `s` is something else a
  ## `ValueError` exception is raised.
  runnableExamples:
    let a = "n"
    doAssert parseBool(a) == false

  case normalize(s)
  of "y", "yes", "true", "1", "on": result = true
  of "n", "no", "false", "0", "off": result = false
  else: raise newException(ValueError, "cannot interpret as a bool: " & s)

func parseEnum*[T: enum](s: string): T =
  ## Parses an enum `T`. This errors at compile time, if the given enum
  ## type contains multiple fields with the same string value.
  ##
  ## Raises `ValueError` for an invalid value in `s`. The comparison is
  ## done in a style insensitive way.
  runnableExamples:
    type
      MyEnum = enum
        first = "1st",
        second,
        third = "3rd"

    doAssert parseEnum[MyEnum]("1_st") == first
    doAssert parseEnum[MyEnum]("second") == second
    doAssertRaises(ValueError):
      echo parseEnum[MyEnum]("third")

  genEnumCaseStmt(T, s, default = nil, ord(low(T)), ord(high(T)), nimIdentNormalize)

func parseEnum*[T: enum](s: string, default: T): T =
  ## Parses an enum `T`. This errors at compile time, if the given enum
  ## type contains multiple fields with the same string value.
  ##
  ## Uses `default` for an invalid value in `s`. The comparison is done in a
  ## style insensitive way.
  runnableExamples:
    type
      MyEnum = enum
        first = "1st",
        second,
        third = "3rd"

    doAssert parseEnum[MyEnum]("1_st") == first
    doAssert parseEnum[MyEnum]("second") == second
    doAssert parseEnum[MyEnum]("last", third) == third

  genEnumCaseStmt(T, s, default, ord(low(T)), ord(high(T)), nimIdentNormalize)

func repeat*(c: char, count: Natural): string {.rtl, extern: "nsuRepeatChar".} =
  ## Returns a string of length `count` consisting only of
  ## the character `c`.
  runnableExamples:
    let a = 'z'
    doAssert a.repeat(5) == "zzzzz"
  result = newString(count)
  for i in 0..count-1: result[i] = c

func repeat*(s: string, n: Natural): string {.rtl, extern: "nsuRepeatStr".} =
  ## Returns string `s` concatenated `n` times.
  runnableExamples:
    doAssert "+ foo +".repeat(3) == "+ foo ++ foo ++ foo +"

  result = newStringOfCap(n * s.len)
  for i in 1..n: result.add(s)

func spaces*(n: Natural): string {.inline.} =
  ## Returns a string with `n` space characters. You can use this func
  ## to left align strings.
  ##
  ## See also:
  ## * `align func<#align,string,Natural,char>`_
  ## * `alignLeft func<#alignLeft,string,Natural,char>`_
  ## * `indent func<#indent,string,Natural,string>`_
  ## * `center func<#center,string,int,char>`_
  runnableExamples:
    let
      width = 15
      text1 = "Hello user!"
      text2 = "This is a very long string"
    doAssert text1 & spaces(max(0, width - text1.len)) & "|" ==
             "Hello user!    |"
    doAssert text2 & spaces(max(0, width - text2.len)) & "|" ==
             "This is a very long string|"
  repeat(' ', n)

func align*(s: string, count: Natural, padding = ' '): string {.rtl,
    extern: "nsuAlignString".} =
  ## Aligns a string `s` with `padding`, so that it is of length `count`.
  ##
  ## `padding` characters (by default spaces) are added before `s` resulting in
  ## right alignment. If `s.len >= count`, no spaces are added and `s` is
  ## returned unchanged. If you need to left align a string use the `alignLeft
  ## func<#alignLeft,string,Natural,char>`_.
  ##
  ## See also:
  ## * `alignLeft func<#alignLeft,string,Natural,char>`_
  ## * `spaces func<#spaces,Natural>`_
  ## * `indent func<#indent,string,Natural,string>`_
  ## * `center func<#center,string,int,char>`_
  runnableExamples:
    assert align("abc", 4) == " abc"
    assert align("a", 0) == "a"
    assert align("1232", 6) == "  1232"
    assert align("1232", 6, '#') == "##1232"
  if s.len < count:
    result = newString(count)
    let spaces = count - s.len
    for i in 0..spaces-1: result[i] = padding
    for i in spaces..count-1: result[i] = s[i-spaces]
  else:
    result = s

func alignLeft*(s: string, count: Natural, padding = ' '): string =
  ## Left-Aligns a string `s` with `padding`, so that it is of length `count`.
  ##
  ## `padding` characters (by default spaces) are added after `s` resulting in
  ## left alignment. If `s.len >= count`, no spaces are added and `s` is
  ## returned unchanged. If you need to right align a string use the `align
  ## func<#align,string,Natural,char>`_.
  ##
  ## See also:
  ## * `align func<#align,string,Natural,char>`_
  ## * `spaces func<#spaces,Natural>`_
  ## * `indent func<#indent,string,Natural,string>`_
  ## * `center func<#center,string,int,char>`_
  runnableExamples:
    assert alignLeft("abc", 4) == "abc "
    assert alignLeft("a", 0) == "a"
    assert alignLeft("1232", 6) == "1232  "
    assert alignLeft("1232", 6, '#') == "1232##"
  if s.len < count:
    result = newString(count)
    if s.len > 0:
      result[0 .. (s.len - 1)] = s
    for i in s.len ..< count:
      result[i] = padding
  else:
    result = s

func center*(s: string, width: int, fillChar: char = ' '): string {.rtl,
    extern: "nsuCenterString".} =
  ## Return the contents of `s` centered in a string `width` long using
  ## `fillChar` (default: space) as padding.
  ##
  ## The original string is returned if `width` is less than or equal
  ## to `s.len`.
  ##
  ## See also:
  ## * `align func<#align,string,Natural,char>`_
  ## * `alignLeft func<#alignLeft,string,Natural,char>`_
  ## * `spaces func<#spaces,Natural>`_
  ## * `indent func<#indent,string,Natural,string>`_
  runnableExamples:
    let a = "foo"
    doAssert a.center(2) == "foo"
    doAssert a.center(5) == " foo "
    doAssert a.center(6) == " foo  "
  if width <= s.len: return s
  result = newString(width)
  # Left padding will be one fillChar
  # smaller if there are an odd number
  # of characters
  let
    charsLeft = (width - s.len)
    leftPadding = charsLeft div 2
  for i in 0 ..< width:
    if i >= leftPadding and i < leftPadding + s.len:
      # we are where the string should be located
      result[i] = s[i-leftPadding]
    else:
      # we are either before or after where
      # the string s should go
      result[i] = fillChar

func indent*(s: string, count: Natural, padding: string = " "): string {.rtl,
    extern: "nsuIndent".} =
  ## Indents each line in `s` by `count` amount of `padding`.
  ##
  ## **Note:** This does not preserve the new line characters used in `s`.
  ##
  ## See also:
  ## * `align func<#align,string,Natural,char>`_
  ## * `alignLeft func<#alignLeft,string,Natural,char>`_
  ## * `spaces func<#spaces,Natural>`_
  ## * `unindent func<#unindent,string,Natural,string>`_
  ## * `dedent func<#dedent,string,Natural>`_
  runnableExamples:
    doAssert indent("First line\c\l and second line.", 2) ==
             "  First line\l   and second line."
  result = ""
  var i = 0
  for line in s.splitLines():
    if i != 0:
      result.add("\n")
    for j in 1..count:
      result.add(padding)
    result.add(line)
    i.inc

func unindent*(s: string, count: Natural = int.high,
               padding: string = " "): string {.rtl, extern: "nsuUnindent".} =
  ## Unindents each line in `s` by `count` amount of `padding`.
  ##
  ## **Note:** This does not preserve the new line characters used in `s`.
  ##
  ## See also:
  ## * `dedent func<#dedent,string,Natural>`_
  ## * `align func<#align,string,Natural,char>`_
  ## * `alignLeft func<#alignLeft,string,Natural,char>`_
  ## * `spaces func<#spaces,Natural>`_
  ## * `indent func<#indent,string,Natural,string>`_
  runnableExamples:
    let x = """
      Hello
        There
    """.unindent()

    doAssert x == "Hello\nThere\n"
  result = ""
  var i = 0
  for line in s.splitLines():
    if i != 0:
      result.add("\n")
    var indentCount = 0
    for j in 0..<count.int:
      indentCount.inc
      if j + padding.len-1 >= line.len or line[j .. j + padding.len-1] != padding:
        indentCount = j
        break
    result.add(line[indentCount*padding.len .. ^1])
    i.inc

func indentation*(s: string): Natural {.since: (1, 3).} =
  ## Returns the amount of indentation all lines of `s` have in common,
  ## ignoring lines that consist only of whitespace.
  result = int.high
  for line in s.splitLines:
    for i, c in line:
      if i >= result: break
      elif c != ' ':
        result = i
        break
  if result == int.high:
    result = 0

func dedent*(s: string, count: Natural = indentation(s)): string {.rtl,
    extern: "nsuDedent", since: (1, 3).} =
  ## Unindents each line in `s` by `count` amount of `padding`.
  ## The only difference between this and the
  ## `unindent func<#unindent,string,Natural,string>`_ is that this by default
  ## only cuts off the amount of indentation that all lines of `s` share as
  ## opposed to all indentation. It only supports spaces as padding.
  ##
  ## **Note:** This does not preserve the new line characters used in `s`.
  ##
  ## See also:
  ## * `unindent func<#unindent,string,Natural,string>`_
  ## * `align func<#align,string,Natural,char>`_
  ## * `alignLeft func<#alignLeft,string,Natural,char>`_
  ## * `spaces func<#spaces,Natural>`_
  ## * `indent func<#indent,string,Natural,string>`_
  runnableExamples:
    let x = """
      Hello
        There
    """.dedent()

    doAssert x == "Hello\n  There\n"
  unindent(s, count, " ")

func delete*(s: var string, slice: Slice[int]) =
  ## Deletes the items `s[slice]`, raising `IndexDefect` if the slice contains
  ## elements out of range.
  ##
  ## This operation moves all elements after `s[slice]` in linear time, and
  ## is the string analog to `sequtils.delete`.
  runnableExamples:
    var a = "abcde"
    doAssertRaises(IndexDefect): a.delete(4..5)
    assert a == "abcde"
    a.delete(4..4)
    assert a == "abcd"
    a.delete(1..2)
    assert a == "ad"
    a.delete(1..<1) # empty slice
    assert a == "ad"
  when compileOption("boundChecks"):
    if (slice.a > s.len) or (slice.a < 0) or (slice.b >= s.len):
      raise newException(IndexDefect, $(slice: slice, len: s.len))
  if slice.b >= slice.a:
    var i = slice.a
    var j = slice.b + 1
    var newLen = s.len - j + i
    # if j < s.len: moveMem(addr s[i], addr s[j], s.len - j) # pending benchmark
    while i < newLen:
      s[i] = s[j]
      inc(i)
      inc(j)
    setLen(s, newLen)

func startsWith*(s: string, prefix: char): bool {.inline.} =
  ## Returns true if `s` starts with character `prefix`.
  ##
  ## See also:
  ## * `endsWith func<#endsWith,string,char>`_
  ## * `continuesWith func<#continuesWith,string,string,Natural>`_
  ## * `removePrefix func<#removePrefix,string,char>`_
  runnableExamples:
    let a = "abracadabra"
    doAssert a.startsWith('a') == true
    doAssert a.startsWith('b') == false
  result = s.len > 0 and s[0] == prefix

func startsWith*(s, prefix: string): bool {.rtl, extern: "nsuStartsWith".} =
  ## Returns true if `s` starts with string `prefix`.
  ##
  ## If `prefix == ""` true is returned.
  ##
  ## See also:
  ## * `endsWith func<#endsWith,string,string>`_
  ## * `continuesWith func<#continuesWith,string,string,Natural>`_
  ## * `removePrefix func<#removePrefix,string,string>`_
  runnableExamples:
    let a = "abracadabra"
    doAssert a.startsWith("abra") == true
    doAssert a.startsWith("bra") == false
  startsWithImpl(s, prefix)

func endsWith*(s: string, suffix: char): bool {.inline.} =
  ## Returns true if `s` ends with `suffix`.
  ##
  ## See also:
  ## * `startsWith func<#startsWith,string,char>`_
  ## * `continuesWith func<#continuesWith,string,string,Natural>`_
  ## * `removeSuffix func<#removeSuffix,string,char>`_
  runnableExamples:
    let a = "abracadabra"
    doAssert a.endsWith('a') == true
    doAssert a.endsWith('b') == false
  result = s.len > 0 and s[s.high] == suffix

func endsWith*(s, suffix: string): bool {.rtl, extern: "nsuEndsWith".} =
  ## Returns true if `s` ends with `suffix`.
  ##
  ## If `suffix == ""` true is returned.
  ##
  ## See also:
  ## * `startsWith func<#startsWith,string,string>`_
  ## * `continuesWith func<#continuesWith,string,string,Natural>`_
  ## * `removeSuffix func<#removeSuffix,string,string>`_
  runnableExamples:
    let a = "abracadabra"
    doAssert a.endsWith("abra") == true
    doAssert a.endsWith("dab") == false
  endsWithImpl(s, suffix)

func continuesWith*(s, substr: string, start: Natural): bool {.rtl,
    extern: "nsuContinuesWith".} =
  ## Returns true if `s` continues with `substr` at position `start`.
  ##
  ## If `substr == ""` true is returned.
  ##
  ## See also:
  ## * `startsWith func<#startsWith,string,string>`_
  ## * `endsWith func<#endsWith,string,string>`_
  runnableExamples:
    let a = "abracadabra"
    doAssert a.continuesWith("ca", 4) == true
    doAssert a.continuesWith("ca", 5) == false
    doAssert a.continuesWith("dab", 6) == true
  var i = 0
  while true:
    if i >= substr.len: return true
    if i+start >= s.len or s[i+start] != substr[i]: return false
    inc(i)


func removePrefix*(s: var string, chars: set[char] = Newlines) {.rtl,
    extern: "nsuRemovePrefixCharSet".} =
  ## Removes all characters from `chars` from the start of the string `s`
  ## (in-place).
  ##
  ## See also:
  ## * `removeSuffix func<#removeSuffix,string,set[char]>`_
  runnableExamples:
    var userInput = "\r\n*~Hello World!"
    userInput.removePrefix
    doAssert userInput == "*~Hello World!"
    userInput.removePrefix({'~', '*'})
    doAssert userInput == "Hello World!"

    var otherInput = "?!?Hello!?!"
    otherInput.removePrefix({'!', '?'})
    doAssert otherInput == "Hello!?!"

  var start = 0
  while start < s.len and s[start] in chars: start += 1
  if start > 0: s.delete(0 ..< start)

func removePrefix*(s: var string, c: char) {.rtl,
    extern: "nsuRemovePrefixChar".} =
  ## Removes all occurrences of a single character (in-place) from the start
  ## of a string.
  ##
  ## See also:
  ## * `removeSuffix func<#removeSuffix,string,char>`_
  ## * `startsWith func<#startsWith,string,char>`_
  runnableExamples:
    var ident = "pControl"
    ident.removePrefix('p')
    doAssert ident == "Control"
  removePrefix(s, chars = {c})

func removePrefix*(s: var string, prefix: string) {.rtl,
    extern: "nsuRemovePrefixString".} =
  ## Remove the first matching prefix (in-place) from a string.
  ##
  ## See also:
  ## * `removeSuffix func<#removeSuffix,string,string>`_
  ## * `startsWith func<#startsWith,string,string>`_
  runnableExamples:
    var answers = "yesyes"
    answers.removePrefix("yes")
    doAssert answers == "yes"
  if s.startsWith(prefix):
    s.delete(0 ..< prefix.len)

func removeSuffix*(s: var string, chars: set[char] = Newlines) {.rtl,
    extern: "nsuRemoveSuffixCharSet".} =
  ## Removes all characters from `chars` from the end of the string `s`
  ## (in-place).
  ##
  ## See also:
  ## * `removePrefix func<#removePrefix,string,set[char]>`_
  runnableExamples:
    var userInput = "Hello World!*~\r\n"
    userInput.removeSuffix
    doAssert userInput == "Hello World!*~"
    userInput.removeSuffix({'~', '*'})
    doAssert userInput == "Hello World!"

    var otherInput = "Hello!?!"
    otherInput.removeSuffix({'!', '?'})
    doAssert otherInput == "Hello"

  if s.len == 0: return
  var last = s.high
  while last > -1 and s[last] in chars: last -= 1
  s.setLen(last + 1)

func removeSuffix*(s: var string, c: char) {.rtl,
    extern: "nsuRemoveSuffixChar".} =
  ## Removes all occurrences of a single character (in-place) from the end
  ## of a string.
  ##
  ## See also:
  ## * `removePrefix func<#removePrefix,string,char>`_
  ## * `endsWith func<#endsWith,string,char>`_
  runnableExamples:
    var table = "users"
    table.removeSuffix('s')
    doAssert table == "user"

    var dots = "Trailing dots......."
    dots.removeSuffix('.')
    doAssert dots == "Trailing dots"

  removeSuffix(s, chars = {c})

func removeSuffix*(s: var string, suffix: string) {.rtl,
    extern: "nsuRemoveSuffixString".} =
  ## Remove the first matching suffix (in-place) from a string.
  ##
  ## See also:
  ## * `removePrefix func<#removePrefix,string,string>`_
  ## * `endsWith func<#endsWith,string,string>`_
  runnableExamples:
    var answers = "yeses"
    answers.removeSuffix("es")
    doAssert answers == "yes"
  var newLen = s.len
  if s.endsWith(suffix):
    newLen -= len(suffix)
    s.setLen(newLen)


func addSep*(dest: var string, sep = ", ", startLen: Natural = 0) {.inline.} =
  ## Adds a separator to `dest` only if its length is bigger than `startLen`.
  ##
  ## A shorthand for:
  ##
  ## .. code-block:: nim
  ##   if dest.len > startLen: add(dest, sep)
  ##
  ## This is often useful for generating some code where the items need to
  ## be *separated* by `sep`. `sep` is only added if `dest` is longer than
  ## `startLen`. The following example creates a string describing
  ## an array of integers.
  runnableExamples:
    var arr = "["
    for x in items([2, 3, 5, 7, 11]):
      addSep(arr, startLen = len("["))
      add(arr, $x)
    add(arr, "]")
    doAssert arr == "[2, 3, 5, 7, 11]"

  if dest.len > startLen: add(dest, sep)

func allCharsInSet*(s: string, theSet: set[char]): bool =
  ## Returns true if every character of `s` is in the set `theSet`.
  runnableExamples:
    doAssert allCharsInSet("aeea", {'a', 'e'}) == true
    doAssert allCharsInSet("", {'a', 'e'}) == true

  for c in items(s):
    if c notin theSet: return false
  return true

func abbrev*(s: string, possibilities: openArray[string]): int =
  ## Returns the index of the first item in `possibilities` which starts
  ## with `s`, if not ambiguous.
  ##
  ## Returns -1 if no item has been found and -2 if multiple items match.
  runnableExamples:
    doAssert abbrev("fac", ["college", "faculty", "industry"]) == 1
    doAssert abbrev("foo", ["college", "faculty", "industry"]) == -1 # Not found
    doAssert abbrev("fac", ["college", "faculty", "faculties"]) == -2 # Ambiguous
    doAssert abbrev("college", ["college", "colleges", "industry"]) == 0

  result = -1 # none found
  for i in 0..possibilities.len-1:
    if possibilities[i].startsWith(s):
      if possibilities[i] == s:
        # special case: exact match shouldn't be ambiguous
        return i
      if result >= 0: return -2 # ambiguous
      result = i

# ---------------------------------------------------------------------------

func join*(a: openArray[string], sep: string = ""): string {.rtl,
    extern: "nsuJoinSep".} =
  ## Concatenates all strings in the container `a`, separating them with `sep`.
  runnableExamples:
    doAssert join(["A", "B", "Conclusion"], " -> ") == "A -> B -> Conclusion"

  if len(a) > 0:
    var L = sep.len * (a.len-1)
    for i in 0..high(a): inc(L, a[i].len)
    result = newStringOfCap(L)
    add(result, a[0])
    for i in 1..high(a):
      add(result, sep)
      add(result, a[i])
  else:
    result = ""

func join*[T: not string](a: openArray[T], sep: string = ""): string =
  ## Converts all elements in the container `a` to strings using `$`,
  ## and concatenates them with `sep`.
  runnableExamples:
    doAssert join([1, 2, 3], " -> ") == "1 -> 2 -> 3"

  result = ""
  for i, x in a:
    if i > 0:
      add(result, sep)
    add(result, $x)

type
  SkipTable* = array[char, int]

func initSkipTable*(a: var SkipTable, sub: string) {.rtl,
    extern: "nsuInitSkipTable".} =
  ## Preprocess table `a` for `sub`.
  let m = len(sub)
  fill(a, m)

  for i in 0 ..< m - 1:
    a[sub[i]] = m - 1 - i

func find*(a: SkipTable, s, sub: string, start: Natural = 0, last = 0): int {.
    rtl, extern: "nsuFindStrA".} =
  ## Searches for `sub` in `s` inside range `start..last` using preprocessed
  ## table `a`. If `last` is unspecified, it defaults to `s.high` (the last
  ## element).
  ##
  ## Searching is case-sensitive. If `sub` is not in `s`, -1 is returned.
  let
    last = if last == 0: s.high else: last
    subLast = sub.len - 1

  if subLast == -1:
    # this was an empty needle string,
    # we count this as match in the first possible position:
    return start

  # This is an implementation of the Boyer-Moore Horspool algorithms
  # https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm
  var skip = start

  while last - skip >= subLast:
    var i = subLast
    while s[skip + i] == sub[i]:
      if i == 0:
        return skip
      dec i
    inc skip, a[s[skip + subLast]]
  return -1

when not (defined(js) or defined(nimdoc) or defined(nimscript)):
  func c_memchr(cstr: pointer, c: char, n: csize_t): pointer {.
                importc: "memchr", header: "<string.h>".}
  func c_strstr(haystack, needle: cstring): cstring {.
    importc: "strstr", header: "<string.h>".}

  const hasCStringBuiltin = true
else:
  const hasCStringBuiltin = false

func find*(s: string, sub: char, start: Natural = 0, last = 0): int {.rtl,
    extern: "nsuFindChar".} =
  ## Searches for `sub` in `s` inside range `start..last` (both ends included).
  ## If `last` is unspecified, it defaults to `s.high` (the last element).
  ##
  ## Searching is case-sensitive. If `sub` is not in `s`, -1 is returned.
  ## Otherwise the index returned is relative to `s[0]`, not `start`.
  ## Use `s[start..last].rfind` for a `start`-origin index.
  ##
  ## See also:
  ## * `rfind func<#rfind,string,char,Natural,int>`_
  ## * `replace func<#replace,string,char,char>`_
  let last = if last == 0: s.high else: last
  when nimvm:
    for i in int(start)..last:
      if sub == s[i]: return i
  else:
    when hasCStringBuiltin:
      let L = last-start+1
      if L > 0:
        let found = c_memchr(s[start].unsafeAddr, sub, cast[csize_t](L))
        if not found.isNil:
          return cast[ByteAddress](found) -% cast[ByteAddress](s.cstring)
    else:
      for i in int(start)..last:
        if sub == s[i]: return i
  return -1

func find*(s: string, chars: set[char], start: Natural = 0, last = 0): int {.
    rtl, extern: "nsuFindCharSet".} =
  ## Searches for `chars` in `s` inside range `start..last` (both ends included).
  ## If `last` is unspecified, it defaults to `s.high` (the last element).
  ##
  ## If `s` contains none of the characters in `chars`, -1 is returned.
  ## Otherwise the index returned is relative to `s[0]`, not `start`.
  ## Use `s[start..last].find` for a `start`-origin index.
  ##
  ## See also:
  ## * `rfind func<#rfind,string,set[char],Natural,int>`_
  ## * `multiReplace func<#multiReplace,string,varargs[]>`_
  let last = if last == 0: s.high else: last
  for i in int(start)..last:
    if s[i] in chars: return i
  return -1

func find*(s, sub: string, start: Natural = 0, last = 0): int {.rtl,
    extern: "nsuFindStr".} =
  ## Searches for `sub` in `s` inside range `start..last` (both ends included).
  ## If `last` is unspecified, it defaults to `s.high` (the last element).
  ##
  ## Searching is case-sensitive. If `sub` is not in `s`, -1 is returned.
  ## Otherwise the index returned is relative to `s[0]`, not `start`.
  ## Use `s[start..last].find` for a `start`-origin index.
  ##
  ## See also:
  ## * `rfind func<#rfind,string,string,Natural,int>`_
  ## * `replace func<#replace,string,string,string>`_
  if sub.len > s.len - start: return -1
  if sub.len == 1: return find(s, sub[0], start, last)

  template useSkipTable {.dirty.} =
    var a {.noinit.}: SkipTable
    initSkipTable(a, sub)
    result = find(a, s, sub, start, last)

  when not hasCStringBuiltin:
    useSkipTable()
  else:
    when nimvm:
      useSkipTable()
    else:
      when hasCStringBuiltin:
        if last == 0 and s.len > start:
          let found = c_strstr(s[start].unsafeAddr, sub)
          if not found.isNil:
            result = cast[ByteAddress](found) -% cast[ByteAddress](s.cstring)
          else:
            result = -1
        else:
          useSkipTable()
      else:
        useSkipTable()

func rfind*(s: string, sub: char, start: Natural = 0, last = -1): int {.rtl,
    extern: "nsuRFindChar".} =
  ## Searches for `sub` in `s` inside range `start..last` (both ends included)
  ## in reverse -- starting at high indexes and moving lower to the first
  ## character or `start`.  If `last` is unspecified, it defaults to `s.high`
  ## (the last element).
  ##
  ## Searching is case-sensitive. If `sub` is not in `s`, -1 is returned.
  ## Otherwise the index returned is relative to `s[0]`, not `start`.
  ## Use `s[start..last].find` for a `start`-origin index.
  ##
  ## See also:
  ## * `find func<#find,string,char,Natural,int>`_
  let last = if last == -1: s.high else: last
  for i in countdown(last, start):
    if sub == s[i]: return i
  return -1

func rfind*(s: string, chars: set[char], start: Natural = 0, last = -1): int {.
    rtl, extern: "nsuRFindCharSet".} =
  ## Searches for `chars` in `s` inside range `start..last` (both ends
  ## included) in reverse -- starting at high indexes and moving lower to the
  ## first character or `start`. If `last` is unspecified, it defaults to
  ## `s.high` (the last element).
  ##
  ## If `s` contains none of the characters in `chars`, -1 is returned.
  ## Otherwise the index returned is relative to `s[0]`, not `start`.
  ## Use `s[start..last].rfind` for a `start`-origin index.
  ##
  ## See also:
  ## * `find func<#find,string,set[char],Natural,int>`_
  let last = if last == -1: s.high else: last
  for i in countdown(last, start):
    if s[i] in chars: return i
  return -1

func rfind*(s, sub: string, start: Natural = 0, last = -1): int {.rtl,
    extern: "nsuRFindStr".} =
  ## Searches for `sub` in `s` inside range `start..last` (both ends included)
  ## included) in reverse -- starting at high indexes and moving lower to the
  ## first character or `start`. If `last` is unspecified, it defaults to
  ## `s.high` (the last element).
  ##
  ## Searching is case-sensitive. If `sub` is not in `s`, -1 is returned.
  ## Otherwise the index returned is relative to `s[0]`, not `start`.
  ## Use `s[start..last].rfind` for a `start`-origin index.
  ##
  ## See also:
  ## * `find func<#find,string,string,Natural,int>`_
  if sub.len == 0:
    return -1
  if sub.len > s.len - start:
    return -1
  let last = if last == -1: s.high else: last
  result = 0
  for i in countdown(last - sub.len + 1, start):
    for j in 0..sub.len-1:
      result = i
      if sub[j] != s[i+j]:
        result = -1
        break
    if result != -1: return
  return -1


func count*(s: string, sub: char): int {.rtl, extern: "nsuCountChar".} =
  ## Counts the occurrences of the character `sub` in the string `s`.
  ##
  ## See also:
  ## * `countLines func<#countLines,string>`_
  result = 0
  for c in s:
    if c == sub: inc result

func count*(s: string, subs: set[char]): int {.rtl,
    extern: "nsuCountCharSet".} =
  ## Counts the occurrences of the group of character `subs` in the string `s`.
  ##
  ## See also:
  ## * `countLines func<#countLines,string>`_
  doAssert card(subs) > 0
  result = 0
  for c in s:
    if c in subs: inc result

func count*(s: string, sub: string, overlapping: bool = false): int {.rtl,
    extern: "nsuCountString".} =
  ## Counts the occurrences of a substring `sub` in the string `s`.
  ## Overlapping occurrences of `sub` only count when `overlapping`
  ## is set to true (default: false).
  ##
  ## See also:
  ## * `countLines func<#countLines,string>`_
  doAssert sub.len > 0
  result = 0
  var i = 0
  while true:
    i = s.find(sub, i)
    if i < 0: break
    if overlapping: inc i
    else: i += sub.len
    inc result

func countLines*(s: string): int {.rtl, extern: "nsuCountLines".} =
  ## Returns the number of lines in the string `s`.
  ##
  ## This is the same as `len(splitLines(s))`, but much more efficient
  ## because it doesn't modify the string creating temporary objects. Every
  ## `character literal <manual.html#lexical-analysis-character-literals>`_
  ## newline combination (CR, LF, CR-LF) is supported.
  ##
  ## In this context, a line is any string separated by a newline combination.
  ## A line can be an empty string.
  ##
  ## See also:
  ## * `splitLines func<#splitLines,string>`_
  runnableExamples:
    doAssert countLines("First line\l and second line.") == 2
  result = 1
  var i = 0
  while i < s.len:
    case s[i]
    of '\c':
      if i+1 < s.len and s[i+1] == '\l': inc i
      inc result
    of '\l': inc result
    else: discard
    inc i


func contains*(s, sub: string): bool =
  ## Same as `find(s, sub) >= 0`.
  ##
  ## See also:
  ## * `find func<#find,string,string,Natural,int>`_
  return find(s, sub) >= 0

func contains*(s: string, chars: set[char]): bool =
  ## Same as `find(s, chars) >= 0`.
  ##
  ## See also:
  ## * `find func<#find,string,set[char],Natural,int>`_
  return find(s, chars) >= 0

func replace*(s, sub: string, by = ""): string {.rtl,
    extern: "nsuReplaceStr".} =
  ## Replaces every occurrence of the string `sub` in `s` with the string `by`.
  ##
  ## See also:
  ## * `find func<#find,string,string,Natural,int>`_
  ## * `replace func<#replace,string,char,char>`_ for replacing
  ##   single characters
  ## * `replaceWord func<#replaceWord,string,string,string>`_
  ## * `multiReplace func<#multiReplace,string,varargs[]>`_
  result = ""
  let subLen = sub.len
  if subLen == 0:
    result = s
  elif subLen == 1:
    # when the pattern is a single char, we use a faster
    # char-based search that doesn't need a skip table:
    let c = sub[0]
    let last = s.high
    var i = 0
    while true:
      let j = find(s, c, i, last)
      if j < 0: break
      add result, substr(s, i, j - 1)
      add result, by
      i = j + subLen
    # copy the rest:
    add result, substr(s, i)
  else:
    var a {.noinit.}: SkipTable
    initSkipTable(a, sub)
    let last = s.high
    var i = 0
    while true:
      let j = find(a, s, sub, i, last)
      if j < 0: break
      add result, substr(s, i, j - 1)
      add result, by
      i = j + subLen
    # copy the rest:
    add result, substr(s, i)

func replace*(s: string, sub, by: char): string {.rtl,
    extern: "nsuReplaceChar".} =
  ## Replaces every occurrence of the character `sub` in `s` with the character
  ## `by`.
  ##
  ## Optimized version of `replace <#replace,string,string,string>`_ for
  ## characters.
  ##
  ## See also:
  ## * `find func<#find,string,char,Natural,int>`_
  ## * `replaceWord func<#replaceWord,string,string,string>`_
  ## * `multiReplace func<#multiReplace,string,varargs[]>`_
  result = newString(s.len)
  var i = 0
  while i < s.len:
    if s[i] == sub: result[i] = by
    else: result[i] = s[i]
    inc(i)

func replaceWord*(s, sub: string, by = ""): string {.rtl,
    extern: "nsuReplaceWord".} =
  ## Replaces every occurrence of the string `sub` in `s` with the string `by`.
  ##
  ## Each occurrence of `sub` has to be surrounded by word boundaries
  ## (comparable to `\b` in regular expressions), otherwise it is not
  ## replaced.
  if sub.len == 0: return s
  const wordChars = {'a'..'z', 'A'..'Z', '0'..'9', '_', '\128'..'\255'}
  var a {.noinit.}: SkipTable
  result = ""
  initSkipTable(a, sub)
  var i = 0
  let last = s.high
  let sublen = sub.len
  if sublen > 0:
    while true:
      var j = find(a, s, sub, i, last)
      if j < 0: break
      # word boundary?
      if (j == 0 or s[j-1] notin wordChars) and
          (j+sub.len >= s.len or s[j+sub.len] notin wordChars):
        add result, substr(s, i, j - 1)
        add result, by
        i = j + sublen
      else:
        add result, substr(s, i, j)
        i = j + 1
    # copy the rest:
    add result, substr(s, i)

func multiReplace*(s: string, replacements: varargs[(string, string)]): string =
  ## Same as replace, but specialized for doing multiple replacements in a single
  ## pass through the input string.
  ##
  ## `multiReplace` performs all replacements in a single pass, this means it
  ## can be used to swap the occurrences of "a" and "b", for instance.
  ##
  ## If the resulting string is not longer than the original input string,
  ## only a single memory allocation is required.
  ##
  ## The order of the replacements does matter. Earlier replacements are
  ## preferred over later replacements in the argument list.
  result = newStringOfCap(s.len)
  var i = 0
  var fastChk: set[char] = {}
  for sub, by in replacements.items:
    if sub.len > 0:
      # Include first character of all replacements
      fastChk.incl sub[0]
  while i < s.len:
    block sIteration:
      # Assume most chars in s are not candidates for any replacement operation
      if s[i] in fastChk:
        for sub, by in replacements.items:
          if sub.len > 0 and s.continuesWith(sub, i):
            add result, by
            inc(i, sub.len)
            break sIteration
      # No matching replacement found
      # copy current character from s
      add result, s[i]
      inc(i)



func insertSep*(s: string, sep = '_', digits = 3): string {.rtl,
    extern: "nsuInsertSep".} =
  ## Inserts the separator `sep` after `digits` characters (default: 3)
  ## from right to left.
  ##
  ## Even though the algorithm works with any string `s`, it is only useful
  ## if `s` contains a number.
  runnableExamples:
    doAssert insertSep("1000000") == "1_000_000"
  result = newStringOfCap(s.len)
  let hasPrefix = isDigit(s[s.low]) == false
  var idx:int
  if hasPrefix:
    result.add s[s.low]
    for i in (s.low + 1)..s.high:
      idx = i
      if not isDigit(s[i]):
        result.add s[i]
      else:
        break
  let partsLen = s.len - idx
  var L = (partsLen-1) div digits + partsLen
  result.setLen(L + idx)
  var j = 0
  dec(L)
  for i in countdown(partsLen-1,0):
    if j == digits:
      result[L + idx] = sep
      dec(L)
      j = 0
    result[L + idx] = s[i + idx]
    inc(j)
    dec(L)

func escape*(s: string, prefix = "\"", suffix = "\""): string {.rtl,
    extern: "nsuEscape".} =
  ## Escapes a string `s`.
  ##
  ## .. note:: The escaping scheme is different from
  ##    `system.addEscapedChar`.
  ##
  ## * replaces `'\0'..'\31'` and `'\127'..'\255'` by `\xHH` where `HH` is its hexadecimal value
  ## * replaces ``\`` by `\\`
  ## * replaces `'` by `\'`
  ## * replaces `"` by `\"`
  ##
  ## The resulting string is prefixed with `prefix` and suffixed with `suffix`.
  ## Both may be empty strings.
  ##
  ## See also:
  ## * `addEscapedChar proc<system.html#addEscapedChar,string,char>`_
  ## * `unescape func<#unescape,string,string,string>`_ for the opposite
  ##   operation
  result = newStringOfCap(s.len + s.len shr 2)
  result.add(prefix)
  for c in items(s):
    case c
    of '\0'..'\31', '\127'..'\255':
      add(result, "\\x")
      add(result, toHex(ord(c), 2))
    of '\\': add(result, "\\\\")
    of '\'': add(result, "\\'")
    of '\"': add(result, "\\\"")
    else: add(result, c)
  add(result, suffix)

func unescape*(s: string, prefix = "\"", suffix = "\""): string {.rtl,
    extern: "nsuUnescape".} =
  ## Unescapes a string `s`.
  ##
  ## This complements `escape func<#escape,string,string,string>`_
  ## as it performs the opposite operations.
  ##
  ## If `s` does not begin with `prefix` and end with `suffix` a
  ## ValueError exception will be raised.
  result = newStringOfCap(s.len)
  var i = prefix.len
  if not s.startsWith(prefix):
    raise newException(ValueError,
                       "String does not start with: " & prefix)
  while true:
    if i >= s.len-suffix.len: break
    if s[i] == '\\':
      if i+1 >= s.len:
        result.add('\\')
        break
      case s[i+1]:
      of 'x':
        inc i, 2
        var c = 0
        i += parseutils.parseHex(s, c, i, maxLen = 2)
        result.add(chr(c))
        dec i, 2
      of '\\':
        result.add('\\')
      of '\'':
        result.add('\'')
      of '\"':
        result.add('\"')
      else:
        result.add("\\" & s[i+1])
      inc(i, 2)
    else:
      result.add(s[i])
      inc(i)
  if not s.endsWith(suffix):
    raise newException(ValueError,
                       "String does not end in: " & suffix)

func validIdentifier*(s: string): bool {.rtl, extern: "nsuValidIdentifier".} =
  ## Returns true if `s` is a valid identifier.
  ##
  ## A valid identifier starts with a character of the set `IdentStartChars`
  ## and is followed by any number of characters of the set `IdentChars`.
  runnableExamples:
    doAssert "abc_def08".validIdentifier

  if s.len > 0 and s[0] in IdentStartChars:
    for i in 1..s.len-1:
      if s[i] notin IdentChars: return false
    return true


# floating point formatting:
when not defined(js):
  func c_sprintf(buf, frmt: cstring): cint {.header: "<stdio.h>",
                                     importc: "sprintf", varargs}

type
  FloatFormatMode* = enum
    ## The different modes of floating point formatting.
    ffDefault,   ## use the shorter floating point notation
    ffDecimal,   ## use decimal floating point notation
    ffScientific ## use scientific notation (using `e` character)

func formatBiggestFloat*(f: BiggestFloat, format: FloatFormatMode = ffDefault,
                         precision: range[-1..32] = 16;
                         decimalSep = '.'): string {.rtl, extern: "nsu$1".} =
  ## Converts a floating point value `f` to a string.
  ##
  ## If `format == ffDecimal` then precision is the number of digits to
  ## be printed after the decimal point.
  ## If `format == ffScientific` then precision is the maximum number
  ## of significant digits to be printed.
  ## `precision`'s default value is the maximum number of meaningful digits
  ## after the decimal point for Nim's `biggestFloat` type.
  ##
  ## If `precision == -1`, it tries to format it nicely.
  runnableExamples:
    let x = 123.456
    doAssert x.formatBiggestFloat() == "123.4560000000000"
    doAssert x.formatBiggestFloat(ffDecimal, 4) == "123.4560"
    doAssert x.formatBiggestFloat(ffScientific, 2) == "1.23e+02"
  when defined(js):
    var precision = precision
    if precision == -1:
      # use the same default precision as c_sprintf
      precision = 6
    var res: cstring
    case format
    of ffDefault:
      {.emit: "`res` = `f`.toString();".}
    of ffDecimal:
      {.emit: "`res` = `f`.toFixed(`precision`);".}
    of ffScientific:
      {.emit: "`res` = `f`.toExponential(`precision`);".}
    result = $res
    if 1.0 / f == -Inf:
      # JavaScript removes the "-" from negative Zero, add it back here
      result = "-" & $res
    for i in 0 ..< result.len:
      # Depending on the locale either dot or comma is produced,
      # but nothing else is possible:
      if result[i] in {'.', ','}: result[i] = decimalSep
  else:
    const floatFormatToChar: array[FloatFormatMode, char] = ['g', 'f', 'e']
    var
      frmtstr {.noinit.}: array[0..5, char]
      buf {.noinit.}: array[0..2500, char]
      L: cint
    frmtstr[0] = '%'
    if precision >= 0:
      frmtstr[1] = '#'
      frmtstr[2] = '.'
      frmtstr[3] = '*'
      frmtstr[4] = floatFormatToChar[format]
      frmtstr[5] = '\0'
      L = c_sprintf(addr buf, addr frmtstr, precision, f)
    else:
      frmtstr[1] = floatFormatToChar[format]
      frmtstr[2] = '\0'
      L = c_sprintf(addr buf, addr frmtstr, f)
    result = newString(L)
    for i in 0 ..< L:
      # Depending on the locale either dot or comma is produced,
      # but nothing else is possible:
      if buf[i] in {'.', ','}: result[i] = decimalSep
      else: result[i] = buf[i]
    when defined(windows):
      # VS pre 2015 violates the C standard: "The exponent always contains at
      # least two digits, and only as many more digits as necessary to
      # represent the exponent." [C11 §7.21.6.1]
      # The following post-processing fixes this behavior.
      if result.len > 4 and result[^4] == '+' and result[^3] == '0':
        result[^3] = result[^2]
        result[^2] = result[^1]
        result.setLen(result.len - 1)

func formatFloat*(f: float, format: FloatFormatMode = ffDefault,
                  precision: range[-1..32] = 16; decimalSep = '.'): string {.
                  rtl, extern: "nsu$1".} =
  ## Converts a floating point value `f` to a string.
  ##
  ## If `format == ffDecimal` then precision is the number of digits to
  ## be printed after the decimal point.
  ## If `format == ffScientific` then precision is the maximum number
  ## of significant digits to be printed.
  ## `precision`'s default value is the maximum number of meaningful digits
  ## after the decimal point for Nim's `float` type.
  ##
  ## If `precision == -1`, it tries to format it nicely.
  runnableExamples:
    let x = 123.456
    doAssert x.formatFloat() == "123.4560000000000"
    doAssert x.formatFloat(ffDecimal, 4) == "123.4560"
    doAssert x.formatFloat(ffScientific, 2) == "1.23e+02"

  result = formatBiggestFloat(f, format, precision, decimalSep)

func trimZeros*(x: var string; decimalSep = '.') =
  ## Trim trailing zeros from a formatted floating point
  ## value `x` (must be declared as `var`).
  ##
  ## This modifies `x` itself, it does not return a copy.
  runnableExamples:
    var x = "123.456000000"
    x.trimZeros()
    doAssert x == "123.456"

  let sPos = find(x, decimalSep)
  if sPos >= 0:
    var last = find(x, 'e', start = sPos)
    last = if last >= 0: last - 1 else: high(x)
    var pos = last
    while pos >= 0 and x[pos] == '0': dec(pos)
    if pos > sPos: inc(pos)
    x.delete(pos .. last)

type
  BinaryPrefixMode* = enum ## The different names for binary prefixes.
    bpIEC,                 # use the IEC/ISO standard prefixes such as kibi
    bpColloquial           # use the colloquial kilo, mega etc

func formatSize*(bytes: int64,
                 decimalSep = '.',
                 prefix = bpIEC,
                 includeSpace = false): string =
  ## Rounds and formats `bytes`.
  ##
  ## By default, uses the IEC/ISO standard binary prefixes, so 1024 will be
  ## formatted as 1KiB.  Set prefix to `bpColloquial` to use the colloquial
  ## names from the SI standard (e.g. k for 1000 being reused as 1024).
  ##
  ## `includeSpace` can be set to true to include the (SI preferred) space
  ## between the number and the unit (e.g. 1 KiB).
  ##
  ## See also:
  ## * `strformat module<strformat.html>`_ for string interpolation and formatting
  runnableExamples:
    doAssert formatSize((1'i64 shl 31) + (300'i64 shl 20)) == "2.293GiB"
    doAssert formatSize((2.234*1024*1024).int) == "2.234MiB"
    doAssert formatSize(4096, includeSpace = true) == "4 KiB"
    doAssert formatSize(4096, prefix = bpColloquial, includeSpace = true) == "4 kB"
    doAssert formatSize(4096) == "4KiB"
    doAssert formatSize(5_378_934, prefix = bpColloquial, decimalSep = ',') == "5,13MB"

  const iecPrefixes = ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi", "Yi"]
  const collPrefixes = ["", "k", "M", "G", "T", "P", "E", "Z", "Y"]
  var
    xb: int64 = bytes
    fbytes: float
    lastXb: int64 = bytes
    matchedIndex = 0
    prefixes: array[9, string]
  if prefix == bpColloquial:
    prefixes = collPrefixes
  else:
    prefixes = iecPrefixes

  # Iterate through prefixes seeing if value will be greater than
  # 0 in each case
  for index in 1..<prefixes.len:
    lastXb = xb
    xb = bytes div (1'i64 shl (index*10))
    matchedIndex = index
    if xb == 0:
      xb = lastXb
      matchedIndex = index - 1
      break
  # xb has the integer number for the latest value; index should be correct
  fbytes = bytes.float / (1'i64 shl (matchedIndex*10)).float
  result = formatFloat(fbytes, format = ffDecimal, precision = 3,
      decimalSep = decimalSep)
  result.trimZeros(decimalSep)
  if includeSpace:
    result &= " "
  result &= prefixes[matchedIndex]
  result &= "B"

func formatEng*(f: BiggestFloat,
                precision: range[0..32] = 10,
                trim: bool = true,
                siPrefix: bool = false,
                unit: string = "",
                decimalSep = '.',
                useUnitSpace = false): string =
  ## Converts a floating point value `f` to a string using engineering notation.
  ##
  ## Numbers in of the range -1000.0<f<1000.0 will be formatted without an
  ## exponent. Numbers outside of this range will be formatted as a
  ## significand in the range -1000.0<f<1000.0 and an exponent that will always
  ## be an integer multiple of 3, corresponding with the SI prefix scale k, M,
  ## G, T etc for numbers with an absolute value greater than 1 and m, μ, n, p
  ## etc for numbers with an absolute value less than 1.
  ##
  ## The default configuration (`trim=true` and `precision=10`) shows the
  ## **shortest** form that precisely (up to a maximum of 10 decimal places)
  ## displays the value. For example, 4.100000 will be displayed as 4.1 (which
  ## is mathematically identical) whereas 4.1000003 will be displayed as
  ## 4.1000003.
  ##
  ## If `trim` is set to true, trailing zeros will be removed; if false, the
  ## number of digits specified by `precision` will always be shown.
  ##
  ## `precision` can be used to set the number of digits to be shown after the
  ## decimal point or (if `trim` is true) the maximum number of digits to be
  ## shown.
  ##
  ## .. code-block:: nim
  ##
  ##    formatEng(0, 2, trim=false) == "0.00"
  ##    formatEng(0, 2) == "0"
  ##    formatEng(0.053, 0) == "53e-3"
  ##    formatEng(52731234, 2) == "52.73e6"
  ##    formatEng(-52731234, 2) == "-52.73e6"
  ##
  ## If `siPrefix` is set to true, the number will be displayed with the SI
  ## prefix corresponding to the exponent. For example 4100 will be displayed
  ## as "4.1 k" instead of "4.1e3". Note that `u` is used for micro- in place
  ## of the greek letter mu (μ) as per ISO 2955. Numbers with an absolute
  ## value outside of the range 1e-18<f<1000e18 (1a<f<1000E) will be displayed
  ## with an exponent rather than an SI prefix, regardless of whether
  ## `siPrefix` is true.
  ##
  ## If `useUnitSpace` is true, the provided unit will be appended to the string
  ## (with a space as required by the SI standard). This behaviour is slightly
  ## different to appending the unit to the result as the location of the space
  ## is altered depending on whether there is an exponent.
  ##
  ## .. code-block:: nim
  ##
  ##    formatEng(4100, siPrefix=true, unit="V") == "4.1 kV"
  ##    formatEng(4.1, siPrefix=true, unit="V") == "4.1 V"
  ##    formatEng(4.1, siPrefix=true) == "4.1" # Note lack of space
  ##    formatEng(4100, siPrefix=true) == "4.1 k"
  ##    formatEng(4.1, siPrefix=true, unit="") == "4.1 " # Space with unit=""
  ##    formatEng(4100, siPrefix=true, unit="") == "4.1 k"
  ##    formatEng(4100) == "4.1e3"
  ##    formatEng(4100, unit="V") == "4.1e3 V"
  ##    formatEng(4100, unit="", useUnitSpace=true) == "4.1e3 " # Space with useUnitSpace=true
  ##
  ## `decimalSep` is used as the decimal separator.
  ##
  ## See also:
  ## * `strformat module<strformat.html>`_ for string interpolation and formatting
  var
    absolute: BiggestFloat
    significand: BiggestFloat
    fexponent: BiggestFloat
    exponent: int
    splitResult: seq[string]
    suffix: string = ""
  func getPrefix(exp: int): char =
    ## Get the SI prefix for a given exponent
    ##
    ## Assumes exponent is a multiple of 3; returns ' ' if no prefix found
    const siPrefixes = ['a', 'f', 'p', 'n', 'u', 'm', ' ', 'k', 'M', 'G', 'T',
        'P', 'E']
    var index: int = (exp div 3) + 6
    result = ' '
    if index in low(siPrefixes)..high(siPrefixes):
      result = siPrefixes[index]

  # Most of the work is done with the sign ignored, so get the absolute value
  absolute = abs(f)
  significand = f

  if absolute == 0.0:
    # Simple case: just format it and force the exponent to 0
    exponent = 0
    result = significand.formatBiggestFloat(ffDecimal, precision,
        decimalSep = '.')
  else:
    # Find the best exponent that's a multiple of 3
    fexponent = floor(log10(absolute))
    fexponent = 3.0 * floor(fexponent / 3.0)
    # Adjust the significand for the new exponent
    significand /= pow(10.0, fexponent)

    # Adjust the significand and check whether it has affected
    # the exponent
    absolute = abs(significand)
    if absolute >= 1000.0:
      significand *= 0.001
      fexponent += 3
    # Components of the result:
    result = significand.formatBiggestFloat(ffDecimal, precision,
        decimalSep = '.')
    exponent = fexponent.int()

  splitResult = result.split('.')
  result = splitResult[0]
  # result should have at most one decimal character
  if splitResult.len() > 1:
    # If trim is set, we get rid of trailing zeros.  Don't use trimZeros here as
    # we can be a bit more efficient through knowledge that there will never be
    # an exponent in this part.
    if trim:
      while splitResult[1].endsWith("0"):
        # Trim last character
        splitResult[1].setLen(splitResult[1].len-1)
      if splitResult[1].len() > 0:
        result &= decimalSep & splitResult[1]
    else:
      result &= decimalSep & splitResult[1]

  # Combine the results accordingly
  if siPrefix and exponent != 0:
    var p = getPrefix(exponent)
    if p != ' ':
      suffix = " " & p
      exponent = 0 # Exponent replaced by SI prefix
  if suffix == "" and useUnitSpace:
    suffix = " "
  suffix &= unit
  if exponent != 0:
    result &= "e" & $exponent
  result &= suffix

func findNormalized(x: string, inArray: openArray[string]): int =
  var i = 0
  while i < high(inArray):
    if cmpIgnoreStyle(x, inArray[i]) == 0: return i
    inc(i, 2) # incrementing by 1 would probably lead to a
              # security hole...
  return -1

func invalidFormatString(explain: string) {.noinline.} =
  ## Raise value error for invalid interpolation format string
  raise newException(ValueError, "invalid format string - " & explain)

type
  AddfFragmentKind* = enum
    ## Kind of the `addf` interpolation fragment
    addfText ## Regular text fragment
    addfPositional ## Positional fragment `$#`
    addfIndexed ## Indexed fragment `$1`
    addfDollar ## Dollar literal `$$`
    addfBackIndexed ## Negative indexed fragment `$-1`
    addfVar ## Interpolated variable `$name`
    addfExpr ## Expression in braces `${some expr}`

  AddfFragment* = object
    ## `addf` format string fragment - can be used to write your own text
    ## interpolation logic.
    case kind*: AddfFragmentKind
      of addfText, addfVar, addfExpr:
        text*: string

      of addfIndexed, addfPositional, addfBackIndexed:
        idx*: int

      of addfDollar:
        discard

iterator addfFragments*(formatstr: string): AddfFragment =
  ## Iterate over interpolation fragments of the `formatstr`
  var i = 0
  var num = 0
  const PatternChars = {'a'..'z', 'A'..'Z', '0'..'9', '\128'..'\255', '_'}
  while i < len(formatstr):
    var frag: AddfFragment
    if formatstr[i] == '$' and i+1 < len(formatstr):
      case formatstr[i+1]
      of '#':
        frag = AddfFragment(kind: addfIndexed, idx: num)
        inc i, 2
        inc num

      of '$':
        inc(i, 2)
        frag = AddfFragment(kind: addfDollar)

      of '1'..'9', '-':
        var j = 0
        inc(i) # skip $
        let starti = i
        var negative = formatstr[i] == '-'
        if negative: inc i
        while i < formatstr.len and formatstr[i] in Digits:
          j = j * 10 + ord(formatstr[i]) - ord('0')
          inc(i)

        if negative:
          frag = AddfFragment(kind: addfBackIndexed, idx: j)

        else:
          frag = AddfFragment(kind: addfIndexed, idx: j - 1)

      of '{':
        var j = i+2
        var k = 0
        var negative = formatstr[j] == '-'
        let starti = j
        if negative: inc j
        var isNumber = 0
        while j < formatstr.len and formatstr[j] notin {'\0', '}'}:
          if formatstr[j] in Digits:
            k = k * 10 + ord(formatstr[j]) - ord('0')
            if isNumber == 0: isNumber = 1
          else:
            isNumber = -1
          inc(j)
        if isNumber == 1:
          if negative:
            frag = AddfFragment(kind: addfBackIndexed, idx: k)

          else:
            frag = AddfFragment(kind: addfIndexed, idx: k - 1)

        else:
          frag = AddfFragment(kind: addfExpr, text: substr(formatstr, i+2, j-1))

        i = j + 1

      of 'a'..'z', 'A'..'Z', '\128'..'\255', '_':
        var j = i+1
        while j < formatstr.len and formatstr[j] in PatternChars:
          inc(j)

        frag = AddfFragment(kind: addfVar, text: substr(formatstr, i+1, j-1))

        i = j

      else:
        invalidFormatString("unexpected char after $ - " & $formatstr[i + 1])

    else:
      var trange = i .. i
      while trange.b < formatstr.len and formatstr[trange.b] != '$':
        inc trange.b

      dec trange.b

      frag = AddfFragment(kind: addfText, text: formatstr[trange])

      i = trange.b
      inc(i)

    yield frag



func addf*(s: var string, formatstr: string, a: varargs[string, `$`]) {.rtl,
    extern: "nsuAddf".} =
  ## The same as `add(s, formatstr % a)`, but more efficient.
  for fr in addfFragments(formatstr):
    case fr.kind:
      of addfDollar:
        s.add '$'

      of addfPositional, addfIndexed, addfBackIndexed:
        let idx = if fr.kind == addfBackIndexed: len(a) - fr.idx else: fr.idx
        if not (0 <= idx and idx < a.len):
          invalidFormatString(
            "index for " & $idx & " is out of bounds for format arguments")

        s.add a[idx]

      of addfText:
        s.add fr.text

      of addfVar, addfExpr:
        var x = findNormalized(fr.text, a)
        if x >= 0 and x < high(a):
          add s, a[x+1]

        else:
          invalidFormatString(" no named interpolation argument")


func `%`*(formatstr: string, a: openArray[string]): string {.rtl,
    extern: "nsuFormatOpenArray".} =
  ## Interpolates a format string with the values from `a`.
  ##
  ## The `substitution`:idx: operator performs string substitutions in
  ## `formatstr` and returns a modified `formatstr`. This is often called
  ## `string interpolation`:idx:.
  ##
  ## This is best explained by an example:
  ##
  ## .. code-block:: nim
  ##   "$1 eats $2." % ["The cat", "fish"]
  ##
  ## Results in:
  ##
  ## .. code-block:: nim
  ##   "The cat eats fish."
  ##
  ## The substitution variables (the thing after the `$`) are enumerated
  ## from 1 to `a.len`.
  ## To produce a verbatim `$`, use `$$`.
  ## The notation `$#` can be used to refer to the next substitution
  ## variable:
  ##
  ## .. code-block:: nim
  ##   "$# eats $#." % ["The cat", "fish"]
  ##
  ## Substitution variables can also be words (that is
  ## `[A-Za-z_]+[A-Za-z0-9_]*`) in which case the arguments in `a` with even
  ## indices are keys and with odd indices are the corresponding values.
  ## An example:
  ##
  ## .. code-block:: nim
  ##   "$animal eats $food." % ["animal", "The cat", "food", "fish"]
  ##
  ## Results in:
  ##
  ## .. code-block:: nim
  ##   "The cat eats fish."
  ##
  ## The variables are compared with `cmpIgnoreStyle`. `ValueError` is
  ## raised if an ill-formed format string has been passed to the `%` operator.
  ##
  ## See also:
  ## * `strformat module<strformat.html>`_ for string interpolation and formatting
  result = newStringOfCap(formatstr.len + a.len shl 4)
  addf(result, formatstr, a)

func `%`*(formatstr, a: string): string {.rtl,
    extern: "nsuFormatSingleElem".} =
  ## This is the same as `formatstr % [a]` (see
  ## `% func<#%25,string,openArray[string]>`_).
  result = newStringOfCap(formatstr.len + a.len)
  addf(result, formatstr, [a])

func format*(formatstr: string, a: varargs[string, `$`]): string {.rtl,
    extern: "nsuFormatVarargs".} =
  ## This is the same as `formatstr % a` (see
  ## `% func<#%25,string,openArray[string]>`_) except that it supports
  ## auto stringification.
  ##
  ## See also:
  ## * `strformat module<strformat.html>`_ for string interpolation and formatting
  result = newStringOfCap(formatstr.len + a.len)
  addf(result, formatstr, a)


func strip*(s: string, leading = true, trailing = true,
            chars: set[char] = Whitespace): string {.rtl, extern: "nsuStrip".} =
  ## Strips leading or trailing `chars` (default: whitespace characters)
  ## from `s` and returns the resulting string.
  ##
  ## If `leading` is true (default), leading `chars` are stripped.
  ## If `trailing` is true (default), trailing `chars` are stripped.
  ## If both are false, the string is returned unchanged.
  ##
  ## See also:
  ## * `strip proc<strbasics.html#strip,string,set[char]>`_ Inplace version.
  ## * `stripLineEnd func<#stripLineEnd,string>`_
  runnableExamples:
    let a = "  vhellov   "
    let b = strip(a)
    doAssert b == "vhellov"

    doAssert a.strip(leading = false) == "  vhellov"
    doAssert a.strip(trailing = false) == "vhellov   "

    doAssert b.strip(chars = {'v'}) == "hello"
    doAssert b.strip(leading = false, chars = {'v'}) == "vhello"

    let c = "blaXbla"
    doAssert c.strip(chars = {'b', 'a'}) == "laXbl"
    doAssert c.strip(chars = {'b', 'a', 'l'}) == "X"

  var
    first = 0
    last = len(s)-1
  if leading:
    while first <= last and s[first] in chars: inc(first)
  if trailing:
    while last >= first and s[last] in chars: dec(last)
  result = substr(s, first, last)

func stripLineEnd*(s: var string) =
  ## Strips one of these suffixes from `s` in-place:
  ## `\r, \n, \r\n, \f, \v` (at most once instance).
  ## For example, can be useful in conjunction with `osproc.execCmdEx`.
  ## aka: `chomp`:idx:
  runnableExamples:
    var s = "foo\n\n"
    s.stripLineEnd
    doAssert s == "foo\n"
    s = "foo\r\n"
    s.stripLineEnd
    doAssert s == "foo"

  if s.len > 0:
    case s[^1]
    of '\n':
      if s.len > 1 and s[^2] == '\r':
        s.setLen s.len-2
      else:
        s.setLen s.len-1
    of '\r', '\v', '\f':
      s.setLen s.len-1
    else:
      discard


iterator tokenize*(s: string, seps: set[char] = Whitespace): tuple[
  token: string, isSep: bool] =
  ## Tokenizes the string `s` into substrings.
  ##
  ## Substrings are separated by a substring containing only `seps`.
  ## Example:
  ##
  ## .. code-block:: nim
  ##   for word in tokenize("  this is an  example  "):
  ##     writeLine(stdout, word)
  ##
  ## Results in:
  ##
  ## .. code-block:: nim
  ##   ("  ", true)
  ##   ("this", false)
  ##   (" ", true)
  ##   ("is", false)
  ##   (" ", true)
  ##   ("an", false)
  ##   ("  ", true)
  ##   ("example", false)
  ##   ("  ", true)
  var i = 0
  while true:
    var j = i
    var isSep = j < s.len and s[j] in seps
    while j < s.len and (s[j] in seps) == isSep: inc(j)
    if j > i:
      yield (substr(s, i, j-1), isSep)
    else:
      break
    i = j

func isEmptyOrWhitespace*(s: string): bool {.rtl,
    extern: "nsuIsEmptyOrWhitespace".} =
  ## Checks if `s` is empty or consists entirely of whitespace characters.
  result = s.allCharsInSet(Whitespace)

when isMainModule:
  for fr in addfFragments("$# $1 ${name} $-1 $4"):
    echo fr

  echo "$1" % "'_"
  echo "$1 $1" % "'_"
