discard """
  output: '''global = levB, arg = levA, test = false
levB'''
"""

# tstempl.nim
import std/strutils

type
  TLev = enum
    levA,
    levB

var abclev = levB

template tstLev(abclev: TLev) =
  bind tstempl.abclev, `%`
  echo "global = $1, arg = $2, test = $3" % [
    $tstempl.abclev, $abclev, $(tstempl.abclev == abclev)]
  # evaluates to true, but must be false


tstLev(levA)
echo $abclev
