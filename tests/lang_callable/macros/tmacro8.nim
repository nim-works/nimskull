# issue #8573

import std/[
  macros,
  strutils]

type
  LogSeverity = enum
    sevError = "Error"
    sevWarn  = "Warn"
    sevInfo  = "Info"
    sevDebug = "Debug"
  Colours = enum
    fgRed
    fgYellow
    fgWhite
    fgBlack

proc setForegroundColour(fg: Colours) = discard

macro log*(severity: static[LogSeverity], group: static[string], m: varargs[typed]): untyped =
  let sevStr   = align("[" & toUpperAscii($severity) & "] ", 8)
  let sevColor = case severity
    of sevError: fgRed
    of sevWarn:  fgYellow
    of sevInfo:  fgWhite
    of sevDebug: fgBlack

  let groupStr = "[" & $group & "] "

  result = quote do:
    setForegroundColour(sevColor) # <==
