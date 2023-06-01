## argument parser (akin to POSIX getopt)
## 
## Can handle:
## -c c_val
## -c:c_val
## -c=c_val
## -abcc_val
## -abc c_val
## -abc:c_val
## -abc=c_val
## --long val
## --long:val
## --long=val
## --long=    (empty value)
## -- -positional1 --positional2
## 
## -    is treated as positional
## --   skipped once; everything after this is treated as positional
## ---  is treated as long arg "-"
## 
## -:   undefined behavior (separator after dash)

import std/[options, strutils]

type
  OptKind* {.pure.} = enum
    short        ## -s
    long         ## --long
    positional   ## positional

  OptError* {.pure.} = enum
    none
    missing    ## missing value (after parsing all the args, last opt need value)
    extraneous ## extra value when optValNone

  Opt* = object
    case kind*: OptKind
    of short:
      keyShort*: char
    of long:
      keyLong*: string
    of positional:
      discard
    value*: string
    error*: OptError
      # abuse "feature": the default value is OptError.none

  OptValExpectation* = enum
    ## Should I expect this option to have a value or not?
    ## yes: -k=v
    ## no : -k   ("bare" option)
    optValRequired
      ## must have value
    optValNone
      ## must not have value
    optValOptional
      ## value is optional
      ## has : -k:v -k=v
      ## has : -kv  (please don't rely on this behavior)
      ## none: -k v

# xxx: trivial impl. remove this once == for case objects is implemented
proc `==`*(a, b: Opt): bool =
  a.kind == b.kind and a.value == b.value and (
    case a.kind:
    of short:
      a.keyShort == b.keyShort
    of long:
      a.keyLong == b.keyLong
    of positional:
      true
  )

proc get[T](explicitOnes: openArray[(T, OptValExpectation)], key: T,
    dflt: OptValExpectation): OptValExpectation =
  for (k, v) in explicitOnes:
    if k == key:
      return v
  return dflt

const defaultOptSeparators* = {'=', ':'}
const posixOptSeparators* = {'='}

iterator opts*(argv: openArray[string],
               # keyed arguments below
               shortDefault: OptValExpectation = optValNone,
               longDefault: OptValExpectation = optValOptional,
               shortVal: openArray[(char, OptValExpectation)] = [],
               longVal: openArray[(string, OptValExpectation)] = [],
               sep: set[char] = defaultOptSeparators): Opt =
  ## Parse command-line arguments like POSIX getopt(3)
  ##
  ## shortDefault:  default expectation of if a short option should receive value
  ## longDefault:   like `shortDefault`, but for long options
  ## shortVal:      expectation for short options
  ## longVal:       expectation for long options
  ## 
  ## See [`OptValExpectation`] for more info about what those parameters mean
  runnableExamples:
    for opt in opts(["-abc"], optValNone, shortVal={'b': optValRequired}):
      discard opt

  var all_positional = false
  var partial = Opt.none
  for arg in argv:
    if partial.isSome: # fill value of partial opt
      var opt = partial.get
      opt.value = arg
      yield opt
      partial = Opt.none
    elif all_positional:
      yield Opt(kind: positional, value: arg)
    elif arg == "-":
      yield Opt(kind: positional, value: arg)
    elif arg == "--":
      all_positional = true
    elif arg.startsWith "--": # process long
      # long
      var key = ""
      var i = 2 # skip starting --
      block process_arg:
        while i < arg.len:
          let c = arg[i]
          i.inc
          if c in sep:
            if longVal.get(key, longDefault) == optValNone:
              yield Opt(kind: long, keyLong: key, value: arg[i..^1], error: OptError.extraneous)
            else:
              yield Opt(kind: long, keyLong: key, value: arg[i..^1])
            break process_arg
          else:
            key &= c

        if longVal.get(key, longDefault) == optValRequired:
          # wait for .value to be filled
          partial = some Opt(kind: long, keyLong: key)
        else:
          yield Opt(kind: long, keyLong: key)
    elif arg.startsWith "-": # process short
      var i = 1 # skip starting -
      while i < arg.len:
        let c = arg[i]
        i.inc
        let expectation = shortVal.get(c, shortDefault)
        if expectation == optValNone:
          yield Opt(kind: short, keyShort: c)
        else:
          if i < arg.len: # if not the last char in this arg
            # implied: expectation == optValOptional or expectation == optValRequired
            if arg[i] in sep:
              # skip separator (e.g. '=') in "-a=c" if it exist
              i.inc
            yield Opt(kind: short, keyShort: c, value: arg[i..^1])
          else: # this arg exhausted (no char left)
            if expectation == optValRequired:
              partial = some Opt(kind: short, keyShort: c)
            else: # implied: expectation == optValOptional
              yield Opt(kind: short, keyShort: c)
          break
    else: # process positional
      yield Opt(kind: positional, value: arg)

  # throw on missing value
  if partial.isSome:
    var opt = partial.get
    opt.error = OptError.missing
    yield opt
