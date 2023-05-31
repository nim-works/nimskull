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
## --long=    (empty val)
## -- -positional1 --positional2
## 
## -    is treated as positional
## --   skipped once; everything after this is treated as positional
## ---  is treated as long arg "-"
## 
## -:   undefined behavior (separator after dash)

import std/[options, strutils]

type
  OptKind* = enum
    optShort ## -s
    optLong  ## --long
    optPos   ## positional
  Opt* = object
    case kind*: OptKind
    of optShort:
      keyShort*: char
    of optLong:
      keyLong*: string
    of optPos:
      discard
    val*: string

  OptError* = ref object of CatchableError
    case kind*: OptKind
    of optShort:
      keyShort*: char
    of optLong:
      keyLong*: string
    of optPos:
      discard
  OptExtraneousVal* = ref object of OptError
  OptMissingVal* = ref object of OptError

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
  a.kind == b.kind and a.val == b.val and (
    case a.kind:
    of optShort:
      a.keyShort == b.keyShort
    of optLong:
      a.keyLong == b.keyLong
    of optPos:
      true
  )

proc get[T](explicitOnes: openArray[(T, OptValExpectation)], key: T,
    dflt: OptValExpectation): OptValExpectation =
  for (k, v) in explicitOnes:
    if k == key:
      return v
  return dflt

const defaultSeparators* = {'=', ':'}

iterator opts*(argv: openArray[string], defaultVal: OptValExpectation,
               shortVal: openArray[(char, OptValExpectation)] = [],
               longVal: openArray[(string, OptValExpectation)] = [],
               sep: set[char] = defaultSeparators): Opt =
  ## Parse command-line arguments like POSIX getopt(3)
  ##
  ## defaultVal:  default expectation of if a option should receive value
  ## shortVal:    expectation for short options
  ## longVal:     expectation for long options
  ## 
  ## See [`OptValExpectation`] for more info about what those parameters mean
  runnableExamples:
    for opt in opts(["-abc"], optValNone, shortVal={'b': optValRequired}):
      discard opt

  var all_positional = false
  var partial = Opt.none
  for arg in argv:
    if partial.isSome: # fill val of partial opt
      var opt = partial.get
      opt.val = arg
      yield opt
      partial = Opt.none
    elif all_positional:
      yield Opt(kind: optPos, val: arg)
    elif arg == "-":
      yield Opt(kind: optPos, val: arg)
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
            if longVal.get(key, defaultVal) == optValNone:
              raise OptExtraneousVal(kind: optLong, keyLong: key)
            yield Opt(kind: optLong, keyLong: key, val: arg[i..^1])
            break process_arg
          else:
            key &= c

        if longVal.get(key, defaultVal) == optValRequired:
          # wait for .val to be filled
          partial = some Opt(kind: optLong, keyLong: key)
        else:
          yield Opt(kind: optLong, keyLong: key)
    elif arg.startsWith "-": # process short
      var i = 1 # skip starting -
      while i < arg.len:
        let c = arg[i]
        i.inc
        let expectation = shortVal.get(c, defaultVal)
        if expectation == optValNone:
          yield Opt(kind: optShort, keyShort: c)
        else:
          if i < arg.len: # if not the last char in this arg
            if arg[i] in sep:
              # skip separator (e.g. '=') in "-a=c" if it exist
              i.inc
            yield Opt(kind: optShort, keyShort: c, val: arg[i..^1])
          else: # this arg exhausted (no char left)
            if expectation == optValRequired:
              partial = some Opt(kind: optShort, keyShort: c)
            else: # implied: expectation == optValOptional
              yield Opt(kind: optShort, keyShort: c)
          break
    else: # process positional
      yield Opt(kind: optPos, val: arg)

  # throw on missing val
  if partial.isSome:
    let opt = partial.get
    case opt.kind
    of optShort:
      raise OptMissingVal(kind: optShort, keyShort: opt.keyShort)
    of optLong:
      raise OptMissingVal(kind: optLong, keyLong: opt.keyLong)
    else:
      doAssert false, "unreachable"
