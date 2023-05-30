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

import options, strutils

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
  OptExtraneousVal* = ref object of OptError
    case kind*: OptKind
    of optShort:
      keyShort*: char
    of optLong:
      keyLong*: string
    of optPos:
      discard
  OptMissingVal* = ref object of OptError
    case kind*: OptKind
    of optShort:
      keyShort*: char
    of optLong:
      keyLong*: string
    of optPos:
      discard

# xxx: trivial impl. remove this once == for case objects is implemented
proc`==`*(a, b: Opt): bool =
  a.kind == b.kind and a.val == b.val and (
    case a.kind:
    of optShort:
      a.keyShort == b.keyShort
    of optLong:
      a.keyLong == b.keyLong
    of optPos:
      true
  )

iterator opts*(argv: openArray[string], shortHasVal: openArray[char] = [], longHasVal: openArray[string] = [], sep: set[char] = {'=', ':'}): Opt =
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
            if key notin longHasVal:
              raise OptExtraneousVal(kind: optLong, keyLong: key)
            yield Opt(kind: optLong, keyLong: key, val: arg[i..^1])
            break process_arg
          else:
            key &= c
        
        let opt = Opt(kind: optLong, keyLong: key)
        if key in longHasVal:
          partial = some opt # wait for .val to be filled
        else:
          yield opt
    elif arg.startsWith "-": # process short
      var i = 1
      while i < arg.len:
        let c = arg[i]
        i.inc
        if c in shortHasVal:
          if i < arg.len:
            if arg[i] in sep:
              # skip = in -a=c if exist
              i.inc
            yield Opt(kind: optShort, keyShort: c, val: arg[i..^1])
            break
          else:
            partial = some Opt(kind: optShort, keyShort: c)
            break
        else:
          yield Opt(kind: optShort, keyShort: c)
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
