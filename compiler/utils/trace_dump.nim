## This module implements a serializer for the trace data recorded with the
## ``Tracer`` facility into the "Chrome Trace Event" JSON format, which is a
## simple JSON-based format used for representing trace data. The format is
## understood by Google Chrome's built-in "Trace Viewer" as well as other
## trace/flamegraph visualizers, such as "Speedscope".
##
## For a specification, see `https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU`_
##
## "Complete" events (`X`) were used originally, as it meant less output to
## write, but because of the issues it caused with visualizers in some cases,
## "Begin" (`B`) and "End" (`E`) events are now used instead.
##
## While not strictly necessary, a manual JSON serializer is used instead of
## ``std/json`` for efficiency.

import
  system/[
    formatfloat
  ],
  std/[
    monotimes,
    times,
    strutils,
    streams
  ],
  compiler/utils/[
    tracer
  ]

const
  BeginPhase = "\"B\""
  EndPhase = "\"E\""

proc writeFloat(stream: Stream, val: float) =
  var str = ""
  str.addFloatRoundtrip(val)
  stream.write str

template fieldPart(name: untyped): untyped {.dirty.} =
  '"' & astToStr(name) & "\":"

template intField(name: untyped, val: SomeInteger) =
  mixin stream
  stream.write fieldPart(name)
  stream.write $val

template floatField(name: untyped, val: float) =
  mixin stream
  stream.write fieldPart(name)
  stream.writeFloat val

template stringField(name: untyped, val: string) =
  mixin stream
  stream.write fieldPart(name)
  stream.write escape(val)

template rawField(name: untyped, raw: string) =
  mixin name
  stream.write fieldPart(name)
  stream.write raw

template next() =
  mixin name
  stream.write ','

proc writePayload(stream: Stream, pl: EventPayload) =
  ## Serializes the payload's content to JSON and writes it to `stream`
  case pl.kind
  of tikModule:
    stringField(name, pl.sym.name.s)
  of tikInclude:
    stringField(name, pl.str)
  of tikSem:
    stringField(kind, $pl.nk)
    next()
    intField(line, pl.loc.line)
  of tikNimScript:
    stringField(file, pl.str)
  of tikCodegen, tikVmCodegen:
    if pl.sym != nil:
      stringField(name, pl.sym.name.s)
    else:
      intField(line, pl.loc.line)
  of tikOther:
    stringField(details, pl.str)
  else:
    if pl.sym != nil:
      stringField(details, pl.sym.name.s)

proc writeToStream*(t: Tracer, stream: Stream) =
  ## Serializes all events and associated data recorded with `t` into the
  ## "Chrome Trace Event" JSON Object format and outputs the result to `stream`
  let start = t.start

  # the JSON object format is used. All events are stored as an array in the
  # 'traceEvents' field
  stream.write """{"traceEvents": ["""

  var first = true
  for e in t.events.items:
    let pl {.cursor.} = t.payloads[e.payload.int]

    if not first:
      stream.write ","
    else:
      first = false

    let ph =
      if e.begin: BeginPhase
      else:       EndPhase

    # make the timestamp a relative one and change the unit it's measured in
    # to microseconds:
    let ts = inNanoseconds(e.timeStamp - start).int / 1000

    stream.write "{"
    intField(pid, 0); next()   # process ID
    intField(tid, 0); next()   # thread ID
    rawField(ph, ph); next()   # the event phase
    floatField(ts, ts); next() # the timestamp

    # while it's not required by the specification for "End" events to store
    # the "name" field, some visualizers won't work without
    stringField(name, $pl.kind)

    if e.begin:
      stream.write ""","args":{"""
      writePayload(stream, pl)
      stream.write "}"

    stream.write "}"

  stream.write "]}"
  stream.close()