## This module implements a simple framework for event-based tracing.
##
## A ``Tracer`` instance is used to record *events*. Each event is associated
## with a payload, which stores user-provided information about the event. As
## of now, there are only two types of events: begin and end events; but this
## is meant to be expanded as needed.

import
  std/[
    monotimes
  ],
  compiler/ast/[
    ast_types,
    lineinfos
  ]

type
  TracedItemKind* = enum
    ## Used to loosely group events together (the kind is used as the event name
    ## in the output). This likely needs a complete redesign - the only goal so
    ## far was to make the output readable
    tikNimScript   = "NimScript" # an event related to NimScript processing
    tikParser      = "Parser"
    tikModule      = "Module"  # a module is imported
    tikInclude     = "Include" # a module is included
    tikSem         = "Sem"     # an event related to the semantic analysis
                               # subsystem
    tikVmCodegen   = "VmCodegen" # ``vmgen`` is invoked to generate bytecode in
                                 # the context of compile-time execution
    tikVm          = "Vm"      # the VM is invoked
    tikTransform   = "Transform" # ``transf`` is invoked
    tikCodegen     = "Codegen" # an event related to code-generation for the
                               # target language
    tikInjectDestr = "InjectDestructors"
    tikBackend     = "Backend" # the backend is invoked (e.g. the C compiler or
                               # linker)
    tikOther       = "Other"   # an unspecified other event

  PayloadId* = distinct uint32
    ## The ID of a payload. Acts as the unique identifier of an event payload
    ## in the context of a ``Tracer`` instance - they're not unique across
    ## multiple instances

  EventPayload* = object
    ## Stores the data associated with a trace event. For simplicity and in
    ## order to allow for figuring out the requirements, no object variant is
    ## used and all field are available for all trace item kinds
    kind*: TracedItemKind

    nk*: TNodeKind

    loc*: TLineInfo
    sym*: PSym
    str*: string

  TraceEvent* = object
    begin*: bool         ## whether this is a 'begin' or 'end' event
    payload*: PayloadId  ## the ID of the associated payload
    timeStamp*: MonoTime ## the timestamp of the event

  Tracer* = object
    start*: MonoTime         ## the point in time where the tracer became
                             ## active. Used to turn the absolute timestamps of
                             ## events into relative ones
    events*: seq[TraceEvent] ## all recorded events
    payloads*: seq[EventPayload] ## the payload data for events

proc startTracer*(): Tracer =
  ## Intializes a new ``Tracer`` instance and returns it. The start time is
  ## initialized with the current time and the start event is recorded
  let time = getMonoTime()
  result = Tracer(start: time)
  # add the payload for the top-level trace item and record a 'begin' event:
  result.payloads.add EventPayload(kind: tikOther)
  result.events.add TraceEvent(begin: true, payload: PayloadId 0,
                               timeStamp: time)

proc finish*(t: var Tracer) =
  ## Records the stop event. No further events should be recorded with the
  ## `t` after a call to ``finish``
  let time = getMonoTime()
  t.events.add TraceEvent(begin: false, payload: PayloadId 0, timeStamp: time)

# disable stack-traces and runtime checks, so that the trace calls have less
# overhead
{.push stackTrace: off, checks: off.}

proc addPayload*(t: var Tracer, payload: sink EventPayload): PayloadId =
  ## Add `payload` to the tracer and returns the ID assigned to it, which can
  ## then be used to
  result = t.payloads.len.PayloadId
  t.payloads.add payload

proc record*(t: var Tracer, begin: bool, payload: PayloadId) {.inline.} =
  ## Records an begin/end event with the given `payload`. A payload can
  ## be associated with an unlimited amount of events
  t.events.add TraceEvent(begin: begin, payload: payload,
                          timeStamp: getMonoTime())

{.pop.}

# below are helper templates for instrumenting procedures or pieces of code:

template traceStr*(t: var Tracer, k: TracedItemKind, s: string) =
  bind addPayload, record
  let p = t.addPayload EventPayload(kind: k, str: s)

  t.record(true, p)
  defer: t.record(false, p)

template traceStr*(t: var Tracer, s: string) =
  bind addPayload, record
  let p = t.addPayload EventPayload(kind: tikOther, str: s)

  t.record(true, p)
  defer: t.record(false, p)

template traceStr*(t: var Tracer, s: string, bloc) =
  bind addPayload, record
  let p = t.addPayload EventPayload(kind: tikOther, str: s)
  t.record(true, p)
  bloc
  t.record(false, p)

template traceLoc*(t: var Tracer, k: TracedItemKind, l: TLineInfo) =
  bind addPayload, record
  let p = t.addPayload EventPayload(kind: k, loc: l)

  t.record(true, p)
  defer: t.record(false, p)

template traceSym*(t: var Tracer, k: TracedItemKind, s: PSym) =
  bind addPayload, record
  let p = t.addPayload EventPayload(kind: k, sym: s)

  t.record(true, p)
  defer: t.record(false, p)

template traceSym*(t: var Tracer, k: TracedItemKind, s: PSym, bloc) =
  bind addPayload, record
  let p = t.addPayload EventPayload(kind: k, sym: s)
  t.record(true, p)
  bloc
  t.record(false, p)

template traceSem*(t: var Tracer, k: TNodeKind, l: TLineInfo) =
  bind addPayload, record
  let p = t.addPayload EventPayload(kind: tikSem, nk: k, loc: l)

  t.record(true, p)
  defer: t.record(false, p)