#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Channel support for threads.
##
## **Note**: This is part of the system module. Do not import it directly.
## To activate thread support compile with the `--threads:on` command line switch.
##
## **Note:** Channels are designed for the `Thread` type.
##
## **Note:** The current implementation of message passing does
## not work with cyclic data structures.
##
## **Note:** Channels cannot be passed between threads. Use globals or pass
## them by `ptr`.
##
## Example
## =======
## The following is a simple example of two different ways to use channels:
## blocking and non-blocking.
##
## .. code-block:: Nim
##   # Be sure to compile with --threads:on.
##   # The channels and threads modules are part of system and should not be
##   # imported.
##   import std/os
##
##   # Channels can either be:
##   #  - declared at the module level, or
##   #  - passed to procedures by ptr (raw pointer) -- see note on safety.
##   #
##   # For simplicity, in this example a channel is declared at module scope.
##   # Channels are generic, and they include support for passing objects between
##   # threads.
##   # Note that objects passed through channels will be deeply copied.
##   var chan: Channel[string]
##
##   # This proc will be run in another thread using the threads module.
##   proc firstWorker() =
##     chan.send("Hello World!")
##
##   # This is another proc to run in a background thread. This proc takes a while
##   # to send the message since it sleeps for 2 seconds (or 2000 milliseconds).
##   proc secondWorker() =
##     sleep(2000)
##     chan.send("Another message")
##
##   # Initialize the channel.
##   chan.open()
##
##   # Launch the worker.
##   var worker1: Thread[void]
##   createThread(worker1, firstWorker)
##
##   # Block until the message arrives, then print it out.
##   echo chan.recv() # "Hello World!"
##
##   # Wait for the thread to exit before moving on to the next example.
##   worker1.joinThread()
##
##   # Launch the other worker.
##   var worker2: Thread[void]
##   createThread(worker2, secondWorker)
##   # This time, use a non-blocking approach with tryRecv.
##   # Since the main thread is not blocked, it could be used to perform other
##   # useful work while it waits for data to arrive on the channel.
##   while true:
##     let tried = chan.tryRecv()
##     if tried.dataAvailable:
##       echo tried.msg # "Another message"
##       break
##
##     echo "Pretend I'm doing useful work..."
##     # For this example, sleep in order not to flood stdout with the above
##     # message.
##     sleep(400)
##
##   # Wait for the second thread to exit before cleaning up the channel.
##   worker2.joinThread()
##
##   # Clean up the channel.
##   chan.close()
##
## Sample output
## -------------
## The program should output something similar to this, but keep in mind that
## exact results may vary in the real world::
##   Hello World!
##   Pretend I'm doing useful work...
##   Pretend I'm doing useful work...
##   Pretend I'm doing useful work...
##   Pretend I'm doing useful work...
##   Pretend I'm doing useful work...
##   Another message
##
## Passing Channels Safely
## -----------------------
## Note that when passing objects to procedures on another thread by pointer
## (for example through a thread's argument), objects created using the default
## allocator will use thread-local, GC-managed memory. Thus it is generally
## safer to store channel objects in global variables (as in the above example),
## in which case they will use a process-wide (thread-safe) shared heap.
##
## However, it is possible to manually allocate shared memory for channels
## using e.g. `system.allocShared0` and pass these pointers through thread
## arguments:
##
## .. code-block:: Nim
##   proc worker(channel: ptr Channel[string]) =
##     let greeting = channel[].recv()
##     echo greeting
##
##   proc localChannelExample() =
##     # Use allocShared0 to allocate some shared-heap memory and zero it.
##     # The usual warnings about dealing with raw pointers apply. Exercise caution.
##     var channel = cast[ptr Channel[string]](
##       allocShared0(sizeof(Channel[string]))
##     )
##     channel[].open()
##     # Create a thread which will receive the channel as an argument.
##     var thread: Thread[ptr Channel[string]]
##     createThread(thread, worker, channel)
##     channel[].send("Hello from the main thread!")
##     # Clean up resources.
##     thread.joinThread()
##     channel[].close()
##     deallocShared(channel)
##
##   localChannelExample() # "Hello from the main thread!"

when not declared(ThisIsSystem):
  {.error: "You must not import this module explicitly".}

type
  pbytes = ptr UncheckedArray[byte]
  RawChannel {.pure, final.} = object ## msg queue for a thread
    rd, wr, count, mask, maxItems: int
    data: pbytes
    lock: SysLock
    cond: SysCond
    elemType: PNimType
    ready: bool
  PRawChannel = ptr RawChannel
  LoadStoreMode = enum mStore, mLoad
  Channel*[TMsg] {.gcsafe.} = RawChannel ## a channel for thread communication

const ChannelDeadMask = -2

proc initRawChannel(p: pointer, maxItems: int) =
  var c = cast[PRawChannel](p)
  initSysLock(c.lock)
  initSysCond(c.cond)
  c.mask = -1
  c.maxItems = maxItems

proc deinitRawChannel(p: pointer) =
  var c = cast[PRawChannel](p)
  # we need to grab the lock to be safe against sending threads!
  acquireSys(c.lock)
  c.mask = ChannelDeadMask
  if c.data != nil:
    deallocShared(c.data)
  deinitSys(c.lock)
  deinitSysCond(c.cond)

proc rawSend(q: PRawChannel, data: pointer, typ: PNimType) =
  ## Adds an `item` to the end of the queue `q`.
  var cap = q.mask+1
  if q.count >= cap:
    # start with capacity for 2 entries in the queue:
    if cap == 0: cap = 1
    var n = cast[pbytes](allocShared0(cap*2*typ.size))
    var z = 0
    var i = q.rd
    var c = q.count
    while c > 0:
      dec c
      copyMem(addr(n[z*typ.size]), addr(q.data[i*typ.size]), typ.size)
      i = (i + 1) and q.mask
      inc z
    if q.data != nil:
      deallocShared(q.data)
    q.data = n
    q.mask = cap*2 - 1
    q.wr = q.count
    q.rd = 0

  copyMem(addr(q.data[q.wr * typ.size]), data, typ.size)
  inc q.count
  q.wr = (q.wr + 1) and q.mask

proc rawRecv(q: PRawChannel, data: pointer, typ: PNimType) =
  sysAssert q.count > 0, "rawRecv"
  dec q.count
  copyMem(data, addr(q.data[q.rd * typ.size]), typ.size)
  q.rd = (q.rd + 1) and q.mask

template lockChannel(q, action): untyped =
  acquireSys(q.lock)
  action
  releaseSys(q.lock)

proc sendImpl(q: PRawChannel, typ: PNimType, msg: pointer, noBlock: bool): bool =
  if q.mask == ChannelDeadMask:
    sysFatal(DeadThreadDefect, "cannot send message; thread died")
  acquireSys(q.lock)
  if q.maxItems > 0:
    # Wait until count is less than maxItems
    if noBlock and q.count >= q.maxItems:
      releaseSys(q.lock)
      return

    while q.count >= q.maxItems:
      waitSysCond(q.cond, q.lock)

  rawSend(q, msg, typ)
  q.elemType = typ
  releaseSys(q.lock)
  signalSysCond(q.cond)
  result = true

proc send*[TMsg](c: var Channel[TMsg], msg: sink TMsg) {.inline.} =
  ## Sends a message to a thread. `msg` is deeply copied.
  discard sendImpl(cast[PRawChannel](addr c), cast[PNimType](getTypeInfo(msg)), unsafeAddr(msg), false)
  wasMoved(msg)

proc trySend*[TMsg](c: var Channel[TMsg], msg: sink TMsg): bool {.inline.} =
  ## Tries to send a message to a thread.
  ##
  ## `msg` is deeply copied. Doesn't block.
  ##
  ## Returns `false` if the message was not sent because number of pending items
  ## in the channel exceeded `maxItems`.
  result = sendImpl(cast[PRawChannel](addr c), cast[PNimType](getTypeInfo(msg)), unsafeAddr(msg), true)
  if result:
    wasMoved(msg)

proc llRecv(q: PRawChannel, res: pointer, typ: PNimType) =
  q.ready = true
  while q.count <= 0:
    waitSysCond(q.cond, q.lock)
  q.ready = false
  if typ != q.elemType:
    releaseSys(q.lock)
    sysFatal(ValueError, "cannot receive message of wrong type")
  rawRecv(q, res, typ)
  if q.maxItems > 0 and q.count == q.maxItems - 1:
    # Parent thread is awaiting in send. Wake it up.
    signalSysCond(q.cond)

proc recv*[TMsg](c: var Channel[TMsg]): TMsg =
  ## Receives a message from the channel `c`.
  ##
  ## This blocks until a message has arrived!
  ## You may use `peek proc <#peek,Channel[TMsg]>`_ to avoid the blocking.
  var q = cast[PRawChannel](addr(c))
  acquireSys(q.lock)
  llRecv(q, addr(result), cast[PNimType](getTypeInfo(result)))
  releaseSys(q.lock)

proc tryRecv*[TMsg](c: var Channel[TMsg]): tuple[dataAvailable: bool,
                                                  msg: TMsg] =
  ## Tries to receive a message from the channel `c`, but this can fail
  ## for all sort of reasons, including contention.
  ##
  ## If it fails, it returns `(false, default(msg))` otherwise it
  ## returns `(true, msg)`.
  var q = cast[PRawChannel](addr(c))
  if q.mask != ChannelDeadMask:
    if tryAcquireSys(q.lock):
      if q.count > 0:
        llRecv(q, addr(result.msg), cast[PNimType](getTypeInfo(result.msg)))
        result.dataAvailable = true
      releaseSys(q.lock)

proc peek*[TMsg](c: var Channel[TMsg]): int =
  ## Returns the current number of messages in the channel `c`.
  ##
  ## Returns -1 if the channel has been closed.
  ##
  ## **Note**: This is dangerous to use as it encourages races.
  ## It's much better to use `tryRecv proc <#tryRecv,Channel[TMsg]>`_ instead.
  var q = cast[PRawChannel](addr(c))
  if q.mask != ChannelDeadMask:
    lockChannel(q):
      result = q.count
  else:
    result = -1

proc open*[TMsg](c: var Channel[TMsg], maxItems: int = 0) =
  ## Opens a channel `c` for inter thread communication.
  ##
  ## The `send` operation will block until number of unprocessed items is
  ## less than `maxItems`.
  ##
  ## For unlimited queue set `maxItems` to 0.
  initRawChannel(addr(c), maxItems)

proc close*[TMsg](c: var Channel[TMsg]) =
  ## Closes a channel `c` and frees its associated resources.
  deinitRawChannel(addr(c))

proc ready*[TMsg](c: var Channel[TMsg]): bool =
  ## Returns true if some thread is waiting on the channel `c` for
  ## new messages.
  var q = cast[PRawChannel](addr(c))
  result = q.ready
