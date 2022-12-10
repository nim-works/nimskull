## Module implements wrapper for execution of the external command-line
## applications.

import
  std/[
    strutils,
    tables,
    osproc,
    streams,
    os,
    algorithm,
    options
  ]

when defined(windows):
  import std/winlean
else:
  import std/posix

when defined(linux) and defined(useClone):
  import std/linux

type
  ShellResult* = object
    ## Result of the shell command execution
    cmd*: ShellCmd
    cwd*: string ## Absolute path of initial command execution directory
    retcode*: int ## Exit code
    stderr*: string ## Stderr for command
    stdout*: string ## Stdout for command

  ShellArgKind* = enum
    ## Type of the shell command argument
    cpkArgument ## String argument to command
    cpkTemplated ## Interpolated parameter that can be replaced later.

  ShellArg* = object
    ## Single shell command argument
    cmd*: string ## Shell command argument string
    kind*: ShellArgKind ## Type of the shell command argument

  ShellCmd* = object
    ## Shell command with associated arguments
    bin*: string ## Binary name or absolute path
    opts*: seq[ShellArg] ## Arguments

func shArg*(arg: string): ShellArg =
  ## Create non-templated shell command argument
  ShellArg(kind: cpkArgument, cmd: arg)

func shSub*(arg: string): ShellArg =
  ## Create templated shell command argument
  ShellArg(kind: cpkTemplated, cmd: arg)

func shSub*(cmd: var ShellCmd, subs: openArray[string]) =
  ## Add templated shell command arguments to a command
  for sub in subs:
    cmd.opts.add shSub(sub)


func args*(cmd: var ShellCmd, args: openArray[ShellArg]) =
  ## Add argument for command
  cmd.opts.add args

func args*(cmd: var ShellCmd, args: openArray[string]) =
  ## Add argument for command
  for arg in args:
    cmd.opts.add shArg(arg)

func arg*(cmd: var ShellCmd, format: string, args: varargs[string]) =
  cmd.args([format(format, args)])

func empty*(cmd: ShellCmd): bool =
  cmd.bin.len() == 0 and cmd.opts.len() == 0

func shell*(bin: string, args: openArray[string] = @[]): ShellCmd =
  ## Create shell command with given bin and it's arguments
  result = ShellCmd(bin: bin)
  result.args args

func shell*(bin: string, args: varargs[ShellArg]): ShellCmd =
  ## Create shell command with given bin and it's arguments
  result = ShellCmd(bin: bin, opts: @args)

func add*(cmd: var ShellCmd, arg: ShellArg) =
  ## Add argument to shell command
  cmd.opts.add arg

func add*(cmd: var ShellCmd, args: openarray[ShellArg]) =
  ## Add arguments to shell command
  cmd.opts.add args

func toStr*(part: ShellArg): string =
  ## Convert non-templated shell argument to string
  if part.kind != cpkArgument:
    raise newException(
      ValueError,
      "Interpolation on the shell part '$" &
        part.cmd &
        "' wasn't finished. Use `interpolate()` on the command " &
        "in order to splice the arguments"
    )

  part.cmd

type
  ShInterpolate* = Table[string, seq[ShellArg]]

func interpolate*(
    part: ShellArg,
    map: ShInterpolate,
    default: Option[seq[string]] = some(newSeq[string]())
  ): seq[ShellArg] =
  ## Replace all templated arguments with appropriate substitutions.
  if part.kind == cpkTemplated:
    if part.cmd in map:
      result.add map[part.cmd]

    elif default.isSome():
      for it in default.get():
        result.add shArg(it)

    else:
      raise newException(
        KeyError, "Interpolated varible '$#' is missing from the map "
      )

  else:
    result.add part

func interpolate*(
    cmd: ShellCmd,
    map: ShInterpolate,
    default: Option[seq[string]] = some(newSeq[string]())
  ): ShellCmd =
  ## Replace all tar
  result.bin = cmd.bin
  for part in cmd.opts:
    for res in part.interpolate(map, default):
      result.add res

func interpolate*(
    cmd: ShellCmd,
    map: openArray[(string, seq[string])],
    default: Option[seq[string]] = some(newSeq[string]())
  ): ShellCmd =

  var tab: ShInterpolate
  for (key, vals) in map:
    for val in vals:
      tab.mgetOrPut(key, @[]).add shArg(val)

  return cmd.interpolate(tab, default)

func argsToStr*(cmd: ShellCmd): seq[string] =
  ## Get command arguments as list of strings
  for part in cmd.opts:
    result.add part.toStr()

func toStr*(cmd: ShellCmd): seq[string] =
  ## Get command as a linst of stirngs
  @[cmd.bin] & cmd.argsToStr()

proc parseShellArgs*(str: string): seq[ShellArg] =
  for it in str.split(" "):
    result.add shArg(it)

proc parseShellCmd*(str: string): ShellCmd =
  let split = str.split(" ")
  result = shell(split[0], split[1..^1])

proc exec*(
    cmd: ShellCmd, dir: string = "",
    stdin: string = "",
    options: set[ProcessOption] = {poUsePath},
    maxOutSize: int = 40 * 1024 * 1024
  ): ShellResult =
  ## Execute shell command.

  result.cwd = if len(dir) == 0: getCurrentDir() else: dir
  var p = startProcess(
    cmd.bin, workingDir = result.cwd,
    args = cmd.argsToStr(), options = options)

  result.cmd = cmd
  var outp = outputStream(p)
  var outerr = errorStream(p)

  if stdin.len > 0:
    inputStream(p).write(stdin)

  close inputStream(p)

  result.retcode = -1
  var line = newStringOfCap(120)
  while true:
    if outp.readLine(line):
      result.stdout.add(line)
      result.stdout.add("\n")

    elif outerr.readLine(line):
      result.stderr.add(line)
      result.stderr.add("\n")

    else:
      result.retcode = peekExitCode(p)
      if result.retcode != -1:
        break

    if maxOutSize < result.stderr.len():
      raise newException(
        OSError, "stderr size exceeded maximum allowed limit")

    if maxOutSize < result.stdout.len():
      raise newException(
        OSError, "stdout size exceeded maximum allowed limit")

  close(p)

proc start*(
    cmd: ShellCmd, dir: string = "",
    options: set[ProcessOption] = {poUsePath}
  ): Process =

  return startProcess(
    command = cmd.bin,
    workingDir = dir,
    args = cmd.argsToStr(),
    options = options
  )

proc exec*(
    cmds: openArray[ShellCmd],
    options: set[ProcessOption] = {poUsePath},
    maxParallel: int = countProcessors(),
    dir: string = "",
    beforeRunEvent: proc(idx: int) = nil,
    afterRunEvent: proc(idx: int, p: Process) = nil
  ): seq[ShellResult] =
  ## Execute multiple shell commands in paralell, return full list of
  ## results in the same order as the original commands.

  assert maxParallel > 0
  var i = 0
  var q = newSeq[Process](maxParallel)
  var idxs = newSeq[int](maxParallel) # map process index to cmds index

  var tmpResult: seq[(int, ShellResult)]
  when defined(windows):
    var w: WOHandleArray
    var m = min(min(maxParallel, MAXIMUM_WAIT_OBJECTS), cmds.len)
    var wcount = m
  else:
    var m = min(maxParallel, cmds.len)

  while i < m:
    if beforeRunEvent != nil:
      beforeRunEvent(i)
    q[i] = start(cmds[i], dir = dir, options = options)
    idxs[i] = i
    when defined(windows):
      w[i] = q[i].fProcessHandle
    inc(i)

  var ecount = len(cmds)
  while ecount > 0:
    var rexit = -1
    when defined(windows):
      # waiting for all children, get result if any child exits
      var ret = waitForMultipleObjects(int32(wcount), addr(w), 0'i32,
                                       INFINITE)
      if ret == WAIT_TIMEOUT:
        # must not be happen
        discard
      elif ret == WAIT_FAILED:
        raiseOSError(osLastError())
      else:
        var status: int32
        for r in 0..m-1:
          if not isNil(q[r]) and q[r].fProcessHandle == w[ret]:
            discard getExitCodeProcess(q[r].fProcessHandle, status)
            q[r].exitFlag = true
            q[r].exitStatus = status
            rexit = r
            break
    else:
      var status: cint = 1
      # waiting for all children, get result if any child exits
      let res = waitpid(-1, status, 0)
      if res > 0:
        for r in 0 .. m-1:
          if not isNil(q[r]) and q[r].processID() == res:
            if WIFEXITED(status) or WIFSIGNALED(status):
              q[r].exitFlag = true
              q[r].exitStatus = status
              rexit = r
              break
      else:
        let err = osLastError()
        if err == OSErrorCode(ECHILD):
          # some child exits, we need to check our childs exit codes
          for r in 0..m-1:
            if (not isNil(q[r])) and (not running(q[r])):
              q[r].exitFlag = true
              q[r].exitStatus = status
              rexit = r
              break
        elif err == OSErrorCode(EINTR):
          # signal interrupted our syscall, lets repeat it
          continue
        else:
          # all other errors are exceptions
          raiseOSError(err)

    if rexit >= 0:
      when defined(windows):
        let processHandle = q[rexit].fProcessHandle
      if afterRunEvent != nil:
        afterRunEvent(idxs[rexit], q[rexit])

      var res: ShellResult
      res.cmd = cmds[idxs[rexit]]
      res.cwd = dir
      res.retcode = q[rexit].peekExitCode()
      res.stdout = outputStream(q[rexit]).readAll()
      res.stderr = errorStream(q[rexit]).readAll()
      tmpResult.add((idxs[rexit], res))
      close(q[rexit])

      if i < len(cmds):
        if beforeRunEvent != nil:
          beforeRunEvent(i)
        q[rexit] = start(cmds[i], options = options)
        idxs[rexit] = i
        when defined(windows):
          w[rexit] = q[rexit].fProcessHandle
        inc(i)
      else:
        when defined(windows):
          for k in 0..wcount - 1:
            if w[k] == processHandle:
              w[k] = w[wcount - 1]
              w[wcount - 1] = 0
              dec(wcount)
              break
        q[rexit] = nil
      dec(ecount)

  for (idx, cmd) in sortedByIt(tmpResult, it[0]):
    result.add cmd

