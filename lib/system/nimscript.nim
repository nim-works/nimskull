#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## To learn about scripting in Nimskull see `NimScript<nims.html>`_

# Nimskull script. This module provides a few extras available when in a
# scripting environment.

template builtin = discard

# We know the effects better than the compiler:
{.push hint[XDeclaredButNotUsed]: off.}

proc listDirsImpl(dir: string): seq[string] {.
  tags: [ReadIOEffect], raises: [OSError].} = builtin
proc listFilesImpl(dir: string): seq[string] {.
  tags: [ReadIOEffect], raises: [OSError].} = builtin
proc removeDir(dir: string, checkDir = true) {.
  tags: [ReadIOEffect, WriteIOEffect], raises: [OSError].} = builtin
proc removeFile(dir: string) {.
  tags: [ReadIOEffect, WriteIOEffect], raises: [OSError].} = builtin
proc moveFile(src, dest: string) {.
  tags: [ReadIOEffect, WriteIOEffect], raises: [OSError].} = builtin
proc moveDir(src, dest: string) {.
  tags: [ReadIOEffect, WriteIOEffect], raises: [OSError].} = builtin
proc copyFile(src, dest: string) {.
  tags: [ReadIOEffect, WriteIOEffect], raises: [OSError].} = builtin
proc copyDir(src, dest: string) {.
  tags: [ReadIOEffect, WriteIOEffect], raises: [OSError].} = builtin
proc createDir(dir: string) {.tags: [WriteIOEffect], raises: [OSError].} =
  builtin

proc getError: string = builtin
proc setCurrentDir(dir: string) = builtin
proc getCurrentDir*(): string =
  ## Retrieves the current working directory.
  builtin
proc rawExec(cmd: string): int {.tags: [ExecIOEffect], raises: [OSError].} =
  builtin

proc paramStr*(i: int): string =
  ## Retrieves the `i`'th command line parameter.
  builtin

proc paramCount*(): int =
  ## Retrieves the number of command line parameters.
  builtin

proc cmpIgnoreStyle(a, b: string): int = builtin
proc cmpIgnoreCase(a, b: string): int = builtin

proc cmpic*(a, b: string): int =
  ## Compares `a` and `b` ignoring case.
  cmpIgnoreCase(a, b)

proc getEnv*(key: string; default = ""): string {.tags: [ReadIOEffect].} =
  ## Retrieves the environment variable of name `key`.
  builtin

proc existsEnv*(key: string): bool {.tags: [ReadIOEffect].} =
  ## Checks for the existence of an environment variable named `key`.
  builtin

proc putEnv*(key, val: string) {.tags: [WriteIOEffect].} =
  ## Sets the value of the environment variable named `key` to `val`.
  builtin

proc delEnv*(key: string) {.tags: [WriteIOEffect].} =
  ## Deletes the environment variable named `key`.
  builtin

proc fileExists*(filename: string): bool {.tags: [ReadIOEffect].} =
  ## Checks if the file exists.
  builtin

proc dirExists*(dir: string): bool {.
  tags: [ReadIOEffect].} =
  ## Checks if the directory `dir` exists.
  builtin

proc selfExe*(): string =
  ## Returns the name of the compiler executable running this script file.
  builtin

proc toExe*(filename: string): string =
  ## On Windows adds ".exe" to `filename`, else returns `filename` unmodified.
  (when defined(windows): filename & ".exe" else: filename)

proc toDll*(filename: string): string =
  ## On Windows adds ".dll" to `filename`, on Posix produces "lib$filename.so".
  (when defined(windows): filename & ".dll" else: "lib" & filename & ".so")

proc strip(s: string): string =
  var i = 0
  while s[i] in {' ', '\c', '\n'}: inc i
  result = s.substr(i)
  if result[0] == '"' and result[^1] == '"':
    result = result[1..^2]

type
  ScriptMode* {.pure.} = enum ## Controls the behaviour of the script.
    Silent,                   ## Be silent.
    Verbose,                  ## Be verbose.
    Whatif                    ## Do not run commands, instead just echo what
                              ## would have been done.

var mode*: ScriptMode ## set to influence mkDir, rmDir, rmFile, etc behaviour

const scriptMode {.strdefine.} = ""
  ## allow overriding scriptmode for safety via the CLI

when defined(scriptMode) or scriptMode != "":
  # include cases where the user defined it, but failed to set a value
  let actualMode =
    case scriptMode.normalize:
    of "silent":  Silent
    of "verbose": Verbose
    of "whatif" : WhatIf
    else:         WhatIf # safe default; doesn't matter what they wrote
    # TODO: add a nice message for fallthrough cases

  template log(msg: string, body: untyped) =
    if actualMode in {ScriptMode.Verbose, ScriptMode.Whatif}:
      echo "[NimScript] ", msg
    if actualMode != ScriptMode.Whatif:
      body
else:
  template log(msg: string, body: untyped) =
    if mode in {ScriptMode.Verbose, ScriptMode.Whatif}:
      echo "[NimScript] ", msg
    if mode != ScriptMode.Whatif:
      body
  

template checkError(exc: untyped): untyped =
  let err = getError()
  if err.len > 0: raise newException(exc, err)

template checkOsError =
  checkError(OSError)

proc listDirs*(dir: string): seq[string] =
  ## Lists all the subdirectories (non-recursively) in the directory `dir`.
  result = listDirsImpl(dir)
  checkOsError()

proc listFiles*(dir: string): seq[string] =
  ## Lists all the files (non-recursively) in the directory `dir`.
  result = listFilesImpl(dir)
  checkOsError()

proc rmDir*(dir: string, checkDir = false) {.raises: [OSError].} =
  ## Removes the directory `dir`.
  log "rmDir: " & dir:
    removeDir(dir, checkDir = checkDir)
    checkOsError()

proc rmFile*(file: string) {.raises: [OSError].} =
  ## Removes the `file`.
  log "rmFile: " & file:
    removeFile file
    checkOsError()

proc mkDir*(dir: string) {.raises: [OSError].} =
  ## Creates the directory `dir` including all necessary subdirectories. If
  ## the directory already exists, no error is raised.
  log "mkDir: " & dir:
    createDir dir
    checkOsError()

proc mvFile*(`from`, to: string) {.raises: [OSError].} =
  ## Moves the file `from` to `to`.
  log "mvFile: " & `from` & ", " & to:
    moveFile `from`, to
    checkOsError()

proc mvDir*(`from`, to: string) {.raises: [OSError].} =
  ## Moves the dir `from` to `to`.
  log "mvDir: " & `from` & ", " & to:
    moveDir `from`, to
    checkOsError()

proc cpFile*(`from`, to: string) {.raises: [OSError].} =
  ## Copies the file `from` to `to`.
  log "cpFile: " & `from` & ", " & to:
    copyFile `from`, to
    checkOsError()

proc cpDir*(`from`, to: string) {.raises: [OSError].} =
  ## Copies the dir `from` to `to`.
  log "cpDir: " & `from` & ", " & to:
    copyDir `from`, to
    checkOsError()

proc exec*(command: string) {.
  raises: [OSError], tags: [ExecIOEffect, WriteIOEffect].} =
  ## Executes an external process. If the external process terminates with
  ## a non-zero exit code, an OSError exception is raised.
  log "exec: " & command:
    if rawExec(command) != 0:
      raise newException(OSError, "FAILED: " & command)
    checkOsError()

proc exec*(command: string, input: string, cache = ""): string {.
  raises: [OSError], tags: [ExecIOEffect, WriteIOEffect].} =
  ## Executes an external process. If the external process terminates with
  ## a non-zero exit code, an OSError exception is raised.
  log "exec: " & command:
    let (output, exitCode) = gorgeEx(command, input, cache)
    if exitCode != 0:
      raise newException(OSError, "FAILED: " & command)
    result = output

proc selfExec*(command: string) {.
  raises: [OSError], tags: [ExecIOEffect, WriteIOEffect].} =
  ## Executes an external command with the current script executable.
  ## `Command` must not contain the "nim " part.
  let c = selfExe() & " " & command
  log "exec: " & c:
    if rawExec(c) != 0:
      raise newException(OSError, "FAILED: " & c)
    checkOsError()

proc get*(key: string): string =
  ## Retrieves a configuration 'key' like 'gcc.options.always'.
  builtin

proc exists*(key: string): bool =
  ## Checks for the existence of a configuration 'key'
  ## like 'gcc.options.always'.
  builtin

proc nimcacheDir*(): string =
  ## Retrieves the location of 'nimcache'.
  builtin

proc thisDir*(): string =
  ## Retrieves the directory of the current `nims` script file. Its path is
  ## obtained via `currentSourcePath` (although, currently,
  ## `currentSourcePath` resolves symlinks, unlike `thisDir`).
  builtin

proc cd*(dir: string) {.raises: [OSError].} =
  ## Changes the current directory.
  ##
  ## The change is permanent for the rest of the execution, since this is just
  ## a shortcut for `os.setCurrentDir() <os.html#setCurrentDir,string>`_ . Use
  ## the `withDir() <#withDir.t,string,untyped>`_ template if you want to
  ## perform a temporary change only.
  setCurrentDir(dir)
  checkOsError()

proc findExe*(bin: string): string =
  ## Searches for bin in the current working directory and then in directories
  ## listed in the PATH environment variable. Returns "" if the exe cannot be
  ## found.
  builtin

template withDir*(dir: string; body: untyped): untyped =
  ## Changes the current directory temporarily.
  ##
  ## If you need a permanent change, use the `cd() <#cd,string>`_ proc.
  ## Usage example:
  ##
  ## .. code-block:: nim
  ##   withDir "foo":
  ##     # inside foo
  ##   #back to last dir
  var curDir = getCurrentDir()
  try:
    cd(dir)
    body
  finally:
    cd(curDir)

proc stdinReadLine(): string {.
  tags: [ReadIOEffect], raises: [IOError].} =
  builtin

proc stdinReadAll(): string {.
  tags: [ReadIOEffect], raises: [IOError].} =
  builtin

proc readLineFromStdin*(): string {.raises: [IOError].} =
  ## Reads a line of data from stdin - blocks until \n or EOF which happens when stdin is closed
  log "readLineFromStdin":
    result = stdinReadLine()
    checkError(EOFError)

proc readAllFromStdin*(): string {.raises: [IOError].} =
  ## Reads all data from stdin - blocks until EOF which happens when stdin is closed
  log "readAllFromStdin":
    result = stdinReadAll()
    checkError(EOFError)

template `==?`(a, b: string): bool = cmpIgnoreStyle(a, b) == 0

{.pop.}
