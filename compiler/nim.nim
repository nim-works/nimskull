#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import std/[os]
when defined(windows) and not defined(nimKochBootstrap):
  # remove workaround pending bootstrap >= 1.5.1
  # refs https://github.com/nim-lang/Nim/issues/18334#issuecomment-867114536
  # alternative would be to prepend `currentSourcePath.parentDir.quoteShell`
  when defined(gcc):
    when defined(x86):
      {.link: "../icons/nim.res".}
    else:
      {.link: "../icons/nim_icon.o".}

  when defined(amd64) and defined(vcc):
    {.link: "../icons/nim-amd64-windows-vcc.res".}
  when defined(i386) and defined(vcc):
    {.link: "../icons/nim-i386-windows-vcc.res".}

import
  compiler/backend/[
    extccomp
  ],
  compiler/front/[
    msgs, main, cmdlinehelper, options, commands, cli_reporter
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/utils/[
    pathutils
  ],
  compiler/ast/[
    reports, idents
  ]


from std/browsers import openDefaultBrowser
from compiler/utils/nodejs import findNodeJs

when hasTinyCBackend:
  import compiler/backend/tccgen

when defined(profiler) or defined(memProfiler):
  {.hint: "Profiling support is turned on!".}
  import sdt/nimprof


proc getNimRunExe(conf: ConfigRef): string =
  # xxx consider defining `conf.getConfigVar("nimrun.exe")` to allow users to
  # customize the binary to run the command with, e.g. for custom `nodejs` or `wine`.
  if conf.isDefined("mingw"):
    if conf.isDefined("i386"): result = "wine"
    elif conf.isDefined("amd64"): result = "wine64"

proc handleCmdLine(cache: IdentCache; conf: ConfigRef) =
  ## Main entry point to the compiler - dispatches command-line commands
  ## into different subsystems, sets up configuration options for the
  ## `conf`:arg: and so on.
  let self = NimProg(
    supportsStdinFile: true,
    processCmdLine: processCmdLine
  )
  self.initDefinesProg(conf, "nim_compiler")
  if paramCount() == 0:
    writeCommandLineUsage(conf)
    return

  self.processCmdLineAndProjectPath(conf)
  var graph = newModuleGraph(cache, conf)

  if not self.loadConfigsAndProcessCmdLine(cache, conf, graph):
    return

  mainCommand(graph)
  if conf.hasHint(rintGCStats):
    conf.localReport(InternalReport(
      kind: rintGCStats, msg: GC_getStatistics()))

  if conf.errorCounter != 0: return
  when hasTinyCBackend:
    if conf.cmd == cmdTcc:
      tccgen.run(conf, conf.arguments)
  if optRun in conf.globalOptions:
    let output = conf.absOutFile
    case conf.cmd
    of cmdBackends, cmdTcc:
      let nimRunExe = getNimRunExe(conf)
      var cmdPrefix: string
      if nimRunExe.len > 0: cmdPrefix.add nimRunExe.quoteShell
      case conf.backend
      of backendC, backendCpp, backendObjc: discard
      of backendJs:
        # D20210217T215950:here this flag is needed for node < v15.0.0, otherwise
        # tasyncjs_fail` would fail, refs https://nodejs.org/api/cli.html#cli_unhandled_rejections_mode
        if cmdPrefix.len == 0: cmdPrefix = findNodeJs().quoteShell
        cmdPrefix.add " --unhandled-rejections=strict"
      of backendNimVm:
        if cmdPrefix.len == 0:
          cmdPrefix = changeFileExt(getAppDir() / "vmrunner", ExeExt)
      else: doAssert false, $conf.backend
      if cmdPrefix.len > 0: cmdPrefix.add " "
        # without the `cmdPrefix.len > 0` check, on windows you'd get a cryptic:
        # `The parameter is incorrect`
      execExternalProgram(
        conf, cmdPrefix & output.quoteShell & ' ' & conf.arguments, rcmdExecuting)

    of cmdDocLike, cmdRst2html, cmdRst2tex: # bugfix(cmdRst2tex was missing)
      if conf.arguments.len > 0:
        # reserved for future use
        localReport(conf, ExternalReport(
          kind: rextExpectedNoCmdArgument, cmdlineSwitch: $conf.cmd))

      openDefaultBrowser($output)
    else:
      # support as needed
      localReport(conf, ExternalReport(
        kind: rextUnexpectedRunOpt, cmdlineSwitch: $conf.cmd))

when declared(GC_setMaxPause):
  GC_setMaxPause 2_000

when compileOption("gc", "refc"):
  # the new correct mark&sweep collector is too slow :-/
  GC_disableMarkAndSweep()

when not defined(selftest):
  var conf = newConfigRef(cli_reporter.reportHook)
  conf.writeHook =
    proc(conf: ConfigRef, msg: string, flags: MsgFlags) =
      msgs.msgWrite(conf, msg, flags)

  conf.writelnHook =
    proc(conf: ConfigRef, msg: string, flags: MsgFlags) =
      conf.writeHook(conf, msg & "\n", flags)

  when defined(nimCompilerBracketTrace):
    import std/[strformat, tables, strutils]
    const fileTable = getCompilerBracketTraceFileMap()

    let
      traceDir = "/tmp"
      fileMap = traceDir / "file-map.csv"
      nodeKind = traceDir / "node-kind.csv"
      typeKind = traceDir / "type-kind.csv"
      trace = traceDir / "trace.csv"
      accessTraceKind = traceDir / "access-trace-kind.csv"
      accessTraceData = traceDir / "access-trace-data.csv"

    block writeFileMapping:
      let fileMap = open(fileMap, fmWrite)
      fileMap.writeLine("id,path")
      for file, id in fileTable:
        fileMap.writeLine(&"{id},\"{file}\"")

      fileMap.close()

    proc writeEnumMapping[E: enum](path: string) =
      let file = open(path, fmWrite)
      file.writeLine("id,name")
      for kind in low(E) .. high(E):
        file.writeLine(&"{kind.int},\"{kind}\"")

      file.close()

    writeEnumMapping[TNodeKind](nodeKind)
    writeEnumMapping[TTypeKind](typeKind)
    writeEnumMapping[AccessTraceKind](accessTraceKind)
    writeEnumMapping[AccessTraceData](accessTraceData)

    let csvFile = open(trace, fmWrite)
    csvFile.writeLine(join([
      "file",
      "line",
      "col",
      "kind",
      "index",
      "is_backwards",
      "data_kind",
      "to_id",
      "to_kind",
      "from_id",
      "from_kind"
    ], ","))

    var traceCount = 0
    accessTraceHook = proc(t: AccessTraceEntry) =
      inc traceCount
      csvFile.writeLine(
        &"{t.info.file},{t.info.line},{t.info.col},{t.kind.int},",
        &"{t.index},{t.backwards.int},{t.data.int},",
        if t.data == acdNode:
          &"{t.toNodeId},{t.toNodeKind.int},{t.fromNodeId},{t.fromNodeKind.int}"
        else:
          &"{t.toTypeId},{t.toTypeKind.int},{t.fromTypeId},{t.fromTypeKind.int}"
      )

  handleCmdLine(newIdentCache(), conf)

  when defined(nimCompilerBracketTrace):
    csvFile.close()
    echo &"""
Wrote {traceCount} traces into file - import it into sqlite using
the following script. It can be entered line-by-line or fed into the
`sqlite3` interpreter via pipe `sqlite3 trace.sqlite < script`


.mode csv

DROP TABLE IF EXISTS path_map;
CREATE TABLE path_map (id int, abs_path text);
.import --skip 1 "{fileMap}" path_map

DROP TABLE IF EXISTS node_kind;
CREATE TABLE node_kind (id int, kind_name text);
.import --skip 1 "{nodeKind}" node_kind

DROP TABLE IF EXISTS type_kind;
CREATE TABLE type_kind (id int, type_name text);
.import --skip 1 "{typeKind}" type_kind

DROP TABLE IF EXISTS access_trace_kind;
CREATE TABLE access_trace_kind(id int, kind_name text);
.import --skip 1 "{accessTraceKind}" acces_trace_kind

DROP TABLE IF EXISTS access_trace_data;
CREATE TABLE access_trace_data(id int, data_name text);
.import --skip 1 "{accessTraceData}" access_trace_data

DROP TABLE IF EXISTS trace;
CREATE TABLE trace (
   file_id int, -- id of the file
   line int, -- line number in file
   col int, -- column in the file
   kind int, -- kind of the trace entry
   acces_index int, -- index that was used in access
   is_backwards int, -- whether the index is backwards
   data_kind int, -- kind of the data used
   to_id int, --
   to_kind int, -- node/type kind that was assigned/evaluated to
   from_id int, --
   from_kind int -- node/type kind that was read/assigned from
);

.import --skip 1 "{trace}" trace
"""

  when declared(GC_setMaxPause):
    echo GC_getStatistics()

  msgQuit(int8(conf.errorCounter > 0))
