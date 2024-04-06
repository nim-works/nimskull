discard """
  description: '''Example program that demonstrates how to use the compiler as
  an API to embed into your own projects.
  '''
  cmd: '''nim c --warnings:off --hints:off $file'''
  output: '''top level statements are executed!
(ival: 10, fval: 2.0)
2.0
my secret
11
12
raising VMQuit
'''
  joinable: "false"
"""


import
  std/[
    os
  ],
  compiler/ast/[
    ast,
    lineinfos,
    reports,
    llstream
  ],
  compiler/front/[
    options
  ],
  compiler/vm/[
    vmdef,
    vmhooks,
    nimeval,
  ]

proc initInterpreter(script: string, hook: ReportHook): Interpreter =
  let std = findNimStdLibCompileTime()
  result = createInterpreter(
    scriptName = script,
    hook = hook,
    searchPaths = [
      std,
      parentDir(currentSourcePath),
      std / "pure",
      std / "core"])

type VMQuit = object of CatchableError

proc vmReport(config: ConfigRef, report: Report): TErrorHandling {.gcsafe.} =
  if config.severity(report) == rsevError and
     config.errorCounter >= config.errorMax:

    echo "raising VMQuit"
    raise newException(VMQuit, "Script error")

  elif report.kind == rintEchoMessage:
    echo report.internalReport.msg

proc newFloatTypeNode(val: float, typ: PType): PNode =
  result = newFloatNode(nkFloatLit, val)
  result.typ = typ

proc main() =
  let i = initInterpreter("myscript.nim", vmReport)
  i.implementRoutine("*", "exposed", "addFloats", proc (a: VmArgs) =
    setResult(a, getFloat(a, 0) + getFloat(a, 1) + getFloat(a, 2))
  )
  i.evalScript()
  let foreignProc = i.selectRoutine("hostProgramRunsThis")
  if foreignProc == nil:
    quit "script does not export a proc of the name: 'hostProgramRunsThis'"
  let typ = foreignProc.typ[1]
  let res = i.callRoutine(foreignProc, [newFloatTypeNode(0.9, typ),
                                        newFloatTypeNode(0.1, typ)])
  doAssert res.kind == nkFloatLit
  echo res.floatVal

  let foreignValue = i.selectUniqueSymbol("hostProgramWantsThis")
  if foreignValue == nil:
    quit "script does not export a global of the name: hostProgramWantsThis"
  let val = i.getGlobalValue(foreignValue)
  doAssert val.kind in {nkStrLit..nkTripleStrLit}
  echo val.strVal
  i.destroyInterpreter()

main()

block issue9180:
  proc evalString(code: string, moduleName = "script.nim") =
    let stream = llStreamOpen(code)
    let std = findNimStdLibCompileTime()
    var intr = createInterpreter(
      scriptName = moduleName,
      searchPaths = [std, std / "pure", std / "core"],
      hook = vmReport)

    intr.evalScript(stream)
    destroyInterpreter(intr)
    llStreamClose(stream)

  evalString("echo 10+1")
  evalString("echo 10+2")

block error_hook:
  let i = initInterpreter("invalid.nim", vmReport)
  doAssertRaises(VMQuit):
    i.evalScript()

block isNimSkull_define:
  const isNimSkull {.booldefine.} = false
  doAssert isNimSkull, "define 'isNimSkull' is not set"
