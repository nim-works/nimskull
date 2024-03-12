#
#
#            Nim's Runtime Library
#        (c) Copyright 2021 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module provides basic primitives for creating parallel programs.
## A `Task` should be only owned by a single Thread, it cannot be shared by threads.

import std/[macros, isolation, typetraits]
import system/ansi_c

export isolation


when compileOption("threads"):
  from std/effecttraits import isGcSafe


#
# proc hello(a: int, b: string) =
#   echo $a & b
#
# let literal = "Nim"
# let t = toTask(hello(521, literal))
#
#
# is roughly converted to
#
# type
#   ScratchObj_369098780 = object
#     a: int
#     b: string
#
# let scratch_369098762 = cast[ptr ScratchObj_369098780](c_calloc(csize_t 1,
#     csize_t sizeof(ScratchObj_369098780)))
# if scratch_369098762.isNil:
#   raise newException(OutOfMemDefect, "Could not allocate memory")
# block:
#   var isolate_369098776 = isolate(521)
#   scratch_369098762.a = extract(isolate_369098776)
#   var isolate_369098778 = isolate(literal)
#   scratch_369098762.b = extract(isolate_369098778)
# proc hello_369098781(args`gensym3: pointer) {.nimcall.} =
#   let objTemp_369098775 = cast[ptr ScratchObj_369098780](args`gensym3)
#   let :tmp_369098777 = objTemp_369098775.a
#   let :tmp_369098779 = objTemp_369098775.b
#   hello(a = :tmp_369098777, b = :tmp_369098779)
#
# proc destroyScratch_369098782(args`gensym3: pointer) {.nimcall.} =
#   let obj_369098783 = cast[ptr ScratchObj_369098780](args`gensym3)
#   =destroy(obj_369098783[])
# let t = Task(callback: hello_369098781, args: scratch_369098762, destroy: destroyScratch_369098782)
#


type
  Task* = object ## `Task` contains the callback and its arguments.
    callback: proc (args: pointer) {.nimcall, gcsafe.}
    args: pointer
    destroy: proc (args: pointer) {.nimcall, gcsafe, raises: [].}


proc `=copy`*(x: var Task, y: Task) {.error.}

proc `=destroy`*(t: var Task) {.inline, gcsafe.} =
  ## Frees the resources allocated for a `Task`.
  if t.args != nil:
    if t.destroy != nil:
      t.destroy(t.args)
    c_free(t.args)

proc invoke*(task: Task) {.inline, gcsafe.} =
  ## Invokes the `task`.
  assert task.callback != nil
  task.callback(task.args)

template checkIsolate(scratchAssignList: seq[NimNode], procParam, scratchDotExpr: NimNode) =
  # block:
  #   var isoTempA = isolate(521)
  #   scratch.a = extract(isolateA)
  #   var isoTempB = isolate(literal)
  #   scratch.b = extract(isolateB)
  let isolatedTemp = genSym(nskTemp, "isoTemp")
  scratchAssignList.add newVarStmt(isolatedTemp, newCall(newidentNode("isolate"), procParam))
  scratchAssignList.add newAssignment(scratchDotExpr,
      newcall(newIdentNode("extract"), isolatedTemp))

template addAllNode(assignParam: NimNode, procParam: NimNode) =
  let scratchDotExpr = newDotExpr(scratchIdent, formalParams[i][0])

  checkIsolate(scratchAssignList, procParam, scratchDotExpr)

  let tempNode = genSym(kind = nskTemp, ident = formalParams[i][0].strVal)
  callNode.add nnkExprEqExpr.newTree(formalParams[i][0], tempNode)
  tempAssignList.add newLetStmt(tempNode, newDotExpr(objTemp, formalParams[i][0]))
  scratchRecList.add newIdentDefs(newIdentNode(formalParams[i][0].strVal), assignParam)

macro toTask*(e: typed{nkCall | nkInfix | nkPrefix | nkPostfix | nkCommand | nkCallStrLit}): Task =
  ## Converts the call and its arguments to `Task`.
  runnableExamples("--gc:orc"):
    proc hello(a: int) = echo a

    let b = toTask hello(13)
    assert b is Task

  doAssert getTypeInst(e).typeKind == ntyVoid

  when compileOption("threads"):
    if not isGcSafe(e[0]):
      error("'toTask' takes a GC safe call expression")

  if hasClosure(e[0]):
    error("closure call is not allowed")

  if e.len > 1:
    let scratchIdent = genSym(kind = nskTemp, ident = "scratch")
    let impl = e[0].getTypeInst

    when defined(nimTasksDebug):
      echo impl.treeRepr
      echo e.treeRepr
    let formalParams = impl[0]

    var
      scratchRecList = newNimNode(nnkRecList)
      scratchAssignList: seq[NimNode]
      tempAssignList: seq[NimNode]
      callNode: seq[NimNode]

    let
      objTemp = genSym(nskTemp, ident = "objTemp")

    for i in 1 ..< formalParams.len:
      var param = formalParams[i][1]

      if param.kind == nnkBracketExpr and param[0].eqIdent("sink"):
        param = param[0]

      if param.typeKind in {ntyExpr, ntyStmt}:
        error("'toTask'ed function cannot have a 'typed' or 'untyped' parameter")

      case param.kind
      of nnkVarTy:
        error("'toTask'ed function cannot have a 'var' parameter")
      of nnkBracketExpr:
        if param[0].typeKind == ntyTypeDesc:
          callNode.add nnkExprEqExpr.newTree(formalParams[i][0], e[i])
        elif param[0].typeKind in {ntyVarargs, ntyOpenArray}:
          if param[1].typeKind in {ntyExpr, ntyStmt}:
            error("'toTask'ed function cannot have a 'typed' or 'untyped' parameter")
          let
            seqType = nnkBracketExpr.newTree(newIdentNode("seq"), param[1])
            seqCallNode = newcall("@", e[i])
          addAllNode(seqType, seqCallNode)
        else:
          addAllNode(param, e[i])
      of nnkBracket, nnkObjConstr:
        # passing by static parameters
        # so we pass them directly instead of passing by scratchObj
        callNode.add nnkExprEqExpr.newTree(formalParams[i][0], e[i])
      of nnkSym, nnkPtrTy:
        addAllNode(param, e[i])
      of nnkCharLit..nnkNilLit:
        callNode.add nnkExprEqExpr.newTree(formalParams[i][0], e[i])
      else:
        error("not supported type kinds")

    let scratchObjType = genSym(kind = nskType, ident = "ScratchObj")
    let scratchObj = nnkTypeSection.newTree(
                      nnkTypeDef.newTree(
                        scratchObjType,
                        newEmptyNode(),
                        nnkObjectTy.newTree(
                          newEmptyNode(),
                          newEmptyNode(),
                          scratchRecList
                        )
                      )
                    )


    let scratchObjPtrType = quote do:
      cast[ptr `scratchObjType`](c_calloc(csize_t 1, csize_t sizeof(`scratchObjType`)))

    let scratchLetSection = newLetStmt(
      scratchIdent,
      scratchObjPtrType
    )

    let scratchCheck = quote do:
      if `scratchIdent`.isNil:
        raise newException(OutOfMemDefect, "Could not allocate memory")

    var stmtList = newStmtList()
    stmtList.add(scratchObj)
    stmtList.add(scratchLetSection)
    stmtList.add(scratchCheck)
    stmtList.add(nnkBlockStmt.newTree(newEmptyNode(), newStmtList(scratchAssignList)))

    var functionStmtList = newStmtList()
    let funcCall = newCall(e[0], callNode)
    functionStmtList.add tempAssignList
    functionStmtList.add funcCall

    let funcName = genSym(nskProc, e[0].strVal)
    let destroyName = genSym(nskProc, "destroyScratch")
    let objTemp2 = genSym(ident = "obj")
    let tempNode = quote("@") do:
        `=destroy`(@objTemp2[])

    result = quote do:
      `stmtList`

      proc `funcName`(args: pointer) {.gcsafe, nimcall.} =
        let `objTemp` = cast[ptr `scratchObjType`](args)
        `functionStmtList`

      proc `destroyName`(args: pointer) {.gcsafe, nimcall.} =
        let `objTemp2` = cast[ptr `scratchObjType`](args)
        `tempNode`

      Task(callback: `funcName`, args: `scratchIdent`, destroy: `destroyName`)
  else:
    let funcCall = newCall(e[0])
    let funcName = genSym(nskProc, e[0].strVal)

    result = quote do:
      proc `funcName`(args: pointer) {.gcsafe, nimcall.} =
        `funcCall`

      Task(callback: `funcName`, args: nil)

  when defined(nimTasksDebug):
    echo result.repr

runnableExamples("--gc:orc"):
  block:
    var num = 0
    proc hello(a: int) = inc num, a

    let b = toTask hello(13)
    b.invoke()
    assert num == 13
    # A task can be invoked multiple times
    b.invoke()
    assert num == 26

  block:
    type
      Runnable = ref object
        data: int

    var data: int
    proc hello(a: Runnable) {.nimcall.} =
      a.data += 2
      data = a.data


    when false:
      # the parameters of call must be isolated.
      let x = Runnable(data: 12)
      let b = toTask hello(x) # error ----> expression cannot be isolated: x
      b.invoke()

    let b = toTask(hello(Runnable(data: 12)))
    b.invoke()
    assert data == 14
    b.invoke()
    assert data == 16
