#
#
#            Nim's Runtime Library
#        (c) Copyright 2017 Nim Authors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.

## This module implements types and macros for writing asynchronous code
## for the JS backend. It provides tools for interaction with JavaScript async API-s
## and libraries, writing async procedures in |NimSkull| and converting
## callback-based code to promises.
##
## A |NimSkull| procedure is asynchronous when it includes the `{.async.}`
## pragma. If the return type is not `Future[T]`, it is opaquely turned
## into one first. A `Future[void]` return type is assumed by default.
##
## This is roughly equivalent to the `async` keyword in JavaScript code.
##
## A call to an asynchronous procedure usually needs `await` to wait for
## the completion of the `Future`.
##
## .. code-block:: nim
##   var game = await loadGame(name)
##
## Often, you might work with callback-based API-s. You can wrap them with
## asynchronous procedures using promises and `newPromise`:
##
## .. code-block:: nim
##   proc loadGame(name: string): Future[Game] =
##     var promise = newPromise() do (resolve: proc(response: Game)):
##       cbBasedLoadGame(name) do (game: Game):
##         resolve(game)
##     return promise
##
## Forward definitions work properly, you just need to always add the `{.async.}` pragma:
##
## .. code-block:: nim
##   proc loadGame(name: string): Future[Game] {.async.}
##
## JavaScript compatibility
## ========================
##
## |NimSkull| generates JavaScript code that uses the ``Promise`` and ``Error``
## APIs, both which are supported by most modern versions of browsers, Node.js
## and Electron.
##
## If you need to use this module with older versions of JavaScript, you can
## use a tool that backports the resulting JavaScript code, as babel.

# xxx code-block:: javascript above gives `LanguageXNotSupported` warning.

when not defined(js) and not defined(nimsuggest):
  {.fatal: "Module asyncjs is designed to be used with the JavaScript backend.".}

import std/jsffi
import std/macros

type
  PromiseJs* {.importjs: "Promise".} = ref object
    ## A JavaScript Promise.

  Future*[T] = distinct PromiseJs
    ## Wraps the return type of an asynchronous procedure.

  Error* {.importjs: "Error".} = ref object of Exception
    ## https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error
    message*: cstring

{.push raises: [].} # the imported procedures don't raise

proc reject(e: cstring): PromiseJs {.importjs: "Promise.reject(new Error(#))".}
proc reject(e: Error): PromiseJs {.importjs: "Promise.reject(#)".}

proc resolve(): PromiseJs {.importjs: "(undefined)".}
proc resolve[T](x: T): PromiseJs {.importjs: "Promise.resolve(#)".}

proc jsCatch(p: PromiseJs, x: proc): PromiseJs {.importjs: "catch".}
proc jsThen(p: PromiseJs, x: proc): PromiseJs {.importjs: "then".}

{.pop.}

proc replaceReturn(n: NimNode, with: NimNode): NimNode =
  ## In-place replaces all usages of the 'result' identifier in `n` with
  ## `with`.
  case n.kind
  of nnkIdent:
    if n.eqIdent("result"):
      with
    else:
      n
  of nnkReturnStmt:
    let callee = bindSym("resolve")
    if with.isNil:
      nnkReturnStmt.newTree(newCall(callee))
    elif n[0].kind == nnkEmpty:
      nnkReturnStmt.newTree(newCall(callee, with))
    else:
      nnkStmtList.newTree(
        newAssignment(with, replaceReturn(n[0], with)),
        nnkReturnStmt.newTree(newCall(callee, with))
      )
  of RoutineNodes:
    n # don't touch nested routines
  else:
    for i in 0..<n.len:
      n[i] = replaceReturn(n[i], with)
    n

macro asyncAux(fut, real: typedesc, arg: untyped): untyped =
  ## Implements the async transformation. The idea is to turn the tagged
  ## routine into a closure iterator (i.e., a resumable procedure), with
  ## every `await` turned into a yield. Prior to yielding, a closure that
  ## assigns the awaited result to a local and resumes the iterator is
  ## chained to the promise.
  ##
  ## Conceptually, this is very similar to how JavaScript's async functions
  ## work.
  if arg.kind notin {nnkProcDef, nnkFuncDef, nnkLambda, nnkDo}:
      error("Cannot transform this node kind into an async proc." &
            " proc definition or lambda node expected.")

  result = arg
  result.params[0] = fut.getTypeInst()[1]

  if arg.body.len == 0:
    return # forward declaration; don't produce a body

  let
    real = real.getTypeInst()[1] # unwrap the typedesc
    isVoid = real.typeKind == ntyVoid
    self = genSym(nskVar, "self")
    err = genSym(nskParam, "err")

  proc newDef(name, typ: NimNode): NimNode =
    nnkIdentDefs.newTree(name, typ, newEmptyNode())

  # `runner` is the routine that does the actual work. The original
  # procedure is turned into a thunk for invoking the iterator
  let runner = newProc(name = arg.name,
                       params = [bindSym"PromiseJs",
                                 newDef(err, nnkRefTy.newTree(
                                                bindSym"Exception"))],
                       procType = nnkIteratorDef,
                       pragmas = nnkPragma.newTree(ident"closure"))
  runner.body = nnkStmtList.newTree()

  # inject the ``await`` routines before the body:
  let preamble = quote do:
    proc genericCatch(e: ref Exception): PromiseJs =
      # define outside of the template to reduce executable size
      `self`(e)

    template await[T](f: Future[T]): T {.used.} =
      var res: T
      yield PromiseJs(f).jsCatch(genericCatch).jsThen(
        proc(x: sink T): PromiseJs =
          res = move x
          # resume the coroutine with no error:
          return `self`(nil)
      )
      if `err` != nil: # handle the error
        raise `err`
      res

    proc voidCont(): PromiseJs {.used.} = `self`(nil)

    template await(f: Future[void]) {.used.} =
      yield PromiseJs(f).jsCatch(genericCatch).jsThen(voidCont)
      if `err` != nil:
        raise `err`

  runner.body.add(preamble)

  # setup and emit the result variable definition:
  var resultVar: NimNode = nil
  if not isVoid:
    resultVar = genSym(nskVar, "res")
    runner.body.add nnkVarSection.newTree(
      nnkIdentDefs.newTree(resultVar, real, newEmptyNode()))

  # now comes the patched body:
  for child in replaceReturn(arg.body, resultVar).items:
    runner.body.add(child)

  # wrap the body in a try/except that turns uncaught exceptions into rejected
  # promises:
  let body = runner.body
  runner.body = quote:
    try:
      `body`
    except CatchableError as e:
      return reject(cstring(e.msg))
    except Error as e:
      return reject(e)

  runner.body = nnkStmtList.newTree(runner.body)

  # emit the final return statement:
  if isVoid:
    runner.body.add nnkReturnStmt.newTree(
      newCall(bindSym"resolve"))
  else:
    runner.body.add nnkReturnStmt.newTree(
      newCall(bindSym"resolve", resultVar))

  let name = runner.name
  # emit the start-up thunk:
  result.body = quote do:
    var `self`: iterator(e: ref Exception): `PromiseJs`
    `runner`
    `self` = `name`
    return `fut`(`self`(nil))

template maybeFuture(T): untyped =
  # avoids `Future[Future[T]]`
  when T is Future: T
  else: Future[T]

template unwrap[T](_: typedesc[Future[T]]): typedesc = T
template unwrap(T: typedesc): typedesc = T

proc generateJsasync(arg: NimNode): NimNode =
  let res =
    if arg.params[0].kind == nnkEmpty:
      ident"void"
    else:
      arg.params[0]

  result = newCall(bindSym"asyncAux",
                   newCall(bindSym"maybeFuture", res),
                   newCall(bindSym"unwrap", res),
                   arg) # the original def

macro async*(arg: untyped): untyped =
  ## Macro that turns normal procedures into awaitable procedures. Within
  ## the body, the `await` procedure is available, for awaiting
  ## `Future <#Future>` instances.
  if arg.kind == nnkStmtList:
    result = newStmtList()
    for oneProc in arg:
      result.add generateJsasync(oneProc)
  else:
    result = generateJsasync(arg)

proc newPromise*[T](handler: proc(resolve: proc(response: T))): Future[T] {.importjs: "(new Promise(#))".}
  ## A helper for wrapping callback-based functions
  ## into promises and async procedures.

proc newPromise*(handler: proc(resolve: proc())): Future[void] {.importjs: "(new Promise(#))".}
  ## A helper for wrapping callback-based functions
  ## into promises and async procedures.

when defined(nimExperimentalAsyncjsThen):
  import std/private/since
  since (1, 5, 1):
    #[
    TODO:
    * map `Promise.all()`
    * proc toString*(a: Error): cstring {.importjs: "#.toString()".}

    Note:
    We probably can't have a `waitFor` in js in browser (single threaded), but maybe it would be possible
    in in nodejs, see https://nodejs.org/api/child_process.html#child_process_child_process_execsync_command_options
    and https://stackoverflow.com/questions/61377358/javascript-wait-for-async-call-to-finish-before-returning-from-function-witho
    ]#

    type OnReject* = proc(reason: Error)

    proc then*[T](future: Future[T], onSuccess: proc, onReject: OnReject = nil): auto =
      ## See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then
      ## Returns a `Future` from the return type of `onSuccess(T.default)`.
      runnableExamples("-d:nimExperimentalAsyncjsThen"):
        from std/sugar import `=>`

        proc fn(n: int): Future[int] {.async.} =
          if n >= 7: raise newException(ValueError, "foobar: " & $n)
          else: result = n * 2

        proc asyncFact(n: int): Future[int] {.async.} =
          if n > 0: result = n * await asyncFact(n-1)
          else: result = 1

        proc main() {.async.} =
          block: # then
            assert asyncFact(3).await == 3*2
            assert asyncFact(3).then(asyncFact).await == 6*5*4*3*2
            let x1 = await fn(3)
            assert x1 == 3 * 2
            let x2 = await fn(4)
              .then((a: int) => a.float)
              .then((a: float) => $a)
            assert x2 == "8.0"

          block: # then with `onReject` callback
            var witness = 1
            await fn(6).then((a: int) => (witness = 2), (r: Error) => (witness = 3))
            assert witness == 2
            await fn(7).then((a: int) => (witness = 2), (r: Error) => (witness = 3))
            assert witness == 3

      template impl(call): untyped =
        # see D20210421T014713
        when typeof(block: call) is void:
          var ret: Future[void]
        else:
          var ret = default(maybeFuture(typeof(call)))
        typeof(ret)
      when T is void:
        type A = impl(onSuccess())
      else:
        type A = impl(onSuccess(default(T)))
      var ret: A
      asm "`ret` = `future`.then(`onSuccess`, `onReject`)"
      return ret

    proc catch*[T](future: Future[T], onReject: OnReject): Future[void] =
      ## See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch
      runnableExamples("-d:nimExperimentalAsyncjsThen"):
        from std/sugar import `=>`
        from std/strutils import contains

        proc fn(n: int): Future[int] {.async.} =
          if n >= 7: raise newException(ValueError, "foobar: " & $n)
          else: result = n * 2

        proc main() {.async.} =
          var reason: Error
          await fn(6).catch((r: Error) => (reason = r)) # note: `()` are needed, `=> reason = r` would not work
          assert reason == nil
          await fn(7).catch((r: Error) => (reason = r))
          assert reason != nil
          assert  "foobar: 7" in $reason.message

        discard main()

      asm "`result` = `future`.catch(`onReject`)"
