discard """
description: '''
This test covers statements, sequential evaluation and control flow.
'''
joinable: false
"""

## This section of specification explains what *statemetn* is, and two different
## types of statemets - "basic" and "noreturn". Examples of the noreturn statement
## should not be confused with general specification of control-flow alteration that
## is documented in :idx:`s09_control_flow/*.nim`

block sequential_control_flow:
  ## Multiple statements placed on the same level will be sequentially executed
  ## unless interrupted by a noreturn statement.

  ## Statement 1 - declares a variable
  var value = 0

  ## Statement 2 - assign 1 to a variable
  value = 1

  ## Statement 3 - asserts value of the variable is equal to 1
  doAssert value == 1

block noreturn_statement:
  ## noreturn statements enable manipulation of control flow for the program in
  ## different ways (depending on the context). Each specific type of the control
  ## flow-altering statement is more thoroughly discussed in their respective
  ## sections; here we will provide a basic overview of the subject.

  block break_statement:
    ## `break` statement can be used inside of loops - (`for`, `while`) and in
    ## `block` expressions.

    block break_unnamed_block:
      # Entered `block break_unnamed_block` path
      var value = 0
      block:
        # Entered `block` path
        value = 1
        break # `break` out of `block` path -> return to previous path of execution.
        value = 2
      # We now resume `block break_unnamed_block` path of execution

      ## Only single increment took place - `break` ended the flow of
      ## the block and resumed its parent flow; therefore, it prevented
      ## the second expression `value = 2` from being executed.
      doAssert value == 1

    block break_named_block:
      ## Unless specified by the `break` operation, it will break only its
      ## local path of execution. By specifying the name of the `block` you
      ## wish to break, you can escape several paths of execution at the same
      ## time.

      # enter `break_named_block` path
      var value = 0
      block name_1:
        # enter `name_1` path
        value = 1
        block name_2:
          # enter `name_2` path
          value = 2
          break name_1  # break out of `name_1` path; since `name_2` is a branch
                        # of `name_1`, we logically must end its execution too.
          value = 3
        value = 4

      ## `break name_1` prevented third and fourth assignment from being executed.
      doAssert value == 2

      ## Should we have instead used `break` or `break name_2` instead of
      ## `break name_1`, this doAssert would fail; the correct assertion would
      ## instead be `doAssert value == 4`.

    block break_while_loop:
      ## `break` in a `while` loop immediately stops the cyclic path execution. For more
      ## details and interactions see tests for `while` statements.
      var value = 0
      while true:
        ## Break in the `while` loop terminates it's execution.
        break
        value = 1

      doAssert value == 0

    block break_for_loop:
      ## Break in the `for` loop operates identically to the `while` case - when
      ## `break` is reached, the loop 'body' or 'path' of execution is stopped. For more details see
      ## specification on `iterators`.

      var value = 0
      for i in 0 .. 10:
        ## Break in the `for` loop terminates it's execution.
        break
        value = 2

      doAssert value == 0

  block continue_statement:
    ## `continue` statement can be used in the `while` and `for` loops to skip
    ## execution of the current loop. It effectively ends the current cyclic path
    ## of execution and returns to the beginning of the cyclic path. This is different
    ## to `break` which ends the cyclic path altogether and instead returns to
    ## its parent path.

    block continue_in_while:
      var value = 0
      var cond = true

      while cond:
        ## This cyclic path is only entered if `cond` is true.

        ## First statement in the loop is executed.
        cond = false
        ## Setting this variable to false will mean the next iteration of the
        ## cyclic path will not proceed since `cond` is no longer true.

        ## Upon reaching `continue`, control flow is transferred to the next
        ## iteration.
        continue
        ## The control flow will now return to the beginning of the cyclic path;
        ## it will re-evaluate `cond` as false and therefore not proceed with
        ## the next iteration of the cyclic path.


        ## Assignment to value is never reached.
        value = 1

      doAssert value == 0

    block continue_in_for:
      ## `continue` in the `for` loop is identical to the `while`
      ## loop - when reached, it transfers control to the next iteration. For
      ## more details see specification for the `iteratrors`.
      var preContinue = 0
      var postContinue = 0

      for i in 0 .. 4:
        ## Statement before `continue` is executed
        preContinue = preContinue + i
        ## Upon reaching `continue`, control flow is returned to the beginning
        ## of the cyclic path for the next iteration.
        continue
        postContinue = 9

      ## Statement placed after continue is never reached
      doAssert postContinue == 0

      ## Statement placed before `continue` is executed on every loop iteration
      doAssert preContinue == 0 + 1 + 2 + 3 + 4
      ## If you flatten out the for loop; this is what you would see:
      ## ```nim
      ## i = 0
      ## preContinue = preContinue + i
      ## # 1, 2, 3...
      ## i = 4
      ## preContinue = preContinue + i
      ## ```
      ##
      ## If we did not have the `continue` operation; we would instead see this:
      ## ```nim
      ## i = 0
      ## preContinue = preContinue + i
      ## postContinue = 9
      ## # 1, 2, 3...
      ## i = 4
      ## preContinue = preContinue + i
      ## postContinue = 9
      ## ```

  block return_statement:
    ## `return` can be used in procedures or macro declaration bodies.
    ## When reached, it immediately transfers control flow back to the caller of the
    ## function body (outside the function body). `return` can also be used to
    ## set resulting value of a procedure or macros - this functionality is
    ## covered in their respective sections.

    var value = 0

    value = 1

    # Unlike with `block` expressions, we do not immediately enter the `proc`
    # path/flow of execution. We first define it, and then enter it when the
    # procedure is called. Subsequently, breaking that path of execution would
    # return control to where the path was entered, not where it was defined.

    proc execReturn() =
      value = 2
      ## When `return` is reached, control flow is immediately transferred out
      ## of the procedure body
      return
      value = 3

    ## We execute the `execReturn` procedure - effectively we have entered the
    ## path of executions for that procedure. Procedures are looked at in more
    ## detail in their respective section.
    execReturn()
    # Path of operations continues when the procedure path reaches its end or
    # is `return`ed.

    doAssert value == 2

  block raise_statement:
    ## `raise` statement can be used inside of a procedure body or macro to
    ## create an exception. Concrete details about exceptions, their types and
    ## handling can be seen in their respective section of the specification.

    var value = 0
    proc raiseInside() =
      value = 1

      ## Specific type of the exception is not important in this case -
      ## `ValueError` would not be any different from `OSError` and alike
      raise newException(ValueError, "")
      value = 2

    try:
      ## When exception is raised it propagates through all function calls (if
      ## any) until it reaches the topmost level (main module/path from which the first
      ## function was called) or a `try` statement. For particular details on
      ## how exceptions affect control flow see respective part of the
      ## specification. In this case `try` was needed so subsequent `doAssert`
      ## would be executed correctly.
      raiseInside()

    except:
      # `try`, `except`, `finally`, will be explored in their respective section.
      discard

    doAssert value == 1

  block noreturn_annotation:
    ## Another type of a noreturn statement is a `quit()` procedure - it
    ## immediatelly terminates whole execution of the program once called.
    ## If looking at this from a paths or flow perspective, this is effectively
    ## ending every path of the program.

    quit()

    ## This statement will never be reached because `quit()` was called before.
    doAssert false
    ## If the statement was reached, then the assertion would fail, and the
    ## test would return a FAIL.
