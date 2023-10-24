block sequential_control_flow:
  var value = 0
  inc value
  inc value

  doAssert value == 2

block variable_shadowing:
  var value = 0
  block:
     var value = 0
     inc value
     doAssert value == 1

  inc value

  doAssert value == 1

block variable_shadowing_different_type:
  ## It is possible to declare new variable with the same but different name in
  ## the new scope.

  ## Declare integer variable `value` in the scope
  let value: int = 0
  doAssert value is int

  ## Block starts new scope
  block:
    ## It is allowed to declare *new* variable with the same name but different
    ## type in the scope.
    let value: string = ""

    ## Until scope is finished `value` will refer to the string variable.
    doAssert value is string
    doAssert value == ""

  ## After scope has ended `value` will refer to the first declared variable
  doAssert value is int
  doAssert value == 0

block scope_nesting_is_unlimited:
  ## There is no hard built-in limit on the maximum nesting of scopes
  let a = 1
  block:
    let a = 2
    block:
      let a = 3
      block:
        let a = 4
        block:
          let a = 5

          doAssert a == 5
        doAssert a == 4
      doAssert a == 3
    doAssert a == 2
  doAssert a == 1
