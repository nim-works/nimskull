discard """
description: '''
Pragmas for wrapping C++ code in nim.
'''
targets: "cpp"
#specifying target does not work when I do `testament run` 
cmd: "nim cpp -r $options $file"
joinable: false
"""

const h = "t02_cxx_interop.nim.hpp"

block complete_struct:
  ## If C or C++ type has been mapped *exactly*, 1:1, it is possible to annotate 
  ## nim wrapper with `completeStruct` pragma - in this case `sizeof()` operation
  ## would work correctly (that is - match with `sizeof()` on the C++ side).
  type
    CxxComplete {.importcpp, header: h, completeStruct.} = object
      field1: cint
      field2: cint

  ## `selfSizeof()` is needed in order to test `sizeof()` correctness - it simply 
  ## returns size of the object as seen on the C++ side.
  proc selfSizeof(it: CxxComplete): cint {.importcpp: "#.selfSizeof()".}

  doAssert sizeof(CxxComplete) == sizeof(cint) * 2
  doAssert sizeof(CxxComplete) == CxxComplete().selfSizeof()

  ## If sizeo of the object is know it is even possible to perform low-level 
  ## cast operations. Note that first element in the array has to be explicitly
  ## converted to the `cint`, since default `int` type does guarantee full 
  ## binary compatibility with C integger type.
  let values = cast[array[2, CxxComplete]]([cint(1), 2, 3, 4])

  doAssert values[0] == CxxComplete(field1: 1, field2: 2)
  doAssert values[1] == CxxComplete(field1: 3, field2: 4)

block wrapping_operators:
  type
    TestOperators {.importcpp, header: h, bycopy.} = object
      value: cint

  block addition:
    proc `+`(lhs, rhs: TestOperators): TestOperators {.importcpp: "(# + #)".}

    doAssert (TestOperators(value: 1) + TestOperators(value: 2)).value == 3

  block set_to_array:
    ## Nim does not have an alternative to C++ lvalvue reference type (T& is returned 
    ## by `[]` operator when resulting value needs to be modified). In order to be 
    ## able to set value to array index it needs to be wrapped using `#[#] = #` pattern
    proc `[]=`(lhs: var TestOperators, idx: int, value: int) {.importcpp: "#[#] = #".}

    var test = TestOperators(value: 12)
    doAssert test.value == 12

    test[1] = 2

    doAssert test.value == 2

    ## It is also possible to wrap `[]` operator as pure getter as well.
    proc `[]`(lhs: var TestOperators, idx: int): int {.importcpp: "#[#]".} 

    let value = test[10]

    doAssert test.value == 2 + 10
    doAssert value == test.value


block wrapping_template_cxx_types:
  type
    CxxTemplate[T1, T2] {.importcpp: "CxxTemplate", header: h.} = object
      field1: T1
      field2: T2

  block wrap_default_constructor:
    proc initCxxTemplate[T1, T2](): CxxTemplate[T1, T2] = 
      proc aux(tmp1: typedesc[T1], tmp2: typedesc[T2]): CxxTemplate[T1, T2] 
        {.importcpp: "CxxTemplate<'*1, '*2>()".}

      return aux(T1, T2)

    var wrap = initCxxTemplate[int, float]()
    wrap.field1 = 0
    wrap.field2 = 0.0

  block wrap_constructor_with_arguments:
    proc initCxxTemplate[T1, T2](arg1: T1, arg2: T2): CxxTemplate[T1, T2] 
      {.importcpp: "CxxTemplate<'*1, '*2>(@)".}

    let wrap = initCxxTemplate(12, 2)

    doAssert wrap.field1 == 12
    doAssert wrap.field2 == 2

block wrapping_cxx_type_with_default_constructor:
  type
    CxxWithDefault {.importcpp, header: h.} = object
      field: cint


  proc initCxxWithDefault(): CxxWithDefault {.constructor, importcpp: "CxxWithDefault()".}
  
  doAssert initCxxWithDefault().field == 12

  let val = initCxxWithDefault()
  doAssert val.field == 12

  doAssert CxxWithDefault(field: 13).field == 13

block wrapping_cxx_type_without_default_constructor:
  type
    CxxNoDefault {.importcpp, header: h.} = object
      field: cint

  proc initCxxNoDefault(arg: cint): CxxNoDefault {.importcpp: "CxxNoDefault(@)", constructor.}

  when false: # FIXME this code fails with Cxx codegen error - `constructor` does not work properly
              # it still creates a temporary variable.
    doAssert initCxxNoDefault(10).field == 10


