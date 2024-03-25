discard """
  description: "Tests for untyped varargs parameters"
"""


import std/macros

block always_construct_a_bracket_wrapper:
  macro foo(args: varargs[typed]): string =
    doAssert args.kind == nnkBracket
    doAssert args.len == 2
    doAssert args[0].intVal == 1
    doAssert args[1].intVal == 2
    result = newStrLitNode("foo")
  
  doAssert foo(1, 2) == "foo"

  block even_if_passed_an_array:
    macro bar(args: varargs[typed]): string =
      doAssert args.kind == nnkBracket
      doAssert args.len == 1
      doAssert args[0].kind == nnkBracket
      doAssert args[0][0].intVal == 1
      doAssert args[0][1].intVal == 2
      result = newStrLitNode("bar")
    
    doAssert bar([1, 2]) == "bar"

block with_a_leading_argument:
  macro foo(this: int, that: typed, args: varargs[typed]): string =
    doAssert this.intVal == 1

    doAssert that.kind == nnkIntLit
    doAssert that.intVal == 2

    doAssert args.kind == nnkBracket
    doAssert args.len == 2
    doAssert args[0].intVal == 3
    doAssert args[1].intVal == 4

    result = newStrLitNode("foo")
  
  doAssert foo(1, 2, 3, 4) == "foo"

block typed_conversion_call_as_a_hack_for_echo:
  macro foo(args: varargs[typed, `$`]): string =
    var r = ""
    for n in args.items:
      case n.kind
      of nnkStrLit: r.add n.strVal
      of nnkHiddenCallConv: r.add repr(n[1])
      else: discard
          
    result = newStrLitNode(r)

  doAssert foo() == ""
  doAssert foo(1) == "1"
  doAssert foo(1, 2) == "12"
  doAssert foo(1, 2, "test") == "12test"
  doAssert foo([1, 2]) == "[1, 2]"

  block match_args_with_enough_for_each_trailing_param:
    discard "knownIssue: tvarargs_typed_knonwn_issue_1"