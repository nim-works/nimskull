discard """
description: '''
Specification of operator overloadin
'''
"""

block operator_precedence:
  block:
    ## Order of the infix expression evaluation depends on the first character
    ## of the operator. For example, `*` (and operators starting with `*`) have
    ## higher precedence (9) compared to the `+` operators (8), so `*` is evaluated first
    proc `+`(a, b: string): string = "+[" & b & a & "]"
    proc `*`(a, b: string): string = "+[" & b & a & "]"

    proc `+~`(a, b: string): string = "+[" & b & a & "]"
    proc `*~`(a, b: string): string = "+[" & b & a & "]"

    doAssert "a" + "b" * "c" == "a" + ("b" * "c")
    doAssert "a" +~ "b" *~ "c" == "a" +~ ("b" *~ "c")


  block infix_precedence:
    proc `=~`(a, b:    string): string = "=~["    & b & a & "]"
    proc `+~`(a, b:    string): string = "+~["    & b & a & "]"
    proc `-~`(a, b:    string): string = "-~["    & b & a & "]"
    proc `*~`(a, b:    string): string = "*~["    & b & a & "]"
    proc `/~`(a, b:    string): string = "/~["    & b & a & "]"
    proc `<~`(a, b:    string): string = "<~["    & b & a & "]"
    proc `>~`(a, b:    string): string = ">~["    & b & a & "]"
    proc `@~`(a, b:    string): string = "@~["    & b & a & "]"
    proc `$~`(a, b:    string): string = "$~["    & b & a & "]"
    proc `~~`(a, b:    string): string = "~~["    & b & a & "]"
    proc `&~`(a, b:    string): string = "&~["    & b & a & "]"
    proc `%~`(a, b:    string): string = "%~["    & b & a & "]"
    proc `|~`(a, b:    string): string = "|~["    & b & a & "]"
    proc `!~`(a, b:    string): string = "!~["    & b & a & "]"
    proc `?~`(a, b:    string): string = "?~["    & b & a & "]"
    proc `^~`(a, b:    string): string = "^~["    & b & a & "]"
    proc `.~`(a, b:    string): string = ".~["    & b & a & "]"
    proc `:~`(a, b:    string): string = ":~["    & b & a & "]"
    proc `\~`(a, b:    string): string = "\\~["   & b & a & "]"

    proc `?->`(a, b:   string): string = "?->["   & b & a & "]"
    proc `?=>`(a, b:   string): string = "?=>["   & b & a & "]"
    proc `?~>`(a, b:   string): string = "?~>["   & b & a & "]"

    proc `@=`(a, b:    string): string = "@=["    & b & a & "]"

    proc `and`(a, b:   string): string = "and["   & b & a & "]"
    proc `or`(a, b:    string): string = "or["    & b & a & "]"
    proc `not`(a, b:   string): string = "not["   & b & a & "]"
    proc `xor`(a, b:   string): string = "xor["   & b & a & "]"
    proc `shl`(a, b:   string): string = "shl["   & b & a & "]"
    proc `shr`(a, b:   string): string = "shr["   & b & a & "]"
    proc `div`(a, b:   string): string = "div["   & b & a & "]"
    proc `mod`(a, b:   string): string = "mod["   & b & a & "]"
    proc `in`(a, b:    string): string = "in["    & b & a & "]"
    proc `notin`(a, b: string): string = "notin[" & b & a & "]"
    proc `is`(a, b:    string): string = "is["    & b & a & "]"
    proc `isnot`(a, b: string): string = "isnot[" & b & a & "]"
    proc `of`(a, b:    string): string = "of["    & b & a & "]"
    proc `as`(a, b:    string): string = "as["    & b & a & "]"
    proc `from`(a, b:  string): string = "from["  & b & a & "]"



    ## Operators starting with `$` or `^` have the highest precedence and
    ## as a result they bind fist.
    doAssert "a" $~ "b" *~ "c" == ("a" $~ "b") *~ "c"
    doAssert "a" ^~ "b" *~ "c" == ("a" ^~ "b") *~ "c"

    ## Operators starting with `^` are right-associative, all other ones are
    ## right-associative.
    doAssert "a" ^~ "b" ^~ "c" == "a" ^~ ("b" ^~ "c")
    doAssert "a" $~ "b" $~ "c" == ("a" $~ "b") $~ "c"

    ## Next precedence level is for the operators starting with `*`, `%`,
    ## `\` or `/` as well as built-in operators `div`, `mod`, `shl` and `shr`.
    doAssert "a" *~ "b" +~ "c" == ("a" *~ "b") +~ "c"
    doAssert "a" %~ "b" +~ "c" == ("a" %~ "b") +~ "c"
    doAssert "a" \~ "b" +~ "c" == ("a" \~ "b") +~ "c"
    doAssert "a" /~ "b" +~ "c" == ("a" /~ "b") +~ "c"

    doAssert "a" div "b" +~ "c" == ("a" div "b") +~ "c"
    doAssert "a" mod "b" +~ "c" == ("a" mod "b") +~ "c"
    doAssert "a" shl "b" +~ "c" == ("a" shl "b") +~ "c"
    doAssert "a" shr "b" +~ "c" == ("a" shr "b") +~ "c"

   # TODO other precedence levels.