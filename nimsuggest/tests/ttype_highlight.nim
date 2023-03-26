type
  TypeA = int
  TypeB* = int
  TypeC {.exportc.} = int
  TypeD[T] = T
  TypeE* {.exportc.} = int#[!]#

discard """
$nimsuggest --tester $file
>highlight $1
highlight;;skType;;2;;2;;5
highlight;;skType;;3;;2;;5
highlight;;skType;;4;;2;;5
highlight;;skType;;5;;2;;5
highlight;;skType;;6;;2;;5
highlight;;skType;;2;;10;;3
highlight;;skType;;3;;11;;3
highlight;;skType;;4;;22;;3
highlight;;skType;;5;;13;;1
highlight;;skType;;6;;23;;3
highlight;;skType;;2;;10;;3
highlight;;skType;;3;;11;;3
highlight;;skType;;4;;22;;3
highlight;;skType;;6;;23;;3
"""
