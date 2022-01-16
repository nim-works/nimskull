template foo() =
  {.warning: "foo".}
  
foo()

#[!]#
discard """
$nimsuggest --tester $file
>chk $1
chk;;skUnknown;;;;Warning;;$file;;2;;11;;"foo [User]";;0
"""
