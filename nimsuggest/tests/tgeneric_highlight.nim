newSeq[int]()
system.newSeq[int]()#[!]#
offsetOf[int]()
# note: the ``offsetOf`` template is not reported by the command because the
# invocation doesn't get through overload resolution (the arguments are
# missing)

discard """
$nimsuggest --tester $file
>highlight $1
highlight;;skType;;1;;7;;3
highlight;;skProc;;1;;0;;6
highlight;;skType;;2;;14;;3
highlight;;skProc;;2;;7;;6
highlight;;skType;;3;;9;;3
"""
