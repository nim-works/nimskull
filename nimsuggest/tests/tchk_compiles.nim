discard compiles(2 + "hello")

#[!]#
discard """
$nimsuggest --tester $file
>chk $1
"""
