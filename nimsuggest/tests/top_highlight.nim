import std/json

%*{}#[!]#

discard """
$nimsuggest --tester $file
>highlight $1
highlight;;skModule;;1;;10;;0
highlight;;skMacro;;3;;0;;2
highlight;;skMacro;;3;;0;;2
"""
