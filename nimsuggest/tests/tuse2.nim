import fixtures/module_a
global = 1

discard """
$nimsuggest --tester $file
>use $path/fixtures/module_a.nim:2:1
def;;skVar;;module_a.global;;int;;*;;1;;4;;"";;100
use;;skVar;;module_a.global;;int;;*;;2;;0;;"";;100
use;;skVar;;module_a.global;;int;;$file;;2;;0;;"";;100
>def $path/fixtures/module_a.nim:2:1
def;;skVar;;module_a.global;;int;;*;;1;;4;;"";;100
>use $path/tuse2.nim:2:1
def;;skVar;;module_a.global;;int;;*;;1;;4;;"";;100
use;;skVar;;module_a.global;;int;;*;;2;;0;;"";;100
use;;skVar;;module_a.global;;int;;$file;;2;;0;;"";;100
"""
