discard """
$nimsuggest --tester $file
>def $1
def;;skModule;;minclude_types;;*;;$file;;10;;15;;"";;100
>def $2
def;;skModule;;minclude_types;;*;;$file;;11;;15;;"";;100
>def $3
def;;skModule;;minclude_import;;*;;$file;;13;;10;;"";;100
"""
import fixtures/[minclude_import, minclud#[!]#e_types]
import fixtures/[minclude_types as typ#[!]#es]
import
  fixtures/[
    minclu#[!]#de_import
  ]
