discard """
$nimsuggest --tester $file
>def $1
def;;skModule;;minclude_types;;*;;$file;;15;;15;;"";;100
>def $2
def;;skModule;;minclude_types;;*;;$file;;16;;15;;"";;100
>def $3
def;;skModule;;minclude_import;;*;;$file;;18;;10;;"";;100
>def $4
def;;skModule;;minclude_types;;*;;$file;;21;;13;;"";;100
>def $5
def;;skModule;;mstrutils;;*;;$file;;22;;15;;"";;100
"""

import fixtures/[minclude_import, minclud#[!]#e_types]
import fixtures/[minclude_types as typ#[!]#es]
# test with multi-line import statements:
import
  fixtures/[
    minclu#[!]#de_import
  ]
from fixtures/mincl#[!]#ude_types import Greet
import fixtures/mst#[!]#rutils except replace
