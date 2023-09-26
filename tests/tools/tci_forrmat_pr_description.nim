discard """
description: "Test the PR description formatter"
target: js
"""

import jsffi

let formatPR = require "../../tools/ci_format_pr_description.js"

proc formatPRdescription(formatPR: JSObject, str: cstring, maxLineLength: int): cstring {.importjs.}

let testingText = cstring"""Lorem ipsum dolor sit amet, consectetur i adipiscing elit. Nullam blandit mauris id venenatis tincidunt. Vestibulum at gravida sapien. Mauris tellus augue, aliquet sed laoreet blandit, pulvinar sed felis. Phasellus nec est vitae enim blandit facilisis.
Vestibulum fermentum ligula sit amet volutpat fermentum. Sed in faucibus orci. Pellentesque a dui ex. Curabitur sollicitudin, nulla id dignissim lacinia, odio mauris blandit nisi, eget auctor arcu odio nec est.

=========================================

## Summary
* The quick brown fox now jumps over the lazy dog


## Details
* THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG'S BACK 1234567890
* Hamburgevons

<!--This HTML comment should be removed-->

```
this is a triple quote delimited codeblock:
  it should not be linewrapped as should no other style of codeblock be
    there are also some birds sitting on a html comment:
                                       ___     ___
                                      (o o)   (o o)
                                     (  V  ) (  V  )
                             <!--------m-m-----m-m--------->
```
Here's some text.
 And some indented text, that will be linewrapped,
  but must not be dedented

<!--- This HTML comment
 spanning multiple lines should be removed
 too! -->

    and this is an indented codeblock:
      with a cat:
        !"#%&'()*+,-./012  _._     _,-'""`-._      3456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefgh
        "#%&'()*+,-./0123 (,-.'._,'(       |\`-/|  456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefghi
        #%&'()*+,-./01234     '-.-' \ )-`( , o o)  56789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefghij
        %&'()*+,-./012345           '-    \`_`"'-  6789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefghijk
                       <!--and-a-html-comment-too--->



https://www.some.url/that/is/quite/long/abcdefghijklmnopqrstuvw123467890xyz

(test) =========================================
(test) ======================================= i


   ~~~tokipona
<!-This is important!-->

	jan ali li kama lon nasin ni: ona li ken tawa li ken pali.
	jan ali li kama lon sama. jan ali li jo e ken pi pilin suli.
	jan ali li ken pali e wile pona ona.
	jan ali li jo e ken pi sona pona e ken pi pali pona.
	jan ali li wile pali nasin ni: ona li jan pona pi jan ante.

~~~

=================================== split this
================================= `keep this`

there are trailing spaces after this sentence      
and they must not leak into this line

the next line is indented
  and it should stay this way
"""

let testingTextWrapped = cstring"""Lorem ipsum dolor sit amet, consectetur i
adipiscing elit. Nullam blandit mauris id
venenatis tincidunt. Vestibulum at
gravida sapien. Mauris tellus augue,
aliquet sed laoreet blandit, pulvinar sed
felis. Phasellus nec est vitae enim
blandit facilisis.
Vestibulum fermentum ligula sit amet
volutpat fermentum. Sed in faucibus orci.
Pellentesque a dui ex. Curabitur
sollicitudin, nulla id dignissim lacinia,
odio mauris blandit nisi, eget auctor
arcu odio nec est.

=========================================

## Summary
* The quick brown fox now jumps over the
lazy dog


## Details
* THE QUICK BROWN FOX JUMPED OVER THE
LAZY DOG'S BACK 1234567890
* Hamburgevons



```
this is a triple quote delimited codeblock:
  it should not be linewrapped as should no other style of codeblock be
    there are also some birds sitting on a html comment:
                                       ___     ___
                                      (o o)   (o o)
                                     (  V  ) (  V  )
                             <!--------m-m-----m-m--------->
```
Here's some text.
 And some indented text, that will be
linewrapped,
  but must not be dedented



    and this is an indented codeblock:
      with a cat:
        !"#%&'()*+,-./012  _._     _,-'""`-._      3456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefgh
        "#%&'()*+,-./0123 (,-.'._,'(       |\`-/|  456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefghi
        #%&'()*+,-./01234     '-.-' \ )-`( , o o)  56789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefghij
        %&'()*+,-./012345           '-    \`_`"'-  6789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefghijk
                       <!--and-a-html-comment-too--->



https://www.some.url/that/is/quite/long/abcdefghijklmnopqrstuvw123467890xyz

(test)
=========================================
(test)
======================================= i


   ~~~tokipona
<!-This is important!-->

	jan ali li kama lon nasin ni: ona li ken tawa li ken pali.
	jan ali li kama lon sama. jan ali li jo e ken pi pilin suli.
	jan ali li ken pali e wile pona ona.
	jan ali li jo e ken pi sona pona e ken pi pali pona.
	jan ali li wile pali nasin ni: ona li jan pona pi jan ante.

~~~

=================================== split
this
================================= 
`keep this`

there are trailing spaces after this
sentence      
and they must not leak into this line

the next line is indented
  and it should stay this way
"""

echo formatPR.formatPRdescription(testingText, 41)
assert formatPR.formatPRdescription(testingText, 41) == testingTextWrapped

