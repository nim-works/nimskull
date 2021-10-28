discard """
description: '''
This tests comments in their various forms
'''
output:'''
1
2
3
4
5
6
7
8
9
10
11
12
13
'''
"""

# this file describes how comments do their thing

# comment before, doesn't affect the next line
echo 1
# comment after, doesn't affect the line before

# echo 2 this is a comment before the code
echo 2

echo 3 # this is a comment after the code

# empty comments like below are fine
#
# or with trailing white spaces
#

echo 4

#[we
can also
do
multi-
line comments
woooooooooooooooooooow]#

echo 5

#[]#

echo 6

echo #[since they're terminated we can sneak them in]# 7

#[]#echo 8 #[]# # yet more

#[ an outer comment won't let
#[ the inner comments close close it early, thanks to the power of counting! ]#
]#

let foo = 9
  ## while we're here let's document this
  ## we're gonna echo with it later
  ## ok, maybe now... (see below)
echo foo

echo 10 ## or keep it brief

let bar = 11
  ## maybe go all out?
  ## with multiple lines
  ## so you can read about
  ## all the importan things
  ##
  ## and what not
  ##
  ## :)

echo 11 # we can't document this guy, it's an oddball statement, hmmm :thinking_face:

let baz = 12
  ##[ or we can
                get
        really
            creative
yeaaaaaaah
  ]##
echo baz

echo 13

proc doStuff =
  ## Doc comments are valid ast so procs with them are valid!
  ## This is an alternative to `discard`, giving the block a statement to parse
proc doOtherStuff =
  ##[
    Even mul-
    tiline
      blocks
  ]##

block someBlock:
  ## Even inside block statements
block someBlock:
  ##[
    Even inside block statements
  ]##

for i in 0..10:
  ## Here too!
for i in 0..10:
  ##[
    also here.
  ]##

while false:
  ## Inside while aswell
while false:
  ##[
    Here aswell!
  ]##
