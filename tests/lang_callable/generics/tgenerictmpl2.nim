discard """
action: reject
knownIssue: "this is not at all how generics + default params should work"
"""

# This is not a "feature", it's completely broken that this works, `999`, the
# default parameter, does not satisfy all `T: any`. Nor can `T` be "inferred"
# as `int` in the default case as one can specify `T` breaking the default.
# This should plainly be illegal, as `T` is not equivalent to `untyped`.

#[
# old output that should _never_ have worked
  output: '''1
1
1
1
999
999
999
2'''
]#

# test if we can pass explicit generic arguments to generic templates
# based on bug report #3496

proc     tproc[T](t: T = 999) = echo t
template ttmpl[T](t: T = 999) = echo t

tproc(1)
tproc[int](1)
ttmpl(1)
ttmpl[int](1) #<- crash case #1

tproc[int]()
let _ = tproc[int]
ttmpl[int]()  #<- crash case #2
ttmpl[int]    #<- crash case #3

# but still allow normal use of [] on non-generic templates

template tarr: untyped = [1, 2, 3, 4]
echo tarr[1]
