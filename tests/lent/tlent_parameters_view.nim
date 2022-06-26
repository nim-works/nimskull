discard """
  errormsg: "invalid type: 'lent Test' in this context: 'proc (self: lent Test)' for proc"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/17621
       [View types] lent parameter can cause a segfault
    . Expected a compiler error - which is what happens without
      {.experimental: "views".}, based on #16898 (comment))
      or an error because self is modified through a lent.
    '''
"""

# bug #17621
{.experimental: "views".}

type Test = ref object
  foo: int

proc modify(self: lent Test) =
  self.foo += 1

let test = Test(foo: 12)
modify(test)

