discard """
  target: "!js !vm"
  description: '''
   . From https://github.com/nim-lang/Nim/issues/8829
     template that overloads [] accessor does not compile
   '''
"""
block:
  let txt = "Hello World"

  template `[]`[T](p: ptr T, span: Slice[int]): untyped =
    toOpenArray(cast[ptr array[0, T]](p)[], span.a, span.b)

  doAssert $cast[ptr uint8](txt[0].addr)[0 ..< txt.len] == 
                "[72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100]"


block:
  let txt = "Hello World"

  template `[]`[T](p: ptr T, span: Slice[int]): untyped =
    toOpenArray(cast[ptr array[0, T]](p)[], span.a, span.b)

  doAssert $cast[ptr uint8](txt[0].addr)[0 ..< txt.len] == 
                "[72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100]"

