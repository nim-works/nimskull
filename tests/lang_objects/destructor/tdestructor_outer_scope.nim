discard """
  matrix: "--gc:orc; --gc:arc"
  output: '''
()
Destroyed
()
Destroyed
()
Destroyed
end
-------------------------
()
Destroyed
end
'''
description: '''
  . From https://github.com/nim-lang/Nim/issues/9440
    Destructors are always injected in the outermost scope
  . This is pretty bad because we can't implement the RAII pattern in a safe way.
  . The following example shows a seemingly correct program that only calls the
    destructor on the last-created instance of the X object.
'''
"""

# bug #9440
block:
  type
    X = object

  proc `=destroy`(x: var X) =
    echo "Destroyed"

  proc main() =
    for x in 0 .. 2:
      var obj = X()
      echo obj
    # The destructor call is invoked after "end" is printed
    echo "end"

  main()

echo "-------------------------"

block:
  type
    X = object

  proc `=destroy`(x: var X) =
    echo "Destroyed"

  proc main() =
    block:
      var obj = X()
      echo obj
      # The destructor is not called when obj goes out of scope
    echo "end"

  main()