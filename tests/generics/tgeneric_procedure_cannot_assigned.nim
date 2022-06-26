discard """
  errormsg: "cannot instantiate: \'T\'"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/7141
      3 lines to break the compiler: proc [T], assignment and SIGSEGV
    . This should not break the compiler, but of course the line
      `var fun = if true: foo else: foo`  cannot compile.
      The reason is that foo is not a concrete procedure,
      hence it cannot be assigned to a variable as a value.
      Generic procedures are a recipe that will be later instantiated to
      a concrete procedure when a type is passed. If the procedure is called
      two times with two different type arguments, there will be two different
      versions in the generated code.
      If the procedure is never called, it will not even appear in the
      generated code at all.
      When you assign a procedure to a variable, you essentially store a pointer
      to where the code of the procedure lives.
      You cannot do this for a generic procedure, because it does not correspond
      to a section in the generated code like a concrete one does.
'''
"""

proc foo[T](x: T) =
  discard

var fun = if true: foo else: foo

