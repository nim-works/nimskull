discard """
  targets: c js vm
  description: '''
    Regression test for a C code generator bug where adding to, setting the
    length, or assigning to a ``seq`` would cause a memory access violation
  '''
"""

proc main() =
  # wrap the test in a procedure to prevent `x` being a global from
  # interfering
  const Data: seq[int] = @[]
  var x = (Data,)
  # the code generator lifted the whole tuple into the immutable data
  # section, but since copy hook injection already took place, no
  # copy of the constant was created and the next mutation of the seq
  # would thus try to write into the immutable data section
  x[0].add 1

main()
