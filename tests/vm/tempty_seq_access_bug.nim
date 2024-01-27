discard """
  description: '''
    Regression test for an internal issue with the VM's pointer-to-cell
    mapping that allowed
  '''
  targets: vm
  matrix: "--boundChecks:off"
  outputsub: "trying to access a location outside of the VM's memory"
  exitcode: 1
"""

# this test is written against internal implementation details of the VM as it
# worked when this test was written. The current VM could differ.

proc test() =
  # test in a procedure so that the locations have local lifetime

  # allocate six memory cells, 3 for the seqs, 3 for their payloads:
  var
    a = newSeq[int](1)
    b = newSeq[int](1)
    c = default(seq[int]) # empty seq

  # free two cells (by overwriting the seqs with empty ones):
  a.newSeq(0)
  b.newSeq(0)

  proc write(i: var int) =
    # when actually writing to the location, a proper access violation needs
    # to be reported
    i = 1

  # bound checks are disabled, so no index error is raised
  write(c[0])

test()