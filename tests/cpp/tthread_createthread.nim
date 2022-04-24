discard """
  targets: "cpp"
  matrix: "--threads:on"
  knownIssue: '''this should work in CPP, see PR:
https://github.com/nim-works/nimskull/pull/290
'''
"""

proc threadMain(a: int) {.thread.} =
    discard

proc main() =
    var thread: Thread[int]

    thread.createThread(threadMain, 0)
    thread.joinThreads()

main()
