discard """
  targets: "cpp"
  matrix: "--threads:on"
"""

proc threadMain(a: int) {.thread.} =
    discard

proc main() =
    var thread: Thread[int]

    thread.createThread(threadMain, 0)
    thread.joinThreads()

main()
