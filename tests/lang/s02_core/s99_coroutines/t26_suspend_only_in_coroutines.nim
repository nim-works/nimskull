## The ``suspend`` routine can only be called within the immediate context of
## a coroutine.

suspend(nil) #[tt.Error
      ^ 'suspend' is only allowed within a coroutine]#

proc coro() {.coroutine.} =
  proc inner() =
    # not in the immediate context of a coroutine
    suspend(nil) #[tt.Error
           ^ 'suspend' is only allowed within a coroutine]#
