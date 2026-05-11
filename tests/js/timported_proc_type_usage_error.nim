discard """
  description: '''
    Ensure that types disallowed for incoming or outgoing values at the FFI
    border are detected and rejected.
  '''
  matrix: "--errorMax:0"
  action: reject
"""

proc test(x: int64) {.importjs.} #[tt.Error
         ^ invalid type: 'int64' in this context: 'proc (x: int64)' for proc]#

proc test(x: range[0'i64 .. 1'i64]) {.importjs.} #[tt.Error
         ^ invalid type: 'int64' in this context: 'proc (x: range 0..1(int64))' for proc]#

proc test(x: seq[int64]) {.importjs.} #[tt.Error
         ^ invalid type: 'int64' in this context: 'proc (x: seq[int64])' for proc]#

proc test(x: (bool, int64)) {.importjs.} #[tt.Error
         ^ invalid type: 'int64' in this context: 'proc (x: (bool, int64))' for proc]#

proc testRet(): int64 {.importjs.} #[tt.Error
            ^ invalid type: 'int64' in this context: 'proc (): int64' for proc]#


proc generic[T](x: T) {.importjs.} #[tt.Error
               ^ invalid type: 'int64' in this context: 'proc (x: int64)' for proc]#

generic(1'i64)
