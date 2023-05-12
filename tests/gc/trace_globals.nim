discard """
  output: '''
10000000
10000000
10000000'''
"""

# bug https://github.com/nim-lang/nim/issues/17085

proc init(): string =
  for a in 0..<10000000:
    result.add 'c'

proc f() =
  var a {.global.} = init()
  var b {.global.} = init()
  var c {.global.} = init()

  echo a.len
    # `echo` intentional according to
    # https://github.com/nim-lang/Nim/pull/17469/files/0c9e94cb6b9ebca9da7cb19a063fba7aa409748e#r600016573
  echo b.len
  echo c.len

f()
