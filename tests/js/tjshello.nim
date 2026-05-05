discard """
  cmd: "nim $target $options --stackTrace:off --lineTrace:off $file"
  output: "Hello World"
  maxcodesize: 1000
  ccodecheck: "!@'function'"
"""

import std/jsconsole

console.log "Hello World"
