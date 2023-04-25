discard """
  cmd: "nim c --compileOnly --os:standalone --exceptions:goto -d:noSignalHandler -d:danger $file"
  action: "compile"
"""

echo "hi"
