discard """
  output: "(allocCount: 201, deallocCount: 201)"
  matrix: "--gc:orc -d:nimAllocStats"
"""

proc main(prefix: string) =
  var c: seq[string]
  for i in 0..<100:
    newSeq(c, 100)
    c[i] = prefix & $i

main("abc")
echo getAllocStats()
