discard """
  output: '''true'''
"""

# Just check that we can parse 'somesql' and render it without crashes.

import std/[parsesql, streams, os]

var tree = parseSql(newFileStream(parentDir(currentSourcePath) / "somesql.sql"), "somesql")
discard renderSql(tree)

echo "true"
