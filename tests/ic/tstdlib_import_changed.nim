discard """
  output: '''yes'''
"""

echo "yes"

#!EDIT!#

discard """
  output: '''yes2'''
  knownIssue: '''
    The compiler crashes, at the time of writing, on reporting an error within
    a `compiles` context.
  '''
"""

import std / [monotimes]
#discard getMonoTime()
echo "yes2"
