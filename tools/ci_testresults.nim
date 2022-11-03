## Print summary of failed tests for CI

import
  std/[
    os,
    json,
    sets,
    strformat,
    strutils
  ]

const skip = toHashSet([
  "reDisabled",
  "reIgnored",
  "reSuccess",
  "reJoined",
  "reKnownIssue"
  # The test is a known issue that failed to execute, to avoud
  # cluttering the CI output they are skipped.
])

let githubActions = existsEnv"GITHUB_ACTIONS"

func formatResult(j: JsonNode): string =
  ## Turn test result `j` into a message
  fmt"""
Category: {j["category"].getStr()}
Name: {j["name"].getStr()}
Action: {j["action"].getStr()}
Result: {j["result"].getStr()}
-------- Expected -------
{j["expected"].getStr()}
--------- Given  --------
{j["given"].getStr()}
-------------------------
"""

proc writeResult(j: JsonNode) =
  ## Write test result `j` into stdout
  
  if githubActions:
    func escape(s: string): string =
      ## Escape the string `s` so that it can be used in Github Annotations
      # The list is obtained from here:
      # https://github.com/actions/toolkit/blob/e2eeb0a784f4067a75f0c6cd2cc9703f3cbc7744/packages/core/src/command.ts#L80-L85
      s.multiReplace(
        ("%", "%25"),
        ("\r", "%0D"),
        ("\n", "%0A")
      )

    let res = j["result"].getStr()

    let errorPrefix = fmt"""::error file={j["name"].getStr()}::"""

    case res
    of "reNimcCrash":
      # Print a simple header, in case the test failure is not an error that is
      # captured by the problem matcher.
      stdout.writeLine errorPrefix & fmt"Test failed"

      stdout.writeLine:
        # Add the problem matcher
        "::add-matcher::.github/nim-problem-matcher.json\n" &
        # Print the result normally, hoping the matcher will catch the errors
        formatResult(j) & '\n' &
        # Remove the matcher afterwards
        "::remove-matcher owner=nim::"

    else:
      # Concatenate the header and error message then print it.
      #
      # This makes the error result displayed in full on Github.
      stdout.writeLine:
        escape:
          errorPrefix &
          formatResult(j)

  else:
    stdout.writeLine formatResult(j)

when isMainModule:
  for fn in walkFiles("testresults/*.json"):
    let entries = fn.readFile().parseJson()
    for j in entries:
      let res = j["result"].getStr()
      if skip.contains(res):
        continue

      writeResult(j)
