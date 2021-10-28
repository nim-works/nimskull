## Print summary of failed tests for CI

import os, json, sets, strformat, strutils

const skip = toHashSet(["reDisabled", "reIgnored", "reSuccess", "reJoined"])

when isMainModule:
  let githubActions = existsEnv"GITHUB_ACTIONS"

  for fn in walkFiles("testresults/*.json"):
    let entries = fn.readFile().parseJson()
    for j in entries:
      let res = j["result"].getStr()
      if skip.contains(res):
        continue

      var msg = ""
      if githubActions:
        # Add error prefix so it shows up as annotations
        msg.add fmt"""::error file={j["name"].getStr()}::"""

      msg.add fmt"""
Category: {j["category"].getStr()}
Name: {j["name"].getStr()}
Action: {j["action"].getStr()}
Result: {res}
-------- Expected -------
{j["expected"].getStr()}
--------- Given  --------
{j["given"].getStr()}
-------------------------
"""

      if githubActions:
        # Escape some characters so that Github Actions can read the full message
        #
        # The list is obtained from here: https://github.com/actions/toolkit/blob/e2eeb0a784f4067a75f0c6cd2cc9703f3cbc7744/packages/core/src/command.ts#L80-L85
        msg = msg.multiReplace(
          ("%", "%25"),
          ("\r", "%0D"),
          ("\n", "%0A")
        )

      # Print the error
      echo msg
