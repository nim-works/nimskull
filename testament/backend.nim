#
#
#              The Nim Tester
#        (c) Copyright 2017 Andreas Rumpf
#
#    Look at license.txt for more info.
#    All rights reserved.

import
  std/[
    strutils,
    os,
    osproc,
    json,
    options,
    tables
  ],
  specs, # Basic testament types
  experimental/[
    sexp,
    sexp_diff
  ]

  ReportParams* = object
    ## Contains additional data about report execution state.
    duration*: float ## Test execution duration
    name*: string ## Name of the test
    origName*: string
    cat*: string ## Test category
    action*: TTestAction ## Test action type
    targetStr*: string
    debugInfo*: string
    outCompare*: TOutCompare
    success*: TResultEnum
    knownIssues*: seq[string] ## Whether the test was marked as a 'known
                              ## issue'
    inCurrentBatch*: bool
    expected*: string ## Expected run output
    given*: string ## Given run output


type
  MachineId* = distinct string
  CommitId = distinct string

proc `$`*(id: MachineId): string {.borrow.}

var
  thisMachine: MachineId
  thisCommit: CommitId
  thisBranch: string

proc getMachine*(): MachineId =
  var name = execProcess("hostname").strip
  if name.len == 0:
    name = when defined(posix): getEnv("HOSTNAME")
           else: getEnv("COMPUTERNAME")
  if name.len == 0:
    quit "cannot determine the machine name"

  result = MachineId(name)

proc getCommit(): CommitId =
  const commLen = "commit ".len
  let hash = execProcess("git log -n 1").strip[commLen..commLen+10]
  thisBranch = execProcess("git symbolic-ref --short HEAD").strip
  if hash.len == 0 or thisBranch.len == 0: quit "cannot determine git HEAD"
  result = CommitId(hash)

var
  results: JsonNode
  currentCategory: string

proc writeTestResult*(param: ReportParams) =
  let
    name = param.name
    category = param.cat
    target = param.targetStr
    action = $param.action
    result = $param.success
    expected = param.expected
    given = param.given

  createDir("testresults")
  if currentCategory != category:
    if currentCategory.len > 0:
      var resFile = open("testresults" / category.addFileExt"json", fmWrite)
      resFile.write pretty(results)
      close resFile
    currentCategory = category
    results = newJArray()

  if results.isNil():
    results = newJArray()

  results.add %*{
    "name": name,
    "category": category,
    "target": target,
    "action": action,
    "result": result,
    "expected": expected,
    "given": given,
    "machine": thisMachine.string,
    "commit": thisCommit.string,
    "branch": thisBranch,
    "knownIssues": %param.knownIssues
  }

proc open*() =
  thisMachine = getMachine()
  thisCommit = getCommit()

const testResults = "testresults"

proc close*() =
  if currentCategory.len > 0:
    # To handle `testament cat stdlib/os` testing. There is no reason to
    # ban this because (surprise!) test directory can actually have more
    # than one level of nesting.
    createDir(joinPath(testResults, parentDir(currentCategory)))

    var resFile = open(testResults / currentCategory.addFileExt"json", fmWrite)
    resFile.write pretty(results)
    close resFile

proc cacheResults*() =
  ## Traverses the testresults directory and extracts any failed json entries
  ## and inserts them into a new file within the cacheresults directory.
  createDir(joinPath(testResults, "cacheresults"))
  # We will ignore any entries that have this as their result
  const passResults = [
    "reJoined", "reSuccess", "reDisabled", "reKnownIssue", ""] # "" is defaulted to if result field not found
  let searchPattern = "testresults" / "*.json"
  # Prepare json array which will be written to our new cache file
  var fresults = newJArray()
  # Walk files in testresults that have a json ext
  for f in walkFiles(searchPattern):
    var jdata: JsonNode
    try:
      # Parse the json file into the json node
        jdata = f.parseFile()
    except IOError:
      echo getCurrentExceptionMsg()
      echo "Failed to open/parse the json file in /testresults/. It is likely the file",
            " is corrupted or locked by another process. Try deleting all files in /testresults/ ",
            "and then running testament without '--retry'."
      jdata = newJArray()
    except JsonParsingError:
      echo getCurrentExceptionMsg()
      echo "Failed to parse the cached results. It is likely the file was not ",
            "written to correctly, or was interrupted while doing so. Try ",
            "running testament without '--retry' first."
      jdata = newJArray()
    # The testresults files should be made up of arrays of json objects representing
    # an entry each. Traverse the array and check the result field of each object.
    # If the result is not one of the "passResults", then we add it to our cache
    # node 'fresults'
    for node in jdata:
      let noderesult = node{"result"}.getStr()
      if not passResults.contains(noderesult):
        fresults.add(node)

  var results = open("testresults" / "cacheresults" / "result".addFileExt"json", fmWrite)
  results.write(fresults.pretty())
  close(results)

proc getRetries*(): (seq[string], seq[(string, string)]) =
  ## Returns the stored categories and tuples of test names/targets within the
  ## cache. This is typically combined with the RetryContainer object to set
  ## a global accessible object with the information.
  ## Result = (Categories that had fails, Name and target combination of failed test)
  # Gets the files that failed in the previous run from the cache
  var
    cats: seq[string]
    file_targs: seq[(string, string)]
    jdata: JsonNode
  # Directory of cache
  const cacheDir = "testresults" / "cacheresults"
  createDir(cacheDir) # does nothing if dir already exists
  const cacheFile = cacheDir / "result".addFileExt"json"

  jdata = newJArray()
  try:
    # Parse the json file into the json node
    jdata = cacheFile.parseFile()
  except IOError:
    echo getCurrentExceptionMsg()
    echo(
      "Failed to open/parse the cached results from file '", cacheFile,
      "'. It is likely the file does",
      " not exist yet. Try running testament without '--retry' first.")
  except JsonParsingError:
    echo getCurrentExceptionMsg()
    echo "Failed to parse the cached results. It is likely the file was not ",
          "written to correctly, or was interrupted while doing so. Try ",
          "running testament without '--retry' first."

  let cacheArray = cacheFile.parseFile()
  # Traverse the array of entries, add the category to cats if it has not been seen,
  # either way add the "name" field to file_targs
  for entry in cacheArray:
    let
      cat = entry["category"].getStr()
      file_targ = entry["name"].getStr()
      # Split target from file name. RSplit with maxsplit of 2 is used to
      # avoid corrupting test names of tests with spaces in dir
      name_target = file_targ.rsplit(maxsplit = 2)
    # Do not want duplicates; only add if not already
    if cat notin cats:
      cats.add cat
    file_targs.add:
      if name_target.len > 1: (name_target[0], name_target[1])
      # In the case that rsplit did not separate the name into a target and
      # test name then we still add it
      else: (name_target[0], "")
  result = (cats, file_targs)
