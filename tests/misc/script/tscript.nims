# executed via tests/misc/trunner.nim (TODO: make testament support scripting)

mode = ScriptMode.Whatif

exec "gcc -v"

# bug #6327
doAssert(existsEnv("dummy") == false)

# issue #7283
putEnv("dummy", "myval")
doAssert(existsEnv("dummy"))
doAssert(getEnv("dummy") == "myval")
delEnv("dummy")
doAssert(existsEnv("dummy") == false)

# issue #7393
let wd = getCurrentDir()
cd("..")
doAssert wd != getCurrentDir()
cd(wd)
doAssert wd == getCurrentDir()

when false:
  # this doesn't work in a 'koch testintall' environment
  doAssert findExe("nim") != ""

# general tests
mode = ScriptMode.Verbose

doAssert cmpic("HeLLO", "hello") == 0

doAssert fileExists("tests/misc/script/tscript.nims") == true
doAssert dirExists("tests") == true

doAssert fileExists("tests/misc/script/tscript.nims") == true
doAssert dirExists("tests") == true

discard selfExe()

when defined(windows):
  doAssert toExe("nim") == "nim.exe"
  doAssert toDll("nim") == "nim.dll"
else:
  doAssert toExe("nim") == "nim"
  doAssert toDll("nim") == "libnim.so"

rmDir("tempXYZ")
doAssertRaises(OSError):
  rmDir("tempXYZ", checkDir = true)
doAssert dirExists("tempXYZ") == false
mkDir("tempXYZ")
doAssert dirExists("tempXYZ") == true
doAssert fileExists("tempXYZ/koch.nim") == false

when false:
  # this doesn't work in a 'koch testintall' environment
  cpFile("koch.nim", "tempXYZ/koch.nim")
  doAssert fileExists("tempXYZ/koch.nim") == true
  cpDir("nimsuggest", "tempXYZ/.")
  doAssert dirExists("tempXYZ/tests") == true
  doAssert fileExists("tempXYZ/nimsuggest.nim") == true
  rmFile("tempXYZ/koch.nim")
  doAssert fileExists("tempXYZ/koch.nim") == false

rmDir("tempXYZ")
doAssert dirExists("tempXYZ") == false

import std/strtabs
block ensure_strtab_interpolation_works_in_scripting_env:
  # the strtab module relied on `os.getEnv` explicitly, now it's no longer
  # fully qualified, so it'll pick-up `getEnv` the script VM instrinsic
  # xxx: make an `os` module _for_ scripting so things just work?
  static:
    let t = {"name": "John", "city": "Monaco"}.newStringTable
    doAssert "${name} lives in ${city}" % t == "John lives in Monaco"
  let t = {"name": "John", "city": "Monaco"}.newStringTable
  doAssert "${name} lives in ${city}" % t == "John lives in Monaco"
