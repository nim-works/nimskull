import std/[ os, osproc, strutils]
type 
  Certainty = enum
    None,
    Folder,
    Cfg,
    Nimble

proc getProjectFile*(file: string): string =
  result = file
  let (dir, _, _) = result.splitFile()
  var
    path = dir
    certainty = None
  while not path.isRootDir:
    let
      (dir, fname, ext) = path.splitFile()
      current = fname & ext
    if fileExists(path / current.addFileExt(".nim")) and certainty <= Folder:
      result = path / current.addFileExt(".nim")
      certainty = Folder
    if fileExists(path / current.addFileExt(".nim")) and
      (fileExists(path / current.addFileExt(".nim.cfg")) or
      fileExists(path / current.addFileExt(".nims"))) and certainty <= Cfg:
      result = path / current.addFileExt(".nim")
      certainty = Cfg
    if certainty <= Nimble:
      for nimble in walkFiles(path / "*.nimble"):
        let info = execProcess("nimble dump " & nimble)
        var sourceDir, name: string
        for line in info.splitLines:
          if line.startsWith("srcDir"):
            sourceDir = path / line[(1 + line.find '"')..^2]
          if line.startsWith("name"):
            name = line[(1 + line.find '"')..^2]
        let projectFile = sourceDir / (name & ".nim")
        if sourceDir.len != 0 and name.len != 0 and
            file.isRelativeTo(sourceDir) and fileExists(projectFile):
          result = projectFile
          certainty = Nimble
    path = dir

proc rstToMarkdown*(content: string): string =
  var 
    c: string
    isCodeBlock = false
  const BlockStart = ".. code-block::"
  const BlockLen = BlockStart.len + 1
  for line in splitLines(content, true):
    let isCodeBlockStart = line.startsWith(BlockStart)
    if isCodeBlockStart:
      isCodeBlock = true
      if line.endsWith("Nim\n") or line.endsWith("nim\n") or 
         line.len == BlockLen:
        c.add "```nim\n"
      else:
        c.add "```\n"
    elif isCodeBlock and line.strip() == "":
      c.add "```\n"
      isCodeBlock = false
    else:
      c.add line
  if isCodeBlock:
    # single code block and ends without trailing line
    c.add "```\n"
  # admonition labels
  c = multiReplace(c, 
    (".. attention::", "**attention**"),
    (".. caution::", "**caution**"),
    (".. danger::", "**danger**"),
    (".. error::", "**error**"),
    (".. hint::", "**hint**"),
    (".. important::", "**important**"),
    (".. note::", "**note**"),
    (".. seealso::", "**seealso**"),
    (".. tip::", "**tip**"),
    (".. warning::", "**warning**"),
  )