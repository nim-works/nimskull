#
#     Copyright (c) 2022 Leorize <leorize+oss@disroot.org>
#
#        See the file "copying.txt", included in this
#        distribution, for details about the copyright.
#

## A tool to build and manage a release manifest for the compiler.
##
## Designed for use in CI, thus not having many interactive components.

import std/[json, options, os, parseopt, strutils, tables]

type
  Database = object
    ## The artifact database
    file: seq[string] ## The artifacts, which are unique
    sha256: seq[string] ## The SHA256 checksum of the artifacts
    triplet: seq[string] ## The target triplet of the artifacts
    version: string ## The version of all artifacts

  ArtifactDataV0 = object
    ## Data about an artifact in the manifest
    name: string ## The file name of the artifact
    sha256: string ## The SHA256 checksum of the artifact

  BinaryArtifactDataV0 = object
    ## Data about a binary artifact in the manifest
    target: string ## The target of the artifact
    name: string ## The file name of the artifact
    sha256: string ## The SHA256 checksum of the artifact

  ReleaseManifestV0 = object
    ## The manifest attached to a release. It is optimized for tools to quickly
    ## discover the artifact they need for their target.
    ##
    ## This structure is meant to be converted into a JSON document. Once in
    ## use, it must not have any change that will affect the JSON schema.
    manifestVersion: int ## The version of the manifest
    version: string ## The version being released
    source: ArtifactDataV0 ## The source archive for this release
    binaries: seq[BinaryArtifactDataV0] ## A list of binary artifacts each have
                                        ## an unique target

  ReleaseManifest = ReleaseManifestV0
    ## The release manifest in use.

const
  SourceTriplet = ""
    ## The triplet used for source archive
  Sha256StrLength = 64
    ## The length of a sha256 string
  ManifestVersion = 0
    ## The version of the published manifest

func addArtifact(d: var Database, file, sha256, version, triplet: string) =
  ## Add an artifact to the database
  ##
  ## :file:
  ##   The file to be added
  ##
  ## :version:
  ##   The version of the artifact
  ##
  ## :triplet:
  ##   The triplet it is targeting
  if file.len == 0:
    raise newException(ValueError):
      "The file name must not be empty"

  if version.len == 0:
    raise newException(ValueError):
      "The version must not be empty"

  if file in d.file:
    raise newException(ValueError):
      "Artifact already exist in the database: " & file

  if sha256.len != Sha256StrLength:
    raise newException(ValueError):
      "Invalid length of (" & $sha256.len & ") for sha256 checksum: " & sha256

  if triplet in d.triplet:
    let id = d.triplet.find(triplet)
    raise newException(ValueError):
      if triplet != SourceTriplet:
        "Could not add artifact (" & file & "): artifact for " & triplet &
        " already exist: " & d.file[id]
      else:
        "Could not add artifact (" & file & "):" &
        " a source archive has already been added: " & d.file[id]

  # If no version is in the database (empty database)
  if d.version.len == 0:
    # Set the version to the artifact's version
    d.version = version
  # If there is a version and it doesn't match the artifact's
  elif d.version != version:
    raise newException(ValueError):
      "The artifact to be added is of a different version " & version &
      " from the database version " & d.version

  d.file.add file
  d.sha256.add sha256
  d.triplet.add triplet

func serialize(d: Database): JsonNode =
  ## Convert the database into a manifest to be used by tools.
  ##
  ## For the schema of the resulting JSON document, refer to the
  ## `ReleaseManifest` structure.
  var manifest = ReleaseManifest(
    manifestVersion: ManifestVersion,
    version: d.version
  )

  for i in 0 ..< d.file.len:
    if d.triplet[i] == SourceTriplet:
      manifest.source = ArtifactDataV0(
        name: d.file[i],
        sha256: d.sha256[i]
      )
    else:
      manifest.binaries.add:
        BinaryArtifactDataV0(
          target: d.triplet[i],
          name: d.file[i],
          sha256: d.sha256[i]
        )

  result = %manifest

func deserialize(j: JsonNode): Database =
  ## Convert the manifest for tools into a database.
  if j["manifestVersion"].getInt(-1) != 0:
    raise newException(ValueError):
      "Unknown manifest version: " & $j["manifestVersion"].getInt(-1)

  # Currently only manifest version 0 is supported.
  let manifest = j.to(ReleaseManifestV0)

  result.version = manifest.version
  # Add the source artifact if available
  if manifest.source.name.len > 0:
    result.addArtifact(
      manifest.source.name, manifest.source.sha256, manifest.version,
      SourceTriplet
    )

  # Add all artifacts
  for artifact in manifest.binaries.items:
    result.addArtifact(
      artifact.name, artifact.sha256, manifest.version, artifact.target
    )

func toTriplet(os, cpu: string): string =
  ## Convert Nim's os/cpu pair into a LLVM-style target triplet.
  ##
  ## See https://clang.llvm.org/docs/CrossCompilation.html#target-triple
  ## for the general format and
  ## https://github.com/llvm/llvm-project/blob/890beda4e1794f8b5cf13d3fcd158c37b65c684e/llvm/lib/Support/Triple.cpp
  ## for the strings used in each field of the triplet.
  ##
  ## The triplets used are the shortest possible to describe a target. This
  ## means only the architecture, operating system and environment fields are
  ## used unless an another field is required for further disambiguation and
  ## established names are preferred (ie. `darwin` preferred over `macosx`,
  ## `apple` vendor is used to separate macOS from other non-Apple
  ## Darwin-based OS).
  ##
  ## A lot of the definitions are based on what is built by CI and by no means
  ## exhaustive.
  result =
    case cpu
    of "amd64":
      "x86_64"
    of "i386":
      "i386"
    of "arm64":
      "aarch64"
    of "arm":
      "armv7a" # CI built compiler for ARMv7-A cores
    else:
      raise newException(ValueError):
        "Unsupported cpu: " & cpu

  result.add:
    case os
    of "linux":
      case cpu
      of "arm":
        # CI built ARM compiler for glibc/hard-float
        "-linux-gnueabihf"
      else:
        # CI built compiler for glibc
        "-linux-gnu"
    of "android":
      case cpu
      of "arm":
        "-linux-androidhf"
      else:
        "-linux-android"
    of "macosx":
      "-apple-darwin"
    of "windows":
      "-windows-gnu" # CI built compiler for MinGW
    else:
      raise newException(ValueError):
        "Unsupported os: " & os

func addArtifact(d: var Database, manifest: JsonNode) =
  ## Add the artifact described by niminst's `manifest` into the database.
  let
    version = manifest["version"].getStr
    file = manifest["name"].getStr
    triplet =
      if "os" in manifest and "cpu" in manifest:
        toTriplet(manifest["os"].getStr, manifest["cpu"].getStr)
      else:
        SourceTriplet
    sha256 = manifest["sha256"].getStr

  d.addArtifact(file, sha256, version, triplet)

# -- CLI actions start here

type
  OutputFormat {.pure.} = enum
    ## Output format for files-to-upload
    Text = "text"
    GithubActions = "github-actions"

proc addCommand(manifest: string, archiveData: varargs[string]) =
  ## Implementation for the `add` subcommand.
  ##
  ## :manifest:
  ##   The filename of the release manifest to modify.
  ##
  ## :archiveData:
  ##   A list of niminst-generated archive manifest.
  var database =
    try:
      # Deserialize the manifest back to database
      json.parseFile(manifest).deserialize()
    except IOError:
      # If the manifest doesn't exist, create a new one
      stderr.writeLine("warning: creating a new manifest")
      Database()

  # Parse each archive data and add the corresponding artifact
  for archiveData in archiveData.items:
    database.addArtifact:
      json.parseFile(archiveData)

  # Serialize a new manifest
  writeFile(manifest, $database.serialize())

func escapeDataForGithubActions(s: string): string =
  ## Escape the string `s` so that it can be used as data for workflow commands.
  # The list is obtained from here:
  # https://github.com/actions/toolkit/blob/e2eeb0a784f4067a75f0c6cd2cc9703f3cbc7744/packages/core/src/command.ts#L80-L85
  s.multiReplace {
    "%": "%25",
    "\r": "%0D",
    "\n": "%0A"
  }

proc filesToUploadCommand(manifest: string, format = Text) =
  ## Implementation for the `files-to-upload` subcommand.
  ##
  ## :manifest:
  ##   The filename of the release manifest to obtain data from.
  ##
  ## :format:
  ##   The format of the output data.
  # Deserialize the manifest back to database
  let database = json.parseFile(manifest).deserialize()

  var output: string

  let
    manifestAbsolute = manifest.expandFileName()
    storageFolder = manifestAbsolute.parentDir()
  # Output the full path of the manifest itself
  output.add(manifestAbsolute)

  # Output all artifacts in the database, made absolute using the storage folder
  for file in database.file.items:
    output.add('\n')
    output.add(storageFolder / file)

  case format
  of Text:
    stdout.writeLine(output)
  of GithubActions:
    stdout.write:
      escapeDataForGithubActions(output)

proc versionCommand(manifest: string) =
  ## Implementation for the `version` subcommand.
  ##
  ## :manifest:
  ##   The filename of the release manifest to obtain data from.
  let database = json.parseFile(manifest).deserialize()

  stdout.writeLine(database.version)

# -- CLI dispatching stuff starts here

type
  Action {.pure.} = enum
    ## The action to be taken
    Unknown
    Help = "help"
    Add = "add"
    FilesToUpload = "files-to-upload"
    Version = "version"

  Flag {.pure.} = enum
    ## Flags passed via CLI
    Error ## Not a valid flag. This is used to store invalid flag from command line.
    Help = "help" ## -h, --help
    File = "file" ## -f, --file
    Format = "format" ## --format

  CliErrorKind {.pure.} = enum
    ## Errors during CLI parsing
    NoError ## No error occurred
    InvalidFlag = "Invalid flag `$1'" ##
      ## An invalid flag was passed, flags[Error] contain the flag
    InvalidCommand = "Invalid command `$1'" ##
      ## An invalid command was passed, args[0] contain the command
    TerminatorBeforeCommand = "No command found before `--'" ##
      ## A terminator stopped command parsing before a command was found
    FlagNeedValue = "A value must be given to flag `$1'"
      ## A flag that requires a value was passed without one, flags[Error]
      ## contain the flag

  CliInterpErrorKind {.pure.} = enum
    ## Errors during CLI interpretation. This is a format string storage for
    ## the most part.
    FlagInvalidValue = "`$1' is not a valid value for flag `$2'"
      ## An invalid value was passed to a flag.

  Cli = object
    flags: Table[Flag, string] ## Table of flags passed and their value
    args: seq[string] ## The non-flag arguments
    error: CliErrorKind ## The error found during argument parsing
    action: Action ## The action to be taken

const
  GlobalOptHelp = """
Global options:
  -h, --help                      Print help for any subcommand
  -f=<file.json>,                 Specify the manifest to be used.
  --file=<file.json>              Defaults to manifest.json in the
                                  current directory.
"""

  MainHelp = """
Usage: $app <command> [args]...

Commands:
  add              Add artifacts to the manifest
  files-to-upload  List the files to be uploaded
  version          Print the release version
  help             Display help for any subcommand

$globalOpt
"""

  AddHelp = """
Usage: $app add [options] [--] <archive.json>...

Add one or more artifacts described by niminst-generated archive.json. The
release manifest will be created if necessary. The added artifact must have the
same version as described in the release manifest.

$globalOpt
"""

  FilesToUploadHelp = """
Usage: $app files-to-upload [options]

Print out files to be uploaded to a Github Release, separated by a newline each.
An error will be raised if no release manifest is found.

Options:
  --format:<text|github-actions>  Specify the output format to be used.
                                  The text format print files separated by
                                  newline.
                                  The github-actions format encodes the text
                                  format such that it can be used in workflow
                                  commands (ie. set-output) without losing data.

$globalOpt
"""

  HelpHelp = """
Usage: $app help [options] [subcommand]

Print help text for the given subcommand, or the main help if no command nor
options were given.

$globalOpt
"""

  VersionHelp = """
Usage: $app version [options]

Print the release version in the release manifest. An error will be raised if
no release manifest is found.

$globalOpt
"""

  DefaultManifestFile = "manifest.json"
    ## The default manifest to operate on

proc printHelp(action: Action) =
  ## Print help message for `action`.
  let defaultHelpFormat = [
    "app", getAppFilename().lastPathPart(),
    "globalOpt", GlobalOptHelp
  ]
  case action
  of Unknown:
    stdout.write(MainHelp % defaultHelpFormat)
  of Action.Help:
    stdout.write(HelpHelp % defaultHelpFormat)
  of Add:
    stdout.write(AddHelp % defaultHelpFormat)
  of FilesToUpload:
    stdout.write(FilesToUploadHelp % defaultHelpFormat)
  of Version:
    stdout.write(VersionHelp % defaultHelpFormat)

proc dispatch(cli: Cli): int =
  ## Dispatches based on `cli`. Returns the exitcode.
  case cli.error
  of InvalidFlag, FlagNeedValue:
    stderr.writeLine("error: ", $cli.error % cli.flags[Error])
    printHelp(cli.action)
    result = 1
  of InvalidCommand:
    stderr.writeLine("error: ", $InvalidCommand % cli.args[0])
    printHelp(Unknown)
    result = 1
  of TerminatorBeforeCommand:
    stderr.writeLine("error: ", $TerminatorBeforeCommand)
    printHelp(Unknown)
    result = 1
  of NoError:
    # If help was requested
    if Flag.Help in cli.flags:
      # Print help for action
      printHelp(cli.action)

    # Otherwise handle the actions
    else:
      case cli.action
      of Unknown:
        # No action was specified, print main help then set failure
        printHelp(Unknown)
        result = 1
      of Action.Help:
        # If there are no command, print main help
        if cli.args.len == 0:
          printHelp(Unknown)
        else:
          let helpSubcommand = parseEnum[Action](cli.args[0], Unknown)
          # If the subcommand is invalid, print main help then set failure
          if helpSubcommand == Unknown:
            stderr.writeLine("error: ", $InvalidCommand % cli.args[0])
            printHelp(Unknown)
            result = 1
          else:
            printHelp(helpSubcommand)
      of Add:
        let manifest = cli.flags.getOrDefault(Flag.File, DefaultManifestFile)
        if cli.args.len > 0:
          addCommand(manifest, cli.args)
        else:
          # No files was given, print the help text and set failure.
          printHelp(cli.action)
          result = 1
      of FilesToUpload:
        let
          manifest = cli.flags.getOrDefault(Flag.File, DefaultManifestFile)
          format =
            try:
              if Flag.Format in cli.flags:
                some parseEnum[OutputFormat](cli.flags[Flag.Format])
              else:
                some Text
            except ValueError:
              none OutputFormat

        # If the format is invalid
        if format.isNone:
          # Print error message and help then set failure
          stderr.writeLine("error: ", $FlagInvalidValue % [cli.flags[Flag.Format], $Flag.Format])
          printHelp(FilesToUpload)
          result = 1
        else:
          filesToUploadCommand(manifest, format.get)
      of Version:
        let manifest = cli.flags.getOrDefault(Flag.File, DefaultManifestFile)
        versionCommand(manifest)

proc main() =
  ## The CLI entrypoint and parser
  var
    cliParser = initOptParser(
      shortNoVal = {'h'},
      longNoVal = @["--help"],
      allowWhitespaceAfterColon = false
    )

    cli: Cli

  for kind, key, val in cliParser.getopt():
    case kind
    of cmdArgument:
      # If no action have been specified
      if cli.action == Unknown:
        cli.action = parseEnum[Action](key, Unknown)
        # If the action was invalid
        if cli.action == Unknown:
          # Stop parsing here, this is an invalid token
          cli.error = InvalidCommand
          cli.args = @[key]
          break

      # Otherwise collect the arguments
      else:
        cli.args.add key
    of cmdLongOption, cmdShortOption:
      case key
      of "help", "h":
        cli.flags[Flag.Help] = ""
      of "file", "f":
        if val.len == 0:
          cli.error = FlagNeedValue
          cli.flags[Flag.Error] = key
          break

        cli.flags[Flag.File] = val
      of "format":
        if cli.action notin {FilesToUpload}:
          cli.error = InvalidFlag
          cli.flags[Flag.Error] = key
          break

        if val.len == 0:
          cli.error = FlagNeedValue
          cli.flags[Flag.Error] = key
          break

        cli.flags[Flag.Format] = val
      of "":
        if cli.action == Unknown:
          cli.error = TerminatorBeforeCommand

        cli.args.add cliParser.remainingArgs
        break
      else:
        cli.error = InvalidFlag
        cli.flags[Flag.Error] = key
        break
    of cmdEnd:
      discard "nothing to do here"

  quit cli.dispatch()

when isMainModule: main()
