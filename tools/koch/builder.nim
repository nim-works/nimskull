#
#
#               Builder library for Nim
#   Copyright (c) 2021 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt" included in this distribution
# for details about the copyright.

import std/[os, osproc, parseopt, sequtils, setutils, tables]

type
  Target* = distinct uint16
    ## A build target

  Executable* = distinct Target
    ## A build target producing a Nim executable

  Run* = distinct Target
    ## A target that runs a command

  Input = uint16
    ## An input

  Operand = uint16
    ## An operand

  Instruction {.pure.} = enum
    ## How to run the builder
    Noop ## Don't do anything

    Build ##\
    ## Build a Nim executable, operand `a` is the source input id
    BuildWithArgs ##\
    ## Build a Nim executable, operand `a` is the source input id, operand `b`
    ## is the number of arguments after the source id

    RunBuilt ##\
    ## Run a binary built by another target, operand `a` is the target id
    RunBuiltWithArgs ##\
    ## Run a binary built by another target with extra arguments, operand `a`
    ## is the target id, `b` is the extra operand id where the entry at `b` is
    ## the starting argument id in input list and the entry at `b + 1` is the
    ## number of arguments

  TargetRecipe = object
    ## The recipe for the target
    instr: Instruction ## The instruction of the target, how operands are
                       ## interpreted depends on this
    a: Operand ## First operand
    b: Operand ## Second operand

  Builder* = object
    ## The builder
    sourceRoot: string ## The root directory of the source code
    buildDir: string ## The directory to store build artifacts

    compiler: string ## The Nim compiler to be used
    defaultCompilerArgs: seq[string] ## Default compiler arguments

    names: seq[string] ## Target to name mapping. Its usage depends on the target itself

    recipes: seq[TargetRecipe] ## Recipe for each target
    dependencies: Table[Target, set[Target]] ## Mapping of target dependencies
    inputs: seq[string] ## List of string inputs

    extraOperands: seq[Operand] ## List of extra operands

const
  ExecutableKinds = {Build .. BuildWithArgs}
  RunKinds = {RunBuilt .. RunBuiltWithArgs}

template defineTargetType(T: typed): untyped =
  when T isnot Target:
    converter toTarget*(id: T): Target {.used.} = Target(id)

  # Distinct base can't be used with Nim 1.0
  converter toBase(id: T): uint16 {.used.} = uint16(id)

  template `[]`[U](s: seq[U], id: T): U {.used.} =
    s[int id]

  template `$`(x: T): string {.used.} =
    $x.int

defineTargetType(Target)
defineTargetType(Executable)
defineTargetType(Run)

proc initBuilder*(sourceRoot = "",
                  buildDir = "build",
                  compiler = "nim",
                  defaultCompilerArgs: openArray[string] = []): Builder =
  ## Creates a new builder
  result.sourceRoot = expandFilename(sourceRoot)
  result.buildDir = expandFilename(buildDir)
  result.compiler = compiler
  result.defaultCompilerArgs = @defaultCompilerArgs

func addDefaultCompilerArgs*(b: var Builder, args: varargs[string]) =
  ## Add `args` to the list of default compiler arguments
  b.defaultCompilerArgs.add args

func setCompiler*(b: var Builder, compiler: string) =
  ## Change the compiler used by builder
  b.compiler = compiler

func atRoot(path, root: string): string =
  ## Rewrite `path` such that `root` is its root directory.
  # To do this, simply turn the path into an absolute path, then normalize it,
  # which will collapse all `..` in the path, then append it to the target
  # root, then normalize that too, to make it looks nicer
  result = normalizedPath(DirSep & path)
  if root.len > 0:
    result = normalizedPath(root / result)
  else:
    # If the root is empty, then convert this into a relative path by stripping
    # the DirSep in front
    result = result[1..^1]

func len(b: Builder): int =
  ## Returns the number of targets in the builder
  b.recipes.len

func addTarget(b: var Builder, name: string, recipe: TargetRecipe,
               dependsOn: set[Target] = {}): Target =
  ## Add a new target with `name` and `recipe` and dependency on `dependsOn`.
  ## Returns the target.
  if b.recipes.len > high(Target).int:
    raise newException(ValueError):
      "Maximum targets capacity (" & $high(Target) & ") reached"

  if name in b.names:
    raise newException(ValueError):
      "Target name `" & name & "' already exists"

  result = Target b.recipes.len
  b.recipes.add recipe
  b.names.add name
  if dependsOn != {}:
    b.dependencies[result] = dependsOn

func addInputs(b: var Builder, inputs: varargs[string]): Input =
  ## Add `inputs` and return the first ID
  if b.inputs.high + inputs.len > high(Input).int:
    raise newException(ValueError):
      "Adding inputs will exceed the maximum capacity of " & $high(Input)

  result = Input b.inputs.len
  b.inputs.add inputs

func addExtraOperands(b: var Builder, operands: varargs[Operand]): Operand =
  ## Add `operands` to the list of extra operands and return the index of the
  ## first operand added as an operand
  if b.extraOperands.high + operands.len > high(Operand).int:
    raise newException(ValueError):
      "Adding operands will exceed the maximum capacity of " & $high(Operand)

  result = Operand b.extraOperands.len
  b.extraOperands.add operands

func topologicalOrder(b: Builder): seq[Target] =
  ## Produce the topological ordering of all targets in the builder
  func pop[T](s: var set[T]): T =
    ## Pop an item from the set
    for item in s.items:
      result = item
      break

    s.excl(result)

  # Don't do anything if there are no targets
  if b.len == 0: return

  var
    starters = {Target(0) .. Target(b.len - 1)}
      ## List of targets without dependencies
    dependencies = b.dependencies
      ## The target dependency table

  # Exclude all targets with dependencies from starters list
  for targetWithDep in dependencies.keys:
    starters.excl targetWithDep

  # Produce a topological ordering using Kahn's algorithm
  while starters != {}:
    let target = starters.pop()
    result.add target

    for targetWithDep, deps in dependencies.mpairs:
      if target in deps:
        deps.excl target

        if deps == {}:
          starters.incl targetWithDep

  # Why is this an assert?
  #
  # The graph is constructed by declaring a node and its dependency at the same
  # time, so it is impossible for a later node to be referenced by a previous
  # one.
  #
  # If this happens, we messed up the code.
  for deps in dependencies.values:
    assert deps == {}:
      "There is a cycle in the dependency graph"

func sortTargets(b: var Builder): seq[Target] =
  ## Perform a topological sort on builders' targets.
  let sortedOrder = b.topologicalOrder

  var
    newNames: typeof(b.names)
    newRecipes: typeof(b.recipes)
    newDependencies: typeof(b.dependencies)

  for idx, target in sortedOrder.pairs:
    # Insert to new target seq according to the sorted order
    newRecipes.add b.recipes[target]
    newNames.add b.names[target]

    # If the target has any dependencies
    let deps = b.dependencies.getOrDefault(target)
    if deps != {}:
      for newDep in Target(0) ..< target:
        let dep = sortedOrder[newDep]
        # If dep was a dependency of the target
        if dep in deps:
          # Re-link it to the new ID
          newDependencies[target].incl newDep

  b.names = newNames
  b.recipes = newRecipes
  b.dependencies = newDependencies

func name*(b: Builder, target: Target): string =
  ## Returns the name of a given target
  b.names[target]

func addExecutable*(b: var Builder, name, src: string,
                    extraFlags: varargs[string]): Executable =
  ## Add a target for building a Nim executable. `name` is the name of the executable (sans extension).
  let name =
    if name.len == 0:
      src.changeFileExt(ExeExt)
    else:
      name

  # Construct a recipe for the executable
  var recipe: TargetRecipe
  recipe.instr = Build
  recipe.a = Operand b.addInputs(src.atRoot(b.sourceRoot))

  if extraFlags.len > 0:
    recipe.instr = BuildWithArgs
    # Append the parameters right after the input
    discard b.addInputs(extraFlags)
    # Record the length of the parameters
    recipe.b = Operand extraFlags.len

  result = Executable b.addTarget(name, recipe)

func builtExe*(b: Builder, exe: Executable): string =
  ## Returns the path to the built executable
  # TODO: Move this to the build directory
  b.sourceRoot / "bin" / b.name(exe).addFileExt(ExeExt)

func cacheDirectory(b: Builder, exe: Executable): string =
  ## Returns the path to the nimcache of the executable
  b.buildDir / "nimcache" / b.name(exe)

func addRun*(b: var Builder, name: string, exe: Executable,
             args: openArray[string], dependsOn: set[Target]): Run =
  ## Add a target for running a built executable
  if exe.int >= b.len:
    raise newException(ValueError):
      "The target #" & $exe & " is not a valid target"

  if b.recipes[exe].instr notin ExecutableKinds:
    raise newException(ValueError):
      "The target #" & $exe & " is not an executable target"

  var recipe = TargetRecipe(
    instr: RunBuilt,
    a: Operand exe
  )
  if args.len > 0:
    recipe.instr = RunBuiltWithArgs
    recipe.b = b.addExtraOperands(Operand b.addInputs(args), Operand args.len)

  result = Run:
    b.addTarget(name, recipe, dependsOn = {Target exe} + dependsOn)

func addRun*(b: var Builder, name: string, exe: Executable,
             args: varargs[string]): Run {.inline.} =
  ## Add a target for running a built executable
  b.addRun(name, exe, args, {})

func addAlias*(b: var Builder, name: string, targets: varargs[Target]): Target =
  ## Add a target for executing other targets
  if targets.len == 0:
    raise newException(ValueError):
      "An alias must have at least one target"
  let recipe = TargetRecipe(instr: Noop)
  b.addTarget(name, recipe, targets.toSet())

func buildCmd*(b: Builder, target: Target): seq[string] =
  ## Produce the command to build the target
  template recipe: untyped = b.recipes[target]

  case recipe.instr
  of Noop:
    discard "There is nothing to be run"
  of Build, BuildWithArgs:
    result = @[b.compiler] & b.defaultCompilerArgs
    result.add:
      "-o:" & b.builtExe(target.Executable)
    result.add:
      "--nimcache:" & b.cacheDirectory(target.Executable)

    if recipe.instr in {BuildWithArgs}:
      # Add extra arguments to the command
      result.add:
        b.inputs.toOpenArray(int(recipe.a + 1), int(recipe.a + recipe.b))

    # Add build command
    result.add "c"

    # Add the source file
    result.add:
      b.inputs[b.recipes[target].a]
  of RunBuilt, RunBuiltWithArgs:
    result = @[b.builtExe(b.recipes[target].a.Executable)]

    if b.recipes[target].instr in {RunBuiltWithArgs}:
      let
        argsIdx = b.extraOperands[recipe.b]
        argsLen = b.extraOperands[recipe.b + 1]

      result.add:
        b.inputs.toOpenArray(int(argsIdx), int(argsIdx + argsLen - 1))

func targetMask(b: Builder, target: Target): set[Target] =
  ## Return the mask containing all targets to be run to satisfy `target`
  result = b.dependencies.getOrDefault(target)

  for target in Target(0) ..< Target(b.len - 1):
    if target in result:
      result.incl b.targetMask(target)

  result.incl target

iterator targets*(b: Builder): tuple[target: Target, name: string] =
  for target, name in b.names.pairs:
    yield (Target(target), name)

proc build*(bld: Builder, target: Target) =
  ## Build the target `target`
  let
    targetMask = bld.targetMask(target)
    orderedTargets = bld.topologicalOrder().filterIt(it in targetMask)

  var completedMask: set[Target]

  while targetMask - completedMask != {}:
    let 
      runnable = orderedTargets.filterIt:
        # Select tasks that are not completed and have no unfulfilled dependency
        it notin completedMask and bld.dependencies.getOrDefault(it) - completedMask == {}

      # Turn list of runnables into list of commands, then turn that list into list of running processes.
      #
      # This effectively run everything in parallel.
      runProcs = runnable.mapIt(bld.buildCmd(it)).mapIt:
        if it.len > 1:
          startProcess(
            command = it[0],
            args = it.toOpenArray(1, it.len - 1),
            options = {poParentStreams, poUsePath, poEchoCmd}
          )
        elif it.len > 0:
          startProcess(
            command = it[0],
            options = {poParentStreams, poUsePath, poEchoCmd}
          )
        else:
          Process(nil)

    # Collect exit codes of started processes
    var failed = newSeqOfCap[Target](runnable.len)
    for idx, p in runProcs.pairs:
      if not p.isNil:
        let
          target = runnable[idx]
          exitCode = p.waitForExit()

        if exitCode != 0:
          failed.add target
        else:
          completedMask.incl target

      else:
        # If a target spawn no processes, mark it as completed
        completedMask.incl target

    if failed.len > 0:
      raise newException(CatchableError):
        "Build failed for targets: " & $failed.mapIt(bld.names[it])

func fromName*[T](b: Builder, name: string): T =
  ## Retrieve target `T` with `name`
  let id = find(b.names, name)
  if id == -1:
    raise newException(ValueError):
      "Target `" & name & "' is not a part of the builder"

  when T is Executable:
    if b.recipes[id].instr notin ExecutableKinds:
      raise newException(ValueError):
        "Target `" & name & "' is not an executable target"
  elif T is Run:
    if b.recipes[id].instr notin RunKinds:
      raise newException(ValueError):
        "Target `" & name & "' is not a run target"
  elif T isnot Target:
    static:
      doAssert false, "fromName can only be used to retrieve Target-derived types"

  result = T id

proc build*(b: Builder, target: string) =
  ## Build the target `target`
  b.build(b.fromName[:Target](target))

func contains*(b: Builder, target: string): bool =
  ## Check if builder contains `target`
  target in b.names


