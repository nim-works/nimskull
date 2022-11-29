
All of the IR related work is currently located in the `vm` directory, since the IR originally started out as an IR for only the VM.

Things to consider when reading the code:
* more or less everything is still a work in progress
* there's a lot of debug code lying around
* there's lots of outdated and stale code
* most outdated or unfinished parts aren't marked as such
* most of the existing documentation is unfinished or outdated
* some comments (general ones, `XXX` and `TODO`) are outdated and don't apply anymore

The general approach for the new compiler backend is the following:
* take the `PNode`-AST that comes out of `transf` and translate it into a dedicated intermediate representation used until code generation
* apply transformations not specific to a target to the IR
* apply target-specific transformations to the IR
* pass the IR to the target's code-generator

Some future directions and missing pieces are also documented here, but not all of them.

## Overview of the added modules

### `vmir.nim`

The module still has it's original name, but the IR is fully decoupled from the VM. A lot of old code from previous iterations is in here.

The relevant parts are:
* `IrNode3`: the type of the IR's node structure. Currently a variant object, but planned to move to a more generalized representation.
* `IrStore3`: stores the nodes together with the extra data needed (referenced symbols, join target, etc.). Generally refered to as **the** IR. A simple mechanism for tracking where in the compiler each `IrNode3` was added (or modified). This currently just uses stack-traces, but is planned to be expanded into a more proper facility to track node modifications.
* `BuiltinCall`: meant as an extension to magics, except that they're only needed in the backend. Introduced in order to not require changes to the `TMagic` enum.
* `IrCursor`: API to modify the IR. It first records all modifications and then applies them. Not very efficient right now. This might replace the older `irXXX`-procedure based IR generation (I'm not sure yet)

Other parts:
* `genCodeV3Exec`: a very early prototype of a code-generator for the VM, taking the IR as input and producing VM bytecode
* `IrStore`, `IrNode2`: old attempts at the IR. Still kept around since there are some parts that might be reused
* `computeInlining`: an early, non-working prototype of computing the most memory efficient IR generation order in the context of procedure inlining

#### IR overview:
* the IR is a linear node-based representation
* nodes reference each other via indices. A node can only reference nodes coming before it; reference cycles are forbidden
* it's still undecided if a node may be referenced multiple times
* control-flow is represented via gotos and joins. Instead of storing the index of the corresponding `join` target, a `goto` stores an index (`JoinPoint`) into a list storing the actual IR indices. This is aimed at making IR modification simpler, by removing the need to patch goto targets in the IR directly.
* there exist a few special experimental gotos (goto-link-back, goto-with-continuation, goto-active-continuation) meant for more efficient `finally` handling
* the implementation is currently not very data-oriented, but the plan is to move it there eventually.


### `irgen.nim`

A heavily modified copy of `vmgen.nim`. Takes a post-`transf` procedure's `PNode`-AST as the input and translates it to the IR. Traverses the input AST and emits IR instructions via the various `irXXX` procedures.

`irXXX` procedures defined in `irgen` are experimental and don't have a corresponding IR instruction (yet), but use a combination of other IR instructions.

### `irpasses.nim`

Implements the existing IR transformations. This currently includes general transformations as well as target specific ones. The target-specific passes will be moved to their own modules later on.  Most transformations are exposed via the `LinearPass` interface.

None of existing passes are finished yet.

`runV2` implements the base for static-control-flow-based analysis (e.g. alias analysis, last-read-write analysis, escape analysis, etc.).

An overview of the current passes:

#### `computeAliases`

Old, unused, and defunct early prototype of the alias analysis.

#### `computeDestructors`

Old, unused, and defunct early prototype of the destructor injection pass.

#### `hookPass`

Meant to replace assignments and `mDestroy` calls with calls to the `=copy`, `=sink` and `=destroy` hooks (if they are present). This is a general pass that applies to all targets and garbage collectors.

Introduced before `LinearPass2` existed and is thus missing the `mDestroy` patching.

Currently also ignores whether or not a hook is trivial and thus replaces the assignment for types that don't actually need/use a `=copy` hook.

#### `refcPass`

Used when the `refc` GC is enabled. Transforms `ref` (and erroneously also `seq` and `string`) assignments; lifts GC visit procs; lowers `new`, etc

The GC visit procedure lifting is not yet implemented.

Doing the lifting in a separate pass would allow for the `refcPass` to also be applicable to the `markAndSweep` and `boehm` GCs. With some additional adjustments the `go` could also be included in that list.

#### `seqsV1Pass`

Lowers `seq`s to `PGenericSeq` based operations. Meant for the C-like targets and used when `optSeqDestructors` (implied by ARC/ORC) is not enabled.

All references to `seq` types need to be rewritten to use the `PGenericSeq` based type. Since seqs are generic, the new types need to be created as part of the pass (implemented).

The types of locals, globals an parametes are currently adjusted (in a rather in-elegant manner), but field types also need to adjusted! This is not easily doable with the current surrounding architecture and a rewrite of how the types are adjusted is planned.

A better approach for the type rewriting part would be to introduce a new kind of pass that operates only on types. `cbackend2` is then responsible for collecting all used types (this makes sense in general). The types could then also be run through a unification step, since most of the time, multiple `PType` instances exist for the exact same type (the VM also does this unification).

As a further improvement, the IR should use it's own representation of types and symbols (a very early prototype exists in `irtypes.nim`).

#### `seqsV2Pass`

Same as `seqsV1Pass`, but uses `NimSeqV2`. Also meant for the C-like targets and used when `optSeqDestructors` is enabled.


#### `typeV1Pass`

Creates globals to hold the used RTTI and also generates their initialization logic. Transforms usage of `mGetTypeInfo` to use the introduced globals. Meant for the C-like and JS targets.

The initialization logic generation is not implemented yet.


#### `lowerTestError`

Injects the pieces necessary for the `exceptions:goto` implementation on the C-like and JS targets. Not needed for the VM.

#### `lowerRangeCheckPass`

Transforms `bcRangeCheck` (coming from `nkRangeChck`) into comparisons plus `raiseRangeErrorXXX` calls. Meant to be a general pass, applying to all targets (except maybe the VM, since it currently does the checks at the instruction level).

#### `lowerSetsPass`

Lowers `set` operations into bit operations on integers and arrays. Meant for the C-like targets.

The required replacing of `set` types is missing.

### `irtypes.nim`

Very early and not yet used prototype of a type representation for the backend.

Using a dedicated type (symbols too) representation for the backend stage would mean that `PType` (and `PSym`) no longer has to accomodate for backend specific needs.

It also allows for using a more linear, data-oriented approach for storing the types without having to adjust all of the compiler. Since the backend is written with a data-oriented approach in mind, it would greatly benefit from this.


### `cbackend2.nim`

A copy of `vmbacked.nim` with the VM related bits removed. Adjusted to use the IR and `cgen2`.

Points of interest:
* `generateCode`: orchestrates IR generation for all alive procedures (`method` handling is missing) and calls the code-generator (`cgen2`)

**Note**: the DCE implementation currently runs before any IR transformations took place and thus doesn't know about used compilerprocs and some magics.

Semantic analysis and backend processing happen separate from each other. That is, first the semantic analysis for the whole program is performed and only then is the backend executed.

Pros:
* makes it easier to reason about the compiler
* compilerprocs in `system.nim` can be declared in any order, since once the backend is reached, all of them are available
* whole-program optimizations become possible

Cons:
* higher memory usage, since the bodies of all semantically analysed procedures need to be kept alive until the backend stage
* errors occuring in the back-end are only reported much later (but these error should only be internal ones, this shouldn't be a problem in practice)

For what it's worth, the VM backend (`vmbackend`) and the `PackedNode`-based backend (`cbackend`) meant for IC both also use the approach described here.

The backend only supports running it against the whole program right now, but it's written in a way that makes it easy to support smaller, more granular working sets.

### `cgen2.nim`

The code-generator for the C target. It takes the IR for all procedures in a module, translates them to a simple AST, and then emits the latter to the given output file.

The first iteration concatenated strings together with the plan to move to an AST-based approach later on, but I quickly figured that doing the switch already would make thing much simpler (and it did!). Some remnants of the original approach are still visible however.

Types currently use their own IR in order to make the whole dependency discovery easier. I'd consider the whole approach to type handling here wrong however. Figuring out the order in which types need to be emitted is not a problem specific to the C target and should thus be done outside of the C code-generator (this is also what's planned).

Instead of always writing to a file, it might make sense to pass `emitModuleToFile` a `Stream` instead and let the caller decide on where the output should go.

Points of interest:
* `genCode`: translates the input IR to the simple C AST
* `genCTypeDecl`: translates `PType` to a `CDecl`
* `emitModuleToFile`: the main entry point into the code generator. Orchestrates the C AST and type declaration generation and then emits everything in the correct order.