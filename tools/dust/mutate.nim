## AST reduction and mutation routines, used by dust (the driver) to actually
## reduce a given module AST.

import
  compiler/ast/[
    ast
  ]

type
  Elem = tuple[n: PNode, i: int, next: pointer]
  # note: `next` is actually a `Continuation`, but structural types cannot be
  # cyclic and thus the type has to be erased
  Stack = seq[Elem]
  Jield = object
    ## The internal result of an mutation iterator invocation. If terminal
    ## (i.e., not holding a value), the iterator is done, otherwise `Jield`
    ## holds the yielded value (i.e., mutation) plus the continuation
    ## representing the next iterator step.
    case isValue: bool
    of true:
      n: PNode
      resume: proc(stack: var Stack, n: PNode): Jield
    of false:
      discard

  Continuation = proc(stack: var Stack): Jield {.tailcall.}

  MutationIter* = object
    ## The mutation iterator state.
    stack: Stack
      ## the manually-managed call stack of the mutator
    n: PNode
      ## the current mutation, or nil, if there are no more possible mutations
    resume: proc(stack: var Stack, n: PNode): Jield
      ## the continuation to call for fetching the next mutation

const
  DynamicSize = {nkBracket, nkCurly, nkRecList, nkTupleTy, nkPragma,
      nkGenericParams}
    ## syntax where an arbitrary number of children is allowed (including none)
  WithBody {.used.} = callableDefs +
      {nkElifExpr, nkElifBranch, nkElse, nkElseExpr, nkWhileStmt, nkForStmt,
       nkBlockExpr, nkBlockStmt, nkPragmaBlock, nkOfBranch}
    ## syntax with a fixed number of children, where the last child represents
    ## a body (statement or expression)
  WithBodyReplaceable = callableDefs +
      {nkWhileStmt, nkForStmt, nkBlockExpr, nkBlockStmt, nkPragmaBlock}
    ## syntax that may be replace with its body (i.e., the last child)
  FixedFirstChild = {nkCall, nkCurlyExpr, nkBracketExpr, nkObjConstr,
      nkFormalParams} - {nkPostFix}
    ## syntax where the child has to always be present
  ListLike = {nkTupleConstr, nkTableConstr, nkVarSection, nkLetSection,
      nkConstSection, nkTypeSection, nkBindStmt, nkMixinStmt, nkEnumTy, nkUsingStmt,
      nkImportStmt}
    ## syntax that must always have at least one element
  IfLike = {nkIfExpr, nkIfStmt, nkWhenStmt, nkRecWhen}
    ## syntax that has branches (e.g., `nkElse`, `nkElifBranch`, etc.) as children

using
  next: Continuation

# since NimSkull lacks coroutines (or something to implement them with, such
# as algebraic effects), the iterator has to be implemented as a set of
# hand-written procedures using continuation-passing style (with manually
# create continuations). Yielding from the iterator then amounts to returning
# a properly set-up `Jield` value from one of the constituent procedures

proc step(stack: var Stack, n: sink PNode, next): Jield {.tailcall.}

proc drop(n: sink PNode) =
  discard n

proc apply(stack: var Stack, next): Jield {.tailcall.} =
  if next.isNil:
    # no continuation; we're done
    assert stack.len == 0
    Jield(isValue: false)
  else:
    next(stack)

proc update(stack: var Stack, n: PNode) =
  ## Synchronizes the stack with the new root `n`.
  var current {.cursor.} = n
  for i in 0..<stack.len:
    stack[i].n = current
    # the index for the last entry may be invalid; don't use it for lookup
    if i < stack.high:
      current = current[stack[i].i]

proc extract(stack: Stack, idx: BackwardsIndex, n: sink PNode): PNode =
  ## Creates a tree derived from the root tree but with the node at the
  ## position the `idx`-th frame points to being replaced with `n`.
  result = n
  for i in countdown(stack.len - int(idx), 0):
    let tree = copyNodeWithKids(stack[i].n)
    tree[stack[i].i] = result
    result = tree

proc replaceAndEnter(stack: Stack, n: sink PNode, next): Jield {.tailcall.} =
  ## 1. yields a mutation where the node at the parent frame's tree position
  ##    is replaced with `n`
  ## 2. (when committing) returns from the current frame and steps into `n`
  ## 2. (when not committing) steps into the current node, invoking `next`
  ##    upon returning
  proc resume(stack: var Stack, n: PNode): Jield =
    if n != nil:
      let (_, _, ret) = stack.pop()
      update(stack, n)
      step(stack, stack[^1].n[stack[^1].i], cast[Continuation](ret))
    else:
      step(stack, stack[^1].n[stack[^1].i], next)

  Jield(isValue: true, n: extract(stack, ^2, n), resume: resume)

proc replaceParent(stack: Stack, n: sink PNode, next): Jield {.tailcall.} =
  ## 1. yields a mutation where the node at the parent frame's tree position is
  ##    replaced with `n`.
  ## 2. (when committing) returns to parent frame
  ## 2. (when not committing) invokes `next`
  proc resume(stack: var Stack, n: PNode): Jield =
    if n != nil:
      let (_, _, next) = stack.pop()
      update(stack, n)
      apply(stack, cast[Continuation](next))
    else:
      next(stack)

  Jield(isValue: true, n: extract(stack, ^2, n), resume: resume)

proc replaceOrEnter(stack: Stack, n: sink PNode, next): Jield {.tailcall.} =
  ## 1. yields a mutation where the node at the *current* tree position is
  ##   replaced with `n`
  ## 2. (when not committing) enters the sub-tree at the current position
  ## 3. invokes `next`
  proc resume(stack: var Stack, n: PNode): Jield =
    if n != nil:
      update(stack, n)
      next(stack)
    else:
      step(stack, stack[^1].n[stack[^1].i], next)

  Jield(isValue: true, n: extract(stack, ^1, n), resume: resume)

proc removeOrEnter(stack: Stack, onKeep, onFail: Continuation): Jield {.tailcall.} =
  ## 1. yields a mutation where the node at the current tree position
  ##    is removed
  ## 2. (when committing) invokes `onKeep`
  ## 2. (when not committing) steps into the node at the current position,
  ##    invoking `onFail` upon returning
  let modified = copyNodeWithKids(stack[^1].n)
  modified.delSon(stack[^1].i)

  proc resume(stack: var Stack, n: PNode): Jield =
    if n.isNil:
      step(stack, stack[^1].n[stack[^1].i], onFail)
    else:
      update(stack, n)
      onKeep(stack)

  Jield(isValue: true, n: extract(stack, ^2, modified), resume: resume)

proc removeOrEnter(stack: Stack, next): Jield {.tailcall.} =
  removeOrEnter(stack, next, next)

proc ret(stack: var Stack): Jield {.tailcall.} =
  ## Applies a "return" action to the call stack.
  let (n, _, next) = stack.pop()
  drop n
  apply(stack, cast[Continuation](next))

proc mutateDyn(stack: var Stack): Jield {.tailcall.} =
  # for each node/subtree, first try removing it from the list-like tree. If
  # that doesn't work, reduce the node/tree
  let fr {.cursor.} = stack[^1]
  if fr.i >= 0:
    removeOrEnter(stack,
      proc(stack: var Stack): Jield {.tailcall.} =
        dec stack[^1].i
        mutateDyn(stack))
  else:
    ret(stack)

proc mutateListElems(stack: var Stack): Jield {.tailcall.} =
  # step into every element, but don't modify the shape of the list-like
  # tree itself
  let fr {.cursor.} = stack[^1]
  if fr.i >= 0:
    step(stack, fr.n[fr.i],
      proc(stack: var Stack): Jield {.tailcall.} =
        dec stack[^1].i
        mutateListElems(stack))
  else:
    ret(stack)

proc mutateList(stack: var Stack): Jield {.tailcall.} =
  # works similar to `mutateDyn`, but makes sure that there's always at least
  # one subnode in the list
  let fr {.cursor.} = stack[^1]
  if fr.i >= 0:
    if fr.n.len == 1:
      step(stack, fr.n[0], ret)
    else:
      removeOrEnter(stack,
        proc(stack: var Stack): Jield {.tailcall.} =
          dec stack[^1].i
          mutateList(stack))
  else:
    ret(stack)

proc mutateStmtList(stack: var Stack): Jield {.tailcall.} =
  # a specialization of `mutateList`. Removes the wrapping nkStmtList if it
  # has only one subnode
  let fr {.cursor.} = stack[^1]
  if fr.i == 0 and fr.n.len == 1:
    step(stack, fr.n[0],
      proc(stack: var Stack): Jield {.tailcall.} =
        replaceParent(stack, stack[^1].n[0], ret))
  elif fr.i >= 0:
    removeOrEnter(stack,
      proc(stack: var Stack): Jield {.tailcall.} =
        dec stack[^1].i
        mutateStmtList(stack))
  else:
    ret(stack)

proc mutateIdentDefs(stack: var Stack): Jield {.tailcall.} =
  proc next(stack: var Stack): Jield {.tailcall.} =
    dec stack[^1].i
    mutateIdentDefs(stack)

  let fr {.cursor.} = stack[^1]
  if fr.i >= fr.n.len - 2: # type and value slot
    if fr.n[^1].kind != nkEmpty and fr.n[^2].kind != nkEmpty:
      # when there's something in both the type and value slot, removing
      # either the value or type expression could work
      replaceOrEnter(stack, newNode(nkEmpty), next)
    else:
      step(stack, fr.n[fr.i], next)
  elif fr.i >= 0: # identifier slots
    if fr.n.len > 3:
      removeOrEnter(stack, next)
    else:
      step(stack, fr.n[fr.i], next)
  else:
    ret(stack)

proc mutateWithBody(stack: var Stack): Jield {.tailcall.} =
  proc next(stack: var Stack): Jield {.tailcall.} =
    dec stack[^1].i
    mutateWithBody(stack)

  # first, reduce the body (in the last slot). Afterwards, try replacing the
  # node with the reduced body. If the latter fails, only reduce the
  # remaining subnodes
  let fr {.cursor.} = stack[^1]
  if fr.i == fr.n.len - 1:
    step(stack, fr.n[fr.i],
      proc(stack: var Stack): Jield {.tailcall.} =
        replaceParent(stack, stack[^1].n[^1], next))
  elif fr.i >= 0:
    step(stack, fr.n[fr.i], next)
  else:
    ret(stack)

proc mutateIfLike(stack: var Stack): Jield {.tailcall.} =
  let fr {.cursor.} = stack[^1]
  if fr.i >= 0:
    replaceAndEnter(stack, fr.n[fr.i][^1],
      proc(stack: var Stack): Jield {.tailcall.} =
        dec stack[^1].i
        mutateIfLike(stack))
  else:
    ret(stack)

proc mutateFixedFirst(stack: var Stack): Jield {.tailcall.} =
  # like `mutateDyn`, but with the first subnode always being kept
  let fr {.cursor.} = stack[^1]
  if fr.i >= 1:
    removeOrEnter(stack,
      proc(stack: var Stack): Jield {.tailcall.} =
        dec stack[^1].i
        mutateFixedFirst(stack))
  else: # i == 0
    step(stack, fr.n[0], ret)

proc mutateCaseStmt(stack: var Stack): Jield {.tailcall.} =
  let fr {.cursor.} = stack[^1]
  if fr.i >= 1:
    replaceAndEnter(stack, fr.n[fr.i][^1],
      proc(stack: var Stack): Jield {.tailcall.} =
        dec stack[^1].i
        mutateCaseStmt(stack))
  else: # i == 0
    step(stack, fr.n[0], ret)

proc mutatePostfix(stack: var Stack): Jield {.tailcall.} =
  replaceAndEnter(stack, stack[^1].n[1], ret)

proc step(stack: var Stack, n: sink PNode, next): Jield {.tailcall.} =
  ## Process `n`, invoking `next` once done.
  proc push(stack: var Stack, n: sink PNode, next) {.inline.} =
    stack.add (n, n.len - 1, cast[pointer](next))

  case n.kind
  of nkWithoutSons:
    # nothing to do; just return
    drop(n)
    apply(stack, next)
  of nkIdentDefs, nkConstDef:
    push(stack, n, next)
    mutateIdentDefs(stack)
  of nkCaseStmt, nkRecCase:
    push(stack, n, next)
    mutateCaseStmt(stack)
  of FixedFirstChild:
    push(stack, n, next)
    mutateFixedFirst(stack)
  of WithBodyReplaceable:
    push(stack, n, next)
    mutateWithBody(stack)
  of DynamicSize:
    push(stack, n, next)
    mutateDyn(stack)
  of ListLike:
    push(stack, n, next)
    mutateList(stack)
  of nkStmtList, nkStmtListExpr:
    push(stack, n, next)
    mutateStmtList(stack)
  of IfLike:
    push(stack, n, next)
    mutateIfLike(stack)
  # of nkCommand:
  #   mutateCommand(stack)
  of nkPostfix:
    push(stack, n, next)
    mutatePostfix(stack)
  # of nkPragmaExpr:
  #   mutatePragmaExpr(stack)
  else:
    push(stack, n, next)
    mutateListElems(stack)

proc initMutator*(root: PNode): MutationIter =
  ## Sets up a mutation iterator over `root`, moving it to the first
  ## candidate, if one exists.
  result = MutationIter()
  let got = step(result.stack, root, nil)
  if got.isValue:
    result.n = got.n
    result.resume = got.resume

proc get*(mut: MutationIter): PNode =
  ## Returns the current mutation.
  mut.n

proc next*(mut: var MutationIter) =
  ## Computes the next mutant and loads it into `mut`.
  let got = mut.resume(mut.stack, nil)
  if got.isValue:
    mut.n = got.n
    mut.resume = got.resume
  else:
    mut.n = nil

proc keep*(mut: var MutationIter) =
  ## Makes the current mutant the new root and computes the next mutant based
  ## on it. This is functionally equivalent to spawning a new iterator with
  ## for the current mutant and moving its tree position to that of `mut`.
  let got = mut.resume(mut.stack, mut.n)
  if got.isValue:
    mut.n = got.n
    mut.resume = got.resume
  else:
    mut.n = nil
