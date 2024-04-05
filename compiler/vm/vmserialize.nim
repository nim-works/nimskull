## Implements routines for initializing VM memory locations directly from
## expressions.

import
  compiler/ast/[
    ast_query,
    ast_types,
    types
  ],
  compiler/front/[
    options
  ],
  compiler/mir/[
    mirenv,
    mirtrees
  ],
  compiler/vm/[
    vmaux,
    vmdef,
    vmobjects,
    vmmemory,
    vmtypegen,
    vmtypes
  ],
  compiler/utils/[
    bitsets,
    idioms,
    int128
  ]

proc initFromExpr(dest: LocHandle, tree: MirTree, n: var int, env: MirEnv,
                  c: var TCtx) =
  ## Loads the value represented by `tree` at `n` into `dest`. On exit, `n`
  ## points to the next sub-tree.
  template recurse(dest: LocHandle) =
    initFromExpr(dest, tree, n, env, c)

  template next(): lent MirNode =
    let i = n
    inc n
    tree[i]

  template arg(body: untyped) =
    inc n # skip the ``mnkArg`` node
    body
    inc n # skip the end node

  template iterTree(name, body: untyped) =
    let len = next().len
    for name in 0..<len:
      body
    inc n # skip the end node

  case dest.typ.kind
  of akInt:
    writeUInt(dest, next().lit.intVal)
  of akDiscriminator:
    # handled during object processing below
    unreachable("cannot be written directly")
  of akFloat:
    if dest.typ.sizeInBytes == 4:
      writeFloat32(dest, float32(next().lit.floatVal))
    else:
      writeFloat64(dest, float64(next().lit.floatVal))
  of akString:
    deref(dest).strVal.newVmString(env[next().strVal], c.allocator)
  of akSeq:
    # allocate the sequence first:
    newVmSeq(deref(dest).seqVal, dest.typ, tree[n].len, c.memory)
    # then initialize the elements:
    let slice = loadFullSlice(c.allocator, deref(dest).seqVal.data,
                              dest.typ.seqElemType)
    iterTree(j):
      arg recurse(slice[j])
  of akPtr:
    # nothing to do, only nil literals are allowed here
    discard next()
  of akRef:
    if tree[n].kind == mnkLiteral:
      discard next() # nothing to do for 'nil' literals
    else:
      # allocate a managed heap location and fill it:
      let
        t = c.getOrCreate(tree[n].typ)
        slot = c.heap.heapNew(c.allocator, t.targetType)
      recurse(c.heap.unsafeDeref(slot))
      deref(dest).refVal = slot
  of akSet:
    proc adjusted(n: PNode, first: Int128): BiggestInt {.inline.} =
      # subtract the first element's value to make all values zero-based
      toInt(getInt(n) - first)

    let first =
      if tree[n].len > 0: firstOrd(c.config, tree[n].typ)
      else:               Zero
    # XXX: ^^ ``set[empty]``-typed literals reach here, but they shouldn't. The
    #      len guard works around the issue
    iterTree(j):
      let node = next()
      if node.kind == mnkRange:
        let
          a = adjusted(next().lit, first)
          b = adjusted(next().lit, first)
        bitSetInclRange(mbitSet(dest), a .. b)
        inc n # skip the end node
      else:
        bitSetIncl(mbitSet(dest), adjusted(node.lit, first))
  of akPNode:
    deref(dest).nodeVal = next().lit[0]
  of akCallable:
    deref(dest).callableVal = toFuncPtr FunctionIndex(next().prc)
  of akObject:
    # the source can either be an object or tuple constructor
    case tree[n].kind
    of mnkLiteral:
      # special case: nil closure literal
      assert tree[n].lit.kind == nkNilLit
      # only skip the node, don't initialize anything
      discard next()
    of mnkTupleConstr, mnkClosureConstr:
      iterTree(j):
        arg recurse(dest.getFieldHandle(j.FieldPosition))
    of mnkObjConstr:
      let typ = tree[n].typ.skipTypes(abstractPtrs) ## the object's type
      iterTree(i):
        let
          sym = lookupInType(typ, next().field)
          field = dest.getFieldHandle(sym.position.FieldPosition)
        # object types require special handling for tag fields
        if sfDiscriminant in sym.flags:
          let (owner, idx) = getFieldAndOwner(dest.typ, fpos sym.position)
          # fetch the integer value:
          var val: Int128
          arg (;val = getInt(next().lit))
          # compute the branch index:
          let b = findMatchingBranch(findRecCase(typ, sym), val)
          # write the tag value to the location:
          field.writeDiscrField(owner, idx, toInt(val), b)
        else:
          arg recurse(field)
    else:
      unreachable(tree[n].kind)
  of akArray:
    let slice = toSlice(dest)
    iterTree(i):
      arg recurse(slice[i])

proc initFromExpr*(dest: LocHandle, tree: MirTree, env: MirEnv,
                   c: var TCtx) {.inline.} =
  ## Intializes the memory location `dest` with the value represented by the
  ## MIR contant expression `tree`. The location is expected to be in its
  ## zero'ed state.
  var i = 0
  initFromExpr(dest, tree, i, env, c)
