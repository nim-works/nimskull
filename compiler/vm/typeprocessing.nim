## This module contains procedures for:
## * transforming types
## * collecting information about the properties of types

# XXX: some general reorganizing is needed. For now, this module just
#      contains procedures that are somewhat related to type processing
#      and don't have a better place to stay at

import
  compiler/vm/[
    bitsetutils,
    irtypes
  ]

func computeGcLookup*(types: TypeEnv): BitSet[TypeId] =
  ## Computes the set of types relevant to the garbage collector.
  ##
  ## .. note:: This procedure depends on the type ordering properties
  ##           that are present directly after type translation in order to
  ##           produce correct results!
  result = newBitSet(TypeId, types.numTypes)

  # at a high-level, what we're doing here is propagating a flag in
  # a directed-acyclic-graph (object types can be cyclic, but only via
  # indirection and the GC status isn't propagated through ``ref``, ``ptr``,
  # or ``seq`` types) from multiple root nodes to all nodes reachable from
  # them

  # first pass: propagate the GC status starting from the "root" types (ref,
  # seq, etc.)
  for id, t in types.items:
    case t.kind
    of tnkRef, tnkSeq, tnkString, tnkClosure:
      result.incl(id)
    of tnkArray:
      # arrays count as GC'ed if their element type is relevant to the GC
      if t.base in result:
        result.incl(id)

    of tnkRecord:
      if t.base != NoneType and t.base in result:
        # if the base type contains GC'ed locations, so does the sub-type
        result.incl(id)
      else:
        # if one of the record's fields is a or contains a GC'ed location, so
        # does the record itself
        for f in types.fields(t):
          if f.typ in result:
            result.incl(id)
            break

    else:
      discard

  # second pass: propagate the GC status again. As for why this is necessary,
  # consider the following (valid) visiting order:
  #   A = object | B = ref | C = tuple[B] | D = array[C]
  # For the sake of this example, 'A' has a field of type 'B'. All types would
  # need to have the "GC'ed" flag set here, but after the first pass this
  # would only be the case for 'B'!

  # XXX: see the comment in ``irtypes.flush`` for a different approach to type
  #      translation in regards to cyclic types that would make the second
  #      pass unnecessary
  for id, t in types.items:
    case t.kind
    of tnkArray:
      # TODO: profile to see if it'd make sense to implement this without
      #       branching
      if t.base in result:
        result.incl(id)

    of tnkRecord:
      if t.base != NoneType and t.base in result:
        result.incl(id)
      else:
        for f in types.fields(t):
          if f.typ in result:
            result.incl(id)
            break

    else:
      discard
