##[

This module implements closure iterator lowering transformation.
The lowering goes from a language with closure iterators to one with inline
iterators and higher-order functions (passing procedural values).

At its core the approach taken is to split the iterator body on `yield`
statements, assigning each split an ordinal value. The intervening code is put
into branches of a case statement which allows executing the code upto any
`yield` position by evaluating the `case` statement with from the last
unexecuted ordinal value to the desired position.

The overall approach requires a bit more machinery, starting with a
standardized-lowered-closure-iterator-protocol, who's type is as follows:

.. code-block:: nim
    type
      IterProtoState = enum
        iterNotStarted
        iterProtoHasProgress
        iterProtoDone

      IterProtoData[T] = tuple[IterProtoState, when T isnot void: T]

There is a pre-existing inline `items` iterator in order to traverse this
structure and produce values in typical iteration scenarios.

]##

import
  compiler/ast/[
    ast,
    ast_idgen,
    idents,
    types,
  ],
  compiler/modules/[
    modulegraphs,
  ],
  compiler/sem/[
    lambdalifting,
  ],
  compiler/utils/[
    idioms,
  ]

from compiler/modules/magicsys import nilOrSysInt

type
  Ctx = object
    g: ModuleGraph
    it: PSym
    stateVarSym: PSym  ## :state variable
    resultVarSym: PSym ## :result used to store the yielded/returned value
    states: seq[tuple[label: int, body: PNode]] ## states for each code slice

proc createClosureIterStateType(g: ModuleGraph, it: PSym,
                                idgen: IdGenerator): PType =
  result = newType(tyRange, nextTypeId(idgen), it)
  result.n =
    newTreeI(nkRange, it.info):
      [newIntNode(nkIntLit, -1), newIntNode(nkIntLit, 0)]
  let intType = nilOrSysInt(g)
  result.rawAddSon:
    if intType.isNil: newType(tyInt, nextTypeId(idgen), it) else: intType

proc lowerClosureIterator*(g: ModuleGraph, idgen: IdGenerator, it: PSym,
                           n: PNode): PNode =
  ## TODO: document
  ## `n` is the body of the closurer iterator to transforrm
  var ctx = Ctx(g: g, it: it,
                stateVarSym: newSym(skVar, getIdent(g.cache, ":state"),
                                    nextSymId(idgen),
                                    it,
                                    it.info,
                                    createClosureIterStateType(g, it, idgen)),
                )

  let
    itProtoConstrType: PType = nil # TODO: implement me
    itProtoConstrSym =
      block:
        let s = copySym(it, nextSymId(idgen))
        s.transitionRoutineSymKind(skProc)
        s.typ = nil # TODO: implement me
        s
    itProtoConstrBody =
      block:
        let b = toStmtList(n)
        # TODO: recursive transform of the body
        b
    itProtoConstr =
      newTreeIT(nkProcDef, n.info, itProtoConstrType):
        [
          newSymNode(itProtoConstrSym),
          g.emptyNode,
          copyTree(it.ast[genericParamsPos]), # xxx: is this the right thing to do?
          copyTree(it.ast[paramsPos]),        # xxx: mark this as converted?
          copyTree(it.ast[miscPos]),          # xxx: same as `genericParamsPos`
          itProtoConstrBody,
          nil,                            # TODO: update to return inner proc type
        ]
  # set `itProtoConstrSym.ast` to `itProtoConstr`