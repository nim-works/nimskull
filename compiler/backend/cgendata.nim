## Implements the data types shared across the modules that make up the C code
## generator.

import
  std/[
    tables
  ],
  compiler/backend/[
    cir
  ],
  compiler/mir/[
    mirenv,
    mirtrees
  ],
  compiler/ic/[
    bitabs
  ],
  compiler/utils/[
    containers
  ]

type
  CodeGenEnv* = object
    ## Stores all the contextual state needed for C code generation, such as
    ## the external data for the CIR. This is generally information that is
    ## not local to single procedures.
    ##
    ## For convenience of the code generator, the ``MirEnv`` instance is also
    ## owned by this type.
    env*: MirEnv

    idents: BiTable[string]
      ## all identifiers

    # the names of the various entities are stored here
    # XXX: the code generator itself doesn't need access to the names,
    #      storing them in a separate type might be better architecturally (but
    #      maybe worse for performance, due to the extra parameter passing?)
    procs*: SeqMap[ProcedureId, CIdentifier]
    globals*: SeqMap[GlobalId, CIdentifier]
    constants*: SeqMap[ConstId, CIdentifier]
    # TODO: anonymous constants need to be handled somehow. They use different
    #       names depending on the module they're place in, so storing them
    #       here won't work. A separate type for the names is likely the best
    #       solution
    types*: Table[TypeId, CIdentifier]

func getIdent*(env: CodeGenEnv, ident: CIdentifier): lent string =
  env.idents[LitId ident]

func addIdent*(env: var CodeGenEnv, ident: string): CIdentifier =
  ## Adds `ident` to the environment and returns the unique ID to later look
  ## it up with.
  CIdentifier env.idents.getOrIncl(ident)
