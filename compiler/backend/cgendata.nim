## Implements the data types shared across the modules that make up the C code
## generator.

import
  compiler/backend/[
    cir
  ],
  compiler/mir/[
    mirenv
  ],
  compiler/ic/[
    bitabs
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

func getIdent*(env: CodeGenEnv, ident: CIdentifier): lent string =
  env.idents[LitId ident]

func addIdent*(env: var CodeGenEnv, ident: string): CIdentifier =
  ## Adds `ident` to the environment and returns the unique ID to later look
  ## it up with.
  CIdentifier env.idents.getOrIncl(ident)
