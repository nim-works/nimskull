Generate tree representation from `PNode`.

Procedure arguments:

- `conf`: some configuration options are inferred from it, such as
  `--filenames:canonical`, but it is mostly used to get full paths from
  file ids
- `pnode`: node to be printed - can be `nil`, recursive, `nkError` or any
  other kind form or shape.
- `flags`: Set of the formatting options for the procedure. Several
  built-in constants are defined in this module that might be suitable for
  different debugging or AST exploration tasks.
- `maxDepth`: Ignore all nodes that are placed deeper than that. Useful to
  see a high-level overview for large nodes, such as full proc bodies
- `maxLength`: on each level, print upto that many subnodes. Useful to cut
  out parts on large `case` trees, toplevel nodes for modules, statement
  lists and such.
- `maxPath`: maximum path length for subnodes
- `indentIncreas`: start tree indentation from certain formatting

Generated output packs in a lot of information - here is a short list of
features, those will be elaborated on

- subnode index in relation to the parent node, optionally extended index
  (like`[0][0]`)
- symbol symbol kind, flags, other relevant elements
- type, if exists
- comment message (optionally)
- all node flags
- all IDs when applicable
- maximum target node depth and maximum node length (to avoid drowning in
  dumps when printing large entries)
- pack things as close as possible horizontally, or make them more spaced
  out vertically
- node declaration information
- any associated structured reports (IDs, kinds, generation location)
- symbol definition location if any
- type symbols definition location if any


.. note:: Output examples in the documentation might be slightly different
          from the actual output due to changes in the default formatting
          options, or layout changes.

.. code-block:: literal

    Call
    0 Sym vmTarget (file.vmTarget)
        sk:   Proc
        flags:{Used, Global}
        typ:  Proc (arg):
    1 ObjConstr
        nflags:{Sem}
        typ:  Object sk:Type
    1.0 Sym Obj (file.Obj)
          sk:   Type
          flags:{Used, Global}
          typ:  Object sk:Type


This is a tree representation listing from the end of the
`semexprs.semExpr` for this piece of code:

.. code-block:: nim

  vmTarget(Obj())

- `.0`, `.1` and `.1.0` show subnode positions of the analyzed input. This
  is very useful when you need to index into input tree - you can
  immediately see necessary positions.

  **TODO**: Allow printing out special defines, like `ast.pragmasPos`
  instead of the hardcoded indices

- After node position comes the node symbol - `Call`, `Sym`, `ObjConstr`
  and so on - they correspond to the value of the `.kind` field of the
  node.

- If node in question is some form of token (identifier, symbol, text or
  numeric literal) it is printed after node kind - `Sym vmTarget` means
  that it is a symbol that `mvTarget` has resolved into, in your code.

  - If symbol node has an `.owner` set, it is printed adjacent to the
    name - `file.vmTarget` means that input node symbol had `owner` and
    it's name was `file`

- In addition to short summary of the node kind and it's 'value'
  information is printed in the fields down below. Most fields are
  optional, and displayed only if target value is non-nil or not empty (for
  sets)

  - `sk` stands for 'symbol kind' and shows which specific kind of the
    symbol - `skProc`, `skLet` etc.
  - `flags` - set of the `TNodeFlag` enums
  - `typ` - type of the node
  - `err` - optional field, shown only for `nkError` nodes. Contains type
    of the error report. Corresponds to the `rsem*` report kinds in the
    `reports.ReportKind`
  - `errid` - shown after error report kind and corresponds to the ID of
    the *stored* error report. Note that only reports that are processed
    via `nkError` are enumerated, so `errid:1` does not mean it is the
    first report ever created, it means it it the first report that got
    into AST. Warnings, hints and other internal messages are processed
    in different channel.



The `treeRepr` is also useful for printing out AST with `nkError` nodes.
This piece of code will fail to compile because there is no `+` overload
that can accept `Obj()` and `12`

.. code-block:: nim

    vmTarget(Obj() + 2)


.. code-block:: nim


    Error
      err:  CallTypeMismatch errid:2
      typ:  Proxy
    0 Call
    0.0 Ident vmTarget
    0.1 Error
          err:  CallTypeMismatch errid:1
          nflags:{Sem}
          typ:  Proxy
    0.1.0 Infix
      1.0.0 Ident +
      1.0.1 ObjConstr
              nflags:{Sem}
              typ:  Object sk:Type
        0.1.0 Sym Obj (file.Obj)
                sk:   Type
                flags:{Used, Global}
                typ:  Object sk:Type
      1.0.2 IntLit 2
              nflags:{Sem}


Note the `Error` node at `0.1` in the ast. In addition to the regular
fields it shows error type (`CallTypeMismatch` and error report id). Note
that due to `nkError` propagation AST contains multiple error nodes.
Innermost was caused by missing `+` overload, and then `vmTarget` also
failed to resolve, due to malformed arguments.
