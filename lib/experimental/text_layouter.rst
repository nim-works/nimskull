This module implements combinator-based text layout algorithm, adapted from
google's rfmt `implementation <https://github.com/google/rfmt>`_. It
provides convenient primitves for building custom pretty-printers without
worrying about choosing optimal layout - you describe all possible layouts
and then get the most optimal one, given constraints on right and left
margins, line break and several others.

To construct block tree for layoyt you can either use `initLineBlock`,
`initStackBlock` and other functions or DSL.

DSL for tree construction is implemented in form of short-named templates,
allowing for easy splicing of custom logic. In order to avoid messing the
global namespace with two-letter named procedures the DSL should be
instantiated by the `initBlockFormatDSL` template. After using it, you can
call any of the followint gemplates.

- `lH(B..)` for **Horizontal** placement of one or more blocks `B..`
- `lV(B..)` for **Vertical**
- `lT(S..)` for **Text**
- `lI(N, B)` for **Indent** block `B` for `N` spaces.
- `lS(N)` for **Space**
- `lC(B..)` for **Choice**
- `lW(S, B..)` for **Wrap** rap one or more blocks automatically, depending on
   the allowed width. Elements are separated by the string `S`
- `lE()` for **Empty** - create a single empty space block

Extra helper routines were added to aid common string formatting
operations: `alignLeft`, `join`, `initAlignedGrid`.

**Note**

Horizontal layout combinator attaches topmost line in the block to the
lowest part of the preceding block, so arrangement ``H[V[T["[#]"],
T["[#]"]], V[T["[#]"], T["[#]"]]]`` would result in

.. code-block ::

  [#]
  [#][#]
     [#]
