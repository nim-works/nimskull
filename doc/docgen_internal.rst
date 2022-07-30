=====================================
   Nim DocGen Tools implementation
=====================================

.. default-role:: code
.. include:: rstcommon.rst
.. contents::

:Author: haxscramper
:Version: |nimversion|


.. raw:: html
  <blockquote><p>
  "Documentation - a set of all information used by the developer or user to understand the principles of operation and use of the system." -- haxscramper
  </p></blockquote>

Introduction
============

This document provides explanation of the internal structure and
implementation of the documentation. First, high-level overview is provided
and then more detailed explanation is offered, with links to the relevant
documentation and module sections.

Second part of this document provides explanation on usage of the
documentation generator as a code analysis and data extraction tool. If you
are looking to implement a completely new output generator for the
documentation (either reimagining current HTML output, or supporting new
output format) or get some insights from the collected data you should look
into this section.

Internal implementation
=======================

Documentation processing pipeline consists of several stages, each dealing
with it's own set of data that is repeatedly transformed from one
representation to another - similar to how most of the compiler should be
functioning. High-level overview of the involved stages:

1. **Before sem**: First processing of the input is performed right before
   the semantic pass - this allows us to register most intricate details
   about the code structure: conditionally compiled and included files,
   fields hidden behind `when` check and so on.
2. **During sem**: Some of the most important activity happens *during*
   semantic pass - macro expansion. In order to properly track all the
   triggers and register generated source code (for later indexing) hooks
   have been added to trigger when macro or template is expanded
3. **After sem**: After semantic pass is completed each toplevel statement
   is examined in multiple smaller stages:

   a. *Definition registration* if statement is a entry declaration
      (procedure, function, method, type section etc.) it is unparsed into
      more concisely structured type `docgen_unparser.DefTree`, which
      compacts ~165 different enum kinds and multitude of semantic meanings
      into 12 documentable types with explicit semantic meaning
      (`docgen_unparser.DefTreeKind`)

   b. *Usage registration* each part of the documentable statement is
      further processed to get information about all usages of the types
      and procedures. Each toplevel statement is recursively traversed and
      all symbols are added in the database as occurrences.

Custom targets
==============

Previous section explains the full pipeline in more details, but for this
one you only need to know a general algorithm for data processing:
generator compiles and analyses all the code and generates intermediate
SQLite database, putting all the data into it. Then, second stage takes the
database and converts it into HTML you see in the default documentation.
Here we explain how to re-implement second part of this process: what do
you need to know, how to use this knowledge and so on.

Provided documentation does not expect you to use any specific language -
aside from support for interfacing with SQLite database and parsing JSON
there are no other prerequisites.

You can, of course, take advantage of the deserialization capabilities of
the documentation database and write your application in nim. Section
`Nim-Specific capabilities`_ covers that.

Database structure
------------------

Database consists of several tables with similar structure. Most entries
have unique primary ID. All tables are semi-automatically generated and
populated (see implementation in the `docgen_sqlite.nim`) based on types
defined in `docgen_types.nim`

1. `occurrences` - information about concrete location of the entry uses
   in the source code.

   + `kind`: column indicates the type of usage, with specific usage *name*
     stored in the `DocOccurKind` (1:1 mapping of the ) table. Specific
     values of the occurrence kinds are subject to change, so better use
     their explicit names.

   + `refid` (refs `entries.id`): used entry

   + `loc` (refs `locations.id`): location where occurrence had been
     registered.

   + `user` (refs `entries.id`): column shows the id of the user
     (`entries.id`). For example, when one procedure (`procA()`) calls
     another one (`procB()`), occurrence will have `refid=<id of procB>`
     and `user=<id of procA>`

2. `docs` - textual content of the documentation. Contains all the
   user-provided explanation - both regular text and runnable examples.

   + `runnable`: whether piece of text is a runnable code example or a
     freeform text.

   + `implicit`: if the code is an implicitly runnable example contains
     the ID of the file from which code had been extracted.

   + `text`: original text or code

   + `location` (refs `locations.id`): ID of the location of the text. Some
     of the documentation pieces represent whole files (`.rst`).

   + `tree`: if entry is not a runnable examples, contains serialized
     **processed** tree in JSON form. For more details see the next section
     on `Documentation text representation`_.

3. `entries`: complete list of all documentable entries that had ever
   been encountered in the code

   + `name`: the textual name (not unique) of the documented entry.

   + `kind`: the type of the found object, which in turn refers to
     `DocEntryKind` (reasoning is identical with `DocOccurKind`), which is
     responsible for storing the names of all possible and supported types.

   + `extent` (refs `extents.id`): id of the range of the found object in
     the source code. Using this field, you can access the full range of
     source code from which the entry was generated.

   + `location` (refs `locations.id`): location of the object in the source
     code. Using this field, you can access the location where the object
     was defined.

   + `parent` (refs `entries.id`): Optional id of the parent entry (if
     any). This field allows for tree-like structure of the project
     documentation to be preserved.

   + `node`: column contains a textual representation of the part of the
     syntax tree from which the entry was generated.

4. `docMap`: 1:N mapping between entry IDs and all the associated pieces of
   documentation.

   + `entry` (refs `entries.id`): Documentation target id

   + `idx`: Which piece of the documentation it is. Procedure might have
     several chunks - text spliced with blocks of `runnableExample`. This
     field allows to discern their order.

   + `doc` (refs `docs.id`): the content itself.

5. `locations`: information about specific singular locations in the code.

   + `file` (refs `files.id`): ID of the file in the `files` table

   + `line`, `col_end`, `col_start`: integers with line + column range of
     the location

6. `extents`: information about range of the source code. Structurally
   similar to the `locations`, but provides both `line_start` and
   `line_end` instead of a single point in code.

7. `files`: Extended information about files used in the project. Contains
   ID, absolute (`abs`) and relative (`rel`) (to the project) fields. Both
   code files and extra documentation (`.rst`) is referenced in this table.

8. `deprecated`: Extra information about deprecation annotations for
   entries that had it.

   + `id` (refs `entries.id`): deprecation annotation target

   + `msg`: Deprecation message if any.

This database structure was chosen to store as much information as
possible, without sacrificing the usability. In addition to simple data
representation it also targets a direct analysis via SQL queries.

.. TODO Write example of the SQL query for processing

Documentation text representation
---------------------------------

In addition to storing original documentation text, processed version is
provided in the `docs.tree` field, which allows developers to work with
higher-level representation even if their language does not have RST parser
implemented. Aside from saving your time and providing a pre-parsed text,
this field also resolves embedded links, and assigns IDs where possible
(referencing `entries.id`).

.. Another reason for this is absolutely nauseating structure of the RST
   AST itself, that provides half a dozen ways of writing an inline code
   block or link.

.. Also, if we plan to switch from RST to the Asciidoctor, abstracted
   representation will help here as well.

JSON is generated automatically from the `docgen_types.DocTextTree` - for
documentation in the particular details of the enum and field please refer
to the module itself.

Nim-Specific capabilities
-------------------------

This section provides pointers that will make it easier to use the
generated documentation database in nim. Although aside from support of the
native types there are not **explicit** upsides that make it a singular
choice for certain applications. Provided data model is designed to avoid
that as much as possible.

In order to deserialize the documentation database you need to use the
`readSqlite` procedure from the `docgen_sqlite`. It can be called without
complete setup of the `ConfigRef` - latter one is only used to store
information about files (`FileIndex` and `FileInfo` are also used in other
parts of the compiler, so they are not copied to the `DocDb` object).

When deserialization completes you are left with `DocDb` object that can be
used for documentation generation.

JSON representation of the processed RST is also deserialized back into the
`DocTextTree`.
