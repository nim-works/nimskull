# v1.8.x - yyyy-mm-dd


## Changes affecting backward compatibility



- `std/sharedstrings` module is removed.
- Constants `colors.colPaleVioletRed` and `colors.colMediumPurple` changed to match the CSS color standard.

- `addr` is now available for all addressable locations, `unsafeAddr` is deprecated and
becomes an alias for `addr`.

## Standard library additions and changes

- `macros.parseExpr` and `macros.parseStmt` now accept an optional
  filename argument for more informative errors.
- Module `colors` expanded with missing colors from the CSS color standard.
- Fixed `lists.SinglyLinkedList` being broken after removing the last node ([#19353](https://github.com/nim-lang/Nim/pull/19353)).

- `std/smtp` sends `ehlo` first. If the mail server does not understand, it sends `helo` as a fallback.

- Added `IsoWeekRange`, a range type to represent the number of weeks in an ISO week-based year.
- Added `IsoYear`, a distinct int type to prevent bugs from confusing the week-based year and the regular year.
- Added `initDateTime` in `times` to create a datetime from a weekday, and ISO 8601 week number and week-based year.
- Added `getIsoWeekAndYear` in `times` to get an ISO week number along with the corresponding ISO week-based year from a datetime.
- Added `getIsoWeeksInYear` in `times` to return the number of weeks in an ISO week-based year.

- Added `std/oserrors` for OS error reporting. Added `std/envvars` for environment variables handling.
- Removed deprecated `oids.oidToString`.


- Changed mimedb to use an `OrderedTable` instead of `OrderedTableRef`, to use it in a const.

- Removed deprecated `math.c_frexp`.


## Language changes


  ```nim
  import macros

  macro multiply(amount: static int, s: untyped): untyped =
    let name = $s[0].basename
    result = newNimNode(nnkTypeSection)
    for i in 1 .. amount:
      result.add(newTree(nnkTypeDef, ident(name & $i), s[1], s[2]))

  type
    Foo = object
    Bar {.multiply: 3.} = object
      x, y, z: int
    Baz = object

  # becomes

  type
    Foo = object
    Bar1 = object
      x, y, z: int
    Bar2 = object
      x, y, z: int
    Bar3 = object
      x, y, z: int
    Baz = object
  ```
- [Case statement macros](manual.html#macros-case-statement-macros) are no longer experimental,
  meaning you no longer need to enable the experimental switch `caseStmtMacros` to use them. 

## Compiler changes

- `nim` can now compile version 1.4.0 as follows: `nim c --lib:lib --stylecheck:off compiler/nim`,
  without requiring `-d:nimVersion140` which is now a noop.


## Tool changes



- Nim now supports Nimble version 0.14 which added support for lock-files. This is done by
  a simple configuration change setting that you can do yourself too. In `$nim/config/nim.cfg`
  replace `pkgs` by `pkgs2`.

- There is a new switch `--nimMainPrefix:prefix` to influence the `NimMain` that the
  compiler produces. This is particularly useful for generating static libraries.
