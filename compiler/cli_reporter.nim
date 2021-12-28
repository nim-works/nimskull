import reports

## Implementation of the default command-line error hook. All the
## pretty-printed messages are constructed in this module.

proc reportHook*(report: Report) =
  echo report
