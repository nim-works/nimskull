## This modules describes a variety of string literals. The discard preceding
## the literals are to ensure forward compatibility with the overall spec.
discard "Hello, World!"
discard ""               ## an empty string

## we can also triple-quote strings:
discard """action: compile""" # this is secretly instructing our test harness


# rest of the spec must stay below this line or testament gets grumpy

discard """a real        # whoops, this isn't a comment; it's in the string
multi-line string
is more like this
"""

discard """"""           ## an empty multi-line string

discard """
we can start here as the first line
then the second
and so on
"""

discard """
  starting with a leading
  indent applies to all lines
  this will be necessary soon
"""

## regular quoted strings can appear in multi-line strings because multi-line
## variant look for `"""`
discard """
{
  "key": "value",
  3:     "three"
}
""" # it's not json, just a strong