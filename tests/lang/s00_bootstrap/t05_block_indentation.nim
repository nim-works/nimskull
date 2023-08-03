discard """
  description: "Describe the basics of blocks and indentation."

  # the rest of the spec modules will almost always have this discarded triple-
  # quoted literal header; we use them to instruct the spec execution tool.
"""

block:      ## this is a block
  discard   ## notice how we indented this line?

## block: # alone, with nothing after would be an error
## Nimskull is blankspace sensitive

## indentation levels do not need to be consistent across the file, only
## per block
block:
        discard
