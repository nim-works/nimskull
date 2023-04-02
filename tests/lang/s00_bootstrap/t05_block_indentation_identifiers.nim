discard """
  description: "Describe the basics of blocks and indentation."

  # the rest of the spec modules will almost always have this discarded triple-
  # quoted literal header; we use them to instruct the spec execution tool.
"""

block:      ## this is a block
  discard   ## notice how we indented this line?

## block: # alone, with nothing after would be an error
## Nimskull is blankspace sensitive

## indentation levels does not need to be consistent across the file, only
## per level
block:
        discard

## blocks can have labels
block label:
  discard

## labels will later be used for control flow, but for now they're any valid
## identifier, and we can use them as "titles" in the spec
block whatever_valid_identifier_suits_our_fancy:
  discard
