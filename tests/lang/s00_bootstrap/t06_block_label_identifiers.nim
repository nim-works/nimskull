discard """
  description: "Describe block labels and valid label identifiers."

  # the valid identifier rules in general also apply to label identifiers and
  # we'll be using and building upon these rules going forward.
"""

## blocks can have labels
block label:
  discard

## labels will later be used for control flow, but for now they're any valid
## identifier, and we can use them as "titles" in the spec
block whatever_valid_identifier_suits_our_fancy:
  discard "consecutive underscores are `not__valid`"
  discard "nor are trailing understores `no_tails_`"

block identifiers_start_with_an_alphabet_character:
  block Or_upper_case:
    block did_we_mention_blocks_can_be_nested:
      discard # well they can

    block numbers_like_10_can_appear_after_the_first_letter:
      discard
