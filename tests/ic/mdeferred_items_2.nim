import mdeferred_items_1

# create an instantiation of the generic procedure with a type from the
# ``mdeferred_items_1`` module
proc call*(): int =
  result = test[Type[int]]()