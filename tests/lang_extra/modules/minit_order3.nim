
echo "init_order3"

# prevent the optimizer from figuring out that the globals are constant
# and subsequently folding them away
func value*(v: int): int {.noinline.} = v

let value3* = value(3)