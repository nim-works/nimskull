discard """
  description: "Regression tests for a generic type instantiation cache issue"
"""

type
  Container[O] = object
    # 3.) for ``Container[Generic[T]]``, `O` was replaced with the resolved
    #     ``Generic[int]``, but since there were only unfinished
    #     ``Container[]`` instances in the cache so far, another new unfinished
    #     instance is commited to the cache and attempted to be resolved. This
    #     led to infinite recursion, which was eventually bailed out of via a
    #     recursion limit
    val: seq[Container[O]]

  Generic[T] = object
    n: Container[Generic[T]] # 2.) since the argument wasn't resolved early, the
                             #     cache lookup failed, and a new unfinished
                             #     ``Container[int]`` was commited to the cache

  Head[T] = object
    a: Container[Generic[T]] # 1.) a ``Container[]`` was commited to the cache
                             #     here
    b: Container[T] # 4.) a ``Container[int]`` instance is attempted to be
                    #     created. However, since the recursion counter is only
                    #     incremented and never reset, this is treated as too
                    #     deep recusion and the locally cached
                    #     ``Container[Generic[int]]`` was used

var head = Head[int]()
static:
  doAssert typeof(head.a.val[0]) is Container[Generic[int]]
  doAssert typeof(head.b.val[0]) is Container[int]
  # both fields have a different type, and comparison must thus not work
  doAssert(not compiles(head.a.val == head.b.val))

# test that the variable can be used at run-time:
head.a.val.add Container[Generic[int]]()
head.b.val.add Container[int]()