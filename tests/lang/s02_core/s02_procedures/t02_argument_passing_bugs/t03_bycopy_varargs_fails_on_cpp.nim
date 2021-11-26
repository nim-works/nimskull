discard """
description: '''
Compiling the code on CPP backend fails with codegen error, C backend works
correctly

```cpp
Error: could not convert ‘{{(& NTIcbase__NLOi0tG1VqC0j2IvzOH9akQ_)}}’ from ‘<brace-enclosed initializer list>’ to ‘tyObject_CBase__NLOi0tG1VqC0j2IvzOH9akQ’
```

'''
knownIssue: "bycopy varargs fails on cpp backendq "
targets: "cpp"
"""


type
  CBase {.bycopy.} = object of RootObj

proc impl(args: varargs[CBase]) = discard

impl(CBase())