when isMainModule:
  import pkg/foo
  import pkg/foo/other

  assert foo.greeter() == "Hello from foo!"
  assert other.farewell() == "Goodbye from foo"