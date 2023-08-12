discard """
  errormsg: "Use a named tuple instead of: (string, float)"
  file: "json.nim"
"""

import std/json

type
  Car = object
    engine: (string, float)
    model: string

let j = """
  {"engine": {"name": "V8", "capacity": 5.5}, model: "Skyline"}
"""
let parsed = parseJson(j)
echo(to(parsed, Car))
