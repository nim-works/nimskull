This directory contains tests related to the implicit converters. It is
split into multiple files because converters cannot be declared in the
`block:` sections, so making a clean test (without any possibility of
triggering converters defined previously) requires multiple files.