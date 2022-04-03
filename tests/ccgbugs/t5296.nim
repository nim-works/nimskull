discard """
matrix: "-d:release"
output: 1
"""

proc bug() : void =
    var x = 0
    try:
        inc x
        raise new(Exception)
    except Exception:
        echo x

bug()
