# Solution of user @DJT45

import numpy as np


def rules(x, y):
    return 0 + (x+y < 26) + (y <= x) - (y == 0)

m = np.fromfunction(rules, (26, 26))
a = np.int32(1)

r = int(input())
for c in input():
    a = np.convolve(a, m[ord(c) - 97])[:r + 1]

print(a.sum())
