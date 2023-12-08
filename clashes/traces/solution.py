# source: https://en.wikipedia.org/wiki/Trace_monoid

import re

def solve():
    a, b, fixed = input().split()
    
    assert len(set(fixed)) == len(fixed)
    assert a == a.lower()
    assert b == b.lower()
    assert fixed == fixed.lower()
    
    # Same length and same chars
    if len(a) != len(b) or set(a) != set(b):
        return False

    # Same positions for fixed chars
    for c1, c2 in zip(a, b):
        if c1 in fixed or c2 in fixed:
            if c1 != c2:
                return False
    
    # Same chars in "islands"
    sep = '|'.join(fixed)
    aa = re.split(sep, a)
    bb = re.split(sep, b)
    for subset1, subset2 in zip(aa, bb):
        if sorted(subset1) != sorted(subset2):
            return False
    
    return True

t = int(input())
for _ in range(t):
    ans = solve()
    print(str(ans).lower())
