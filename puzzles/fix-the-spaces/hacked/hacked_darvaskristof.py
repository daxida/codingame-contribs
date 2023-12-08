# Solution by @DarvasKristof


# ABCABABC
# AB ABC CAB

# Expected solution
# AB CAB ACB


import sys

def E(*a): print(*a, file=sys.stderr)

def die():
    print("Unsolvable")
    exit(0)

def advance(set_words):
    global i
    s = list(set_words)[0]  # Why the first? Seems random
    o.append(s)
    words.remove(s)
    i += len(s)  

original = input()
words = input().split()
i = 0
o = []

while i < len(original):
    pr1 = set()
    for w in words:
        if original[i:].startswith(w):
            pr1.add(w)
    
    # Only one choice, no brainer
    if len(pr1) == 1:
        advance(pr1)
        continue
    
    # More than one choice for prefix
    pr2 = set()
    for w in pr1: # we check prefixes from pr1
        forward = i + len(w)
        if any(original[forward:].startswith(_w) for _w in words):
            pr2.add(w)
    
    E(pr1, pr2, words)
    if len(pr2) == 1:
        advance(pr2)
    else:
        die()

print(*o)