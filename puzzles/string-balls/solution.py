import string
import random

alphabet = string.ascii_lowercase

def dist(chr_a, chr_b):
    return abs(ord(chr_a) - ord(chr_b))

def iterative(center, radius):
    cur = [("", 0)]
    for c1 in center:
        nxt = []
        for word, d in cur:
            for c2 in alphabet:
                nd = d + dist(c1, c2)        
                if nd <= radius:
                    nword = word + c2
                    nxt.append((nword, nd))

        cur = nxt

    return cur

def recursive(center, radius, l, word="", d=0, size=0):
    if size == l:
        return [word]

    solutions = []
    c1 = center[size]

    for c2 in alphabet:
        nd = d + dist(c1, c2)
        if nd <= radius:
            nword = word + c2
            tmp = recursive(center, radius, l, nword, nd, size + 1)
            solutions.extend(tmp)

    return solutions

def main():
    radius = int(input())
    center = input()

    if random.randint(0, 1):
        ans = iterative(center, radius)
    else:
        ans = recursive(center, radius, len(center))

    ans = len(ans)

    print(ans)

main()
