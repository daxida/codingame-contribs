from collections import Counter
import sys
import time

def E(*a): print(*a, file=sys.stderr)

class Trie:
    def __init__(self):
        self.children: dict[str, Trie] = {}
        self.contains = False

    def add(self, s):
        if s:
            self.children.setdefault(s[0], Trie()).add(s[1:]) 
        else:
            self.contains = True

    def find(self, s):
        if self.contains:
            yield ''
        if not s: return
        
        c, *s = s
        ch = self.children.get(c)
        if ch:
            for sol in ch.find(s):
                yield c + sol


def main():
    original, words = input(), Counter(input().split())

    st = time.time()

    root = Trie()
    for w in words:
        root.add(w)

    def solve(remaining):
        if not remaining: yield []
        for s in root.find(remaining):
            for ans in solve(remaining[len(s):]):
                yield [s] + ans

    # Checking Counter here feels like a kludge, 
    # but I didn't notice that rule until the very end...
    poss = (ans for ans in solve(original) if Counter(ans) == words)
    
    # try:
    #     ans, = poss
    #     print(*ans)
    # except ValueError:
    #     print('Unsolvable')
    #     quit(1)
    # finally:
    #     E(time.time() - st)

    ans, *rest = poss
    print(*ans if not rest else ["Unsolvable"])
    E(time.time() - st)

main()
