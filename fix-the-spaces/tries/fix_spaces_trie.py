from collections import Counter
import sys
import time

def E(*a): print(*a, file=sys.stderr)

class TrieNode:
    def __init__(self):
        self.children = dict()
        self.is_end = False # determines if it contains the word or not

class Trie:
    def __init__(self):
        self.root = TrieNode()

    def add(self, word):
        node = self.root
        for char in word:
            if char not in node.children:
                node.children[char] = TrieNode()
            node = node.children[char]

        node.is_end = True
    
    def find(self, word, node):
        # E("S", "".join(word))
        # E("T", node.is_end, node.children.keys())
        # E()

        # Reach the end -> found a word
        # But since this word can be the prefix of another word
        # we don't stop the search here
        if node.is_end:
            yield ''

        # Here we do stop the search since we reached the end
        if not word: 
            return
        
        char, *suffix = word # get the current char
        if char in node.children: # If we haven't found a word
            ch = node.children[char] # we go to the next char
            for sol in self.find(suffix, ch): # and we keep iterating
                yield char + sol # finally yielding the word that we are looking for
        
        
def main():
    original, words = input(), Counter(input().split())

    st = time.time()

    trie = Trie()
    for w in words:
        trie.add(w)

    def solve(remaining):
        if not remaining: 
            yield []
        for s in trie.find(remaining, trie.root):
            for ans in solve(remaining[len(s):]):
                yield [s] + ans

    poss = (ans for ans in solve(original) if Counter(ans) == words)
    try:
        ans, = poss
        print(*ans)
    except ValueError:
        print('Unsolvable')
        quit(1)
    finally:
        E(time.time() - st)

main()
