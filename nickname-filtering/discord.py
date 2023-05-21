import string
import random
from functools import wraps
from time import time
import sys
from collections import defaultdict, deque


def E(*a): print(*a, file=sys.stderr)


def timing(f):
    @wraps(f)
    def wrap(*args, **kw):
        ts = time()
        result = f(*args, **kw)
        te = time()
        print(f'Took: {te-ts:2.4f}sec\n', file=sys.stderr, flush=True)
        return result
    return wrap


class TrieNode:
    def __init__(self, char):
        self.char = char
        self.is_end = False
        self.children = {}


class Trie(object):
    def __init__(self):
        self.root = TrieNode("")

    def insert(self, word, subs):
        node = self.root
        q = deque([node])

        for i, char in enumerate(word):
            nxt = deque()
            variations = subs.get(char, char)

            while q:
                node = q.popleft()
                # E(q, char, node.char)

                for var in variations:
                    if var in node.children:
                        new_node = node.children[var]
                    else:
                        new_node = TrieNode(var)
                        node.children[var] = new_node
                    nxt.append(new_node)
            q = nxt

            if len(word) - 1 == i:
                for node in q:
                    node.is_end = True

    def query(self, x):
        node = self.root

        for char in x:
            if not char in node.children:
                return False
            node = node.children[char]

        return node.is_end


def generate():
    CHOICES = [chr(i) for i in range(33, 126) if chr(i) not in string.ascii_lowercase + string.ascii_uppercase]


    n_bans = 20
    size_bans = 2
    n_pseudos = 100
    size_pseudos = 5

    def random_letters(n):
        return random.sample([chr(i) for i in range(33, 126)], n)

    bans = []
    for _ in range(n_bans):
        w = "".join(random_letters(size_bans + random.randint(0, 2)))
        bans.append(w)
    # E(bans)

    subs = dict()
    half = 1
    for c in string.ascii_lowercase:
        if half and random.randint(0, 1) < 1:
            continue
        amt_subs = 2
        cur_subs = []
        if len(CHOICES) < amt_subs:
            continue
        for _ in range(amt_subs):
            letter = random.choice(CHOICES)
            CHOICES.remove(letter)
            cur_subs.append(letter)

        cur_subs.append(c.upper())
        subs[c] = cur_subs
    # E(subs)

    pseudos = []
    for _ in range(n_pseudos):
        pseudo = "".join(random_letters(size_pseudos + random.randint(0, 3)))
        pseudos.append(pseudo)
    # E(pseudos)

    f = 1
    if f:
        n_subs = len(subs)
        print(n_bans, n_subs, n_pseudos)
        for i in range(n_bans):
            print(bans[i])
        cnt = 0
        for k, v in subs.items():
            print(k, " ".join(v))
            cnt += 1
        assert cnt == n_subs
        for i in range(n_pseudos):
            print(pseudos[i])

    return subs, bans, pseudos


@timing
def main():
    rdm = False
    ipt = True

    if ipt:
        n_bans, n_substitutions, n_nicknames = map(int, input().split())
        bans = [input() for _ in range(n_bans)]
        max_len_ban = max([len(ban) for ban in bans])
        _subs = dict()
        for _ in range(n_substitutions):
            _sub = input().split()
            _subs[_sub[0]] = _sub[1:]
        pseudos = [input() for _ in range(n_nicknames)]
        max_len_pseudo = max([len(pseudo) for pseudo in pseudos])

        E("Max size of a banned word:", max_len_ban)
        E("Max size of a nickname:", max_len_pseudo)

    elif rdm:
        _subs, bans, pseudos = generate()
    else:
        _subs = {"o": ["0", "@"]}
        bans = ["mo"]  # ["mofo"]
        pseudos = ["m", "mof", "m0fo", "mafa", "mofor", "mo", "4mo", "4m@fo"]

    subs = defaultdict(list)
    for k, v in _subs.items():
        every = [k] + v
        # every = list(set(every))
        for char in every:
            subs[char].extend(every)
    # E(subs)

    trie = Trie()
    for w in bans:
        trie.insert(w, subs)

    def check(w):
        for i in range(len(w)):
            for j in range(i + 1, len(w) + 1):
                if trie.query(w[i:j]):
                    return True
        return False

    censured = 0
    for w in pseudos:
        if check(w):
            censured += 1

    print(censured)


main()
