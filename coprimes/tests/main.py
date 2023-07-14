from random import randint, shuffle
import math
import itertools
from collections import defaultdict, deque
from functools import wraps
from time import time
from utils import are_coprime, is_solvable

'''
Coprime graph: graph from a sequence xs such that the vertices
are the elements of xs, and a and b are connected iif a and b are coprimes

--> Find a permutation of xs such that consecutive members are coprime
'''


def test(f):
    @wraps(f)
    def wrap(*args, **kw):
        ts = time()
        result = f(*args, **kw)
        te = time()
        print(f.__name__)
        print(*result)
        print(f'Took: {te-ts:2.4f}sec\n')
        return result
    return wrap


def is_valid_solution(xs):
    for i in range(len(xs) - 1):
        if not are_coprime(xs[i + 1], xs[i]):
            return False
    return True


def build_graph(xs):
    g = defaultdict(list)
    for i, x in enumerate(xs):
        for y in xs[i + 1:]:
            if are_coprime(x, y):
                g[x].append(y)
                g[y].append(x)

    return g


@test
def bruteforce(xs):
    # We sort so we can break as soon as we find a solution
    # taking advantage of the permutations' lexicographic order
    xs.sort()

    ans = [None]
    for p in itertools.permutations(xs):
        if is_valid_solution(p):
            ans = list(p)
            break

    return ans


@test
def bfs(xs):
    n = len(xs)

    # We need to sort for this solution to work so that
    # we try every posible start sorted in ascending order
    xs.sort()

    g = build_graph(xs)

    def _bfs():
        q = deque([(x, [x]) for x in xs])

        while q:
            u, path = q.popleft()
            if len(path) == n:
                return path
            for v in g[u]:
                if v not in path:
                    q.append((v, path + [v]))

        return [None]

    return _bfs()


@test
def held_karp(xs):
    # https://en.wikipedia.org/wiki/Hamiltonian_path_problem
    # https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm

    # Implementation
    # https://github.com/CarlEkerot/held-karp/blob/master/held-karp.py

    xs.sort()
    n = len(xs)

    n += 1
    INF = float('inf')

    # We add an extra point connected to every vertex with distance 0
    # to transform the problem from Hamiltonean path to cycle.
    adj = [[0] * n for _ in range(n)]
    for i, x in enumerate(xs):
        for j, y in enumerate(xs):
            adj[i + 1][j + 1] = 1 if are_coprime(x, y) else 2
            if i == j:
                adj[i + 1][j + 1] = 0

    dists = adj

    C = {}
    # Set transition cost from initial state
    for k in range(1, n):
        C[(1 << k, k)] = (dists[0][k], 0)

    # Iterate subsets of increasing length and store intermediate results
    # in classic dynamic programming manner
    for subset_size in range(2, n):
        for subset in itertools.combinations(range(1, n), subset_size):
            # Set bits for all nodes in this subset
            bits = 0
            for bit in subset:
                bits |= 1 << bit

            # Find the lowest cost to get to this subset
            for k in subset:
                prev = bits & ~(1 << k)

                res = []
                for m in subset:
                    if m == 0 or m == k:
                        continue
                    res.append((C[(prev, m)][0] + dists[m][k], m))
                C[(bits, k)] = min(res)

    # We're interested in all bits but the least significant (the start state)
    bits = (2**n - 1) - 1

    # Calculate optimal cost
    res = []
    for k in range(1, n):
        res.append((C[(bits, k)][0] + dists[k][0], k))
    opt, parent = min(res)

    # Backtrack to find full path
    path = []
    for i in range(n - 1):
        path.append(parent)
        new_bits = bits & ~(1 << parent)
        _, parent = C[(bits, parent)]
        bits = new_bits

    # Add implicit start state
    path.append(0)

    ans = path
    ans = [xs[i - 1] for i in ans if i != 0]

    # We don't know if the solution is actually valid
    if not is_valid_solution(ans):
        ans = [None]

    return ans


@test
def greedy(xs):
    '''
    Tests existence.

    - If this function returns None, we can not say anything about the existence 
    or non existence of a solution for the given sequence.
    - If it returns a path, it is a permutation such that adjacent numbers 
    are coprime (not necessary the minimal lexicographically), so the existence
    of a solution to the main problem is guaranteed.

    On average it yields None when there is a solution once every 100 random tests

    The idea is to alternatively switch between the less connected node and the 
    more connected. Note that the connexions need to be updated at each iteration.
    '''

    N = len(xs)
    xs.sort()

    sol = []
    flag = 1  # 1 for max, 0 for min
    prev = 1
    while xs:
        n = len(xs)

        # We want to sort the nodes by their degree
        # The bigger the degree the closer we are to be a prime

        g = build_graph(xs)

        connexions_dict = {}
        for x in xs:
            connexions_dict[x] = len(g[x])

        xs.sort(key=lambda x: connexions_dict[x])

        # If flag:
        #   we take the first minimum allowed
        #   (minimum conections, node with more divisors)
        # else
        #   we take the first maximum allowed
        #   (maximum conections, node with less divisors)

        if flag:
            idx = 0
            while idx < n:
                # We compare with prev to ensure that it is a solution
                if are_coprime(xs[idx], prev):
                    break
                idx += 1
        else:
            idx = len(xs) - 1
            while idx >= 0:
                if are_coprime(xs[idx], prev):
                    break
                idx -= 1

        if idx >= n or idx < 0:
            break

        flag ^= 1
        choice = xs.pop(idx)
        sol.append(choice)
        prev = choice

    if len(sol) < N:
        sol = [None]

    return sol


def generate_xs(size, low, high, unique=True):
    assert high > low
    assert high - low >= size

    if unique:
        xs = set()
        while len(xs) < size:
            xs.add(randint(low, high))

        ans = list(xs)
    else:
        ans = [randint(low, high) for _ in range(size)]

    shuffle(ans)

    return ans


def main():
    cnt = 0

    amt_tests = 1
    # Set this to True to test every method.
    # Try it only with small values of size (<= 7)
    test_all = False

    while cnt < amt_tests:
        size = 15
        xs = generate_xs(size=size, low=300, high=700)

        is_solvable(build_graph(xs), size)

        print(f"N: {size}")
        print(f"Original sequence")
        print(*xs)
        print()
        
        if test_all:
            a = bruteforce(xs)
            b = bfs(xs)
        c = held_karp(xs)
        d = greedy(xs)

        # Testing different solving methods
        if test_all:
            assert a == b
            assert b == c

            # Testing greedy
            if any(x is None for x in (a, b, c)):
                assert d is None
            if d is None:
                assert all(x is None for x in (a, b, c))

        cnt += 1

    print("Number of tests:", cnt)


main()
