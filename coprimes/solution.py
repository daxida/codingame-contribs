import itertools
import math

# Solves the problem for up to N = 15 in less than 1 second.
# This is an implementation with minimal dependencies to solve
# the problem in the codingame I/O format.


def are_coprime(n, m):
    return math.gcd(n, m) == 1


def is_valid_solution(xs):
    for i in range(len(xs) - 1):
        if not are_coprime(xs[i + 1], xs[i]):
            return False
    return True


def held_karp(xs):
    # https://en.wikipedia.org/wiki/Hamiltonian_path_problem
    # https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm

    # Implementation of the main algorithm:
    # https://github.com/CarlEkerot/held-karp/blob/master/held-karp.py

    # The whole approach is to find a Hamiltonean path in the
    # coprime graph associated to the sequence xs.
    # By definition the coprime graph is:
    #    vertices: integers xs
    #    edges: edge between u and v iif u and v are coprime

    # F.ex.
    # xs = [1, 2, 4]
    #
    # G =   1
    #      / \
    #     /   \
    #    2     4
    #
    # Therefore finding the required permutation is equivalent
    # to finding the lowest lexicographically Hamiltonean path in 
    # the coprime graph.

    # What we are going to do is to transform this problem again
    # into a special case of the TSP, then solve that one with xs
    # sorted to ensure that the solution is minimal lexicographically. 

    # Note that this solution doesn't use any interesting
    # property of primes, and that another purely arithmetic solution
    # is not excluded.

    # Makes it easier at the end to build the solution from indexes
    xs.sort()

    n = len(xs)

    # We add a new universal vertex (vertex connected to every
    # other vertex with distance 0) to transform the problem from
    # finding a Hamiltonean path to a Hamiltonean cycle.

    # Then we apply this strategy (wikipedia):
    # The Hamiltonian cycle problem is a special case of the
    # travelling salesman problem, obtained by setting the distance
    # between two cities to one if they are adjacent and two otherwise,
    # and verifying that the total distance travelled is equal to n
    # (if so, the route is a Hamiltonian circuit; if there is no Hamiltonian
    # circuit then the shortest route will be longer).
    n += 1
    adj = [[0] * n for _ in range(n)]
    for i, x in enumerate(xs):
        for j, y in enumerate(xs):
            adj[i + 1][j + 1] = 1 if are_coprime(x, y) else 2
            if i == j:
                adj[i + 1][j + 1] = 0

    # From here on up to the last 5 lines it's a copy paste of
    # https://github.com/CarlEkerot/held-karp/blob/master/held-karp.py

    C = {}
    # Set transition cost from initial state
    for k in range(1, n):
        C[(1 << k, k)] = (adj[0][k], 0)

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
                    res.append((C[(prev, m)][0] + adj[m][k], m))
                C[(bits, k)] = min(res)

    # We're interested in all bits but the least significant (the start state)
    bits = (2**n - 1) - 1

    # Calculate optimal cost
    res = []
    for k in range(1, n):
        res.append((C[(bits, k)][0] + adj[k][0], k))
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

    ans = [xs[i - 1] for i in path if i != 0]

    # We don't know if the solution is actually valid
    if is_valid_solution(ans):
        print(*ans)
    else:
        print(-1)


n = int(input())
xs = list(map(int, input().split()))
held_karp(xs)
