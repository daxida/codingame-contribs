from collections import defaultdict
import math


def is_solvable(g: dict[list], n: int):
    '''
    Contains both Dirac's and Ore's criterium for finding
    a hamiltonean path together in the function is_solvable

    https://en.wikipedia.org/wiki/Hamiltonian_path#Bondy%E2%80%93Chv%C3%A1tal_theorem
    '''

    def dirac(g: dict[list], n: int):
        # Dirac is just a necesarry condition. It may return false
        # when there is a hamiltonean path (cf xs = [1, 2, 4])

        for node in g:
            degree = len(g[node])
            if degree < n / 2:
                return False

        return True

    def ore(g: dict[list], n: int):
        for node_1 in g:
            degree_1 = len(g[node_1])
            for node_2 in g:
                if node_1 == node_2:
                    continue
                if node_2 in g[node_1]:
                    continue
                degree_2 = len(g[node_2])

                if degree_1 + degree_2 < n:
                    return False

        return True

    solvable_1 = dirac(g, n)
    solvable_2 = ore(g, n)

    print("dirac criterium", solvable_1)
    print("ore criterium  ", solvable_2)
    print()


def are_coprime(n, m):
    return math.gcd(n, m) == 1


def sieve(n):
    primes = [True for i in range(n + 1)]
    primes[0] = False
    primes[1] = False

    p = 2
    while p * p <= n:
        if primes[p]:
            for i in range(p * p, n + 1, p):
                primes[i] = False
        p += 1

    return primes


def get_primes_iter(n):
    _sieve = sieve(n)
    primes = []
    for i in range(2, n):
        if _sieve[i]:
            primes.append(i)

    return iter(primes)


def get_prime_factors(n):
    ans = defaultdict(int)
    d = 2
    while d <= n:
        while n % d == 0:
            ans[d] += 1
            n //= d
        d += 1

    ans = [k for k, v in ans.items()]
    return ans
