# https://www.cut-the-knot.org/arithmetic/int_domain4.shtml

from math import ceil, floor


def closest(n: float) -> int:
    c = ceil(n)
    return c if abs(n - c) <= 1 / 2 else floor(n)


def euclid(z1: complex, z2: complex):
    z = z1 / z2
    q = complex(closest(z.real), closest(z.imag))
    r = z1 - z2 * q
    return q, r


def gcd(z1: complex, z2: complex):
    q, r = euclid(z1, z2)
    print(f"{z1} = {z2} * {q} + {r}")
    return z2 if r == 0 else gcd(z2, r)


def main():
    z1 = complex(*map(int, input().split()))
    z2 = complex(*map(int, input().split()))
    g = gcd(z1, z2)
    print(f"GCD({z1}, {z2}) = {g}")


main()
