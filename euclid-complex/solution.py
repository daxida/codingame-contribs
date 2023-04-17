# https://www.cut-the-knot.org/arithmetic/int_domain4.shtml

import math


def print_complex(z: complex):
    print(f"{z.real:.0f} {z.imag:.0f}")


def closest(n: float) -> int:
    c = math.ceil(n)
    f = math.floor(n)

    return c if abs(n - c) <= 1 / 2 else f


def euclid(z1: complex, z2: complex):
    z = z1 / z2

    x = z.real
    y = z.imag

    # Rounding should not work
    _r = closest(x)
    _s = closest(y)

    q = complex(_r, _s)
    r = z1 - z2 * q

    assert z1 == z2 * q + r
    assert abs(r) < abs(z2)  # Complex norm here

    return q, r


def gcd(z1: complex, z2: complex):
    # Test n3 -> Don't sort.
    # if abs(z1) < abs(z2):
    #     z2, z1 = z1, z2

    q, r = euclid(z1, z2)
    print(f"{z1} = {z2} * {q} + {r}")

    return z2 if r == 0 else gcd(z2, r)


def main():
    z1 = complex(*map(int, input().split()))
    z2 = complex(*map(int, input().split()))

    g = gcd(z1, z2)

    print(f"GCD({z1}, {z2}) = {g}")


main()
