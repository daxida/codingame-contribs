import random


def random_gauss_int(n):
    y = random.randint(-n, n)
    x = random.randint(-n, n)

    return complex(x, y)
