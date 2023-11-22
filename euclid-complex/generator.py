import random


def random_gauss_int(n):
    y = random.randint(-n, n)
    x = random.randint(-n, n)

    return complex(x, y)

n = 1000
z1 = random_gauss_int(n)
z2 = random_gauss_int(n)
print(int(z1.real), int(z1.imag))
print(int(z2.real), int(z2.imag))
