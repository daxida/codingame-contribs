def is_prime(n):
    return n >= 2 and all(n % i for i in range(2, int(n ** 0.5) + 1))

def is_gauss_prime(a, b):
    if a == 0:
        return b % 4 == 3 and is_prime(b)
    elif b == 0:
        return a % 4 == 3 and is_prime(a)
    return is_prime(a**2 + b**2)

n = int(input())
for i in range(n):
    x, y = map(int, input().split())
    ans = is_gauss_prime(x, y)
    print(str(ans).lower())
