import random
import string


def generate(n, m):
    '''
    Yields strings a and b such that b appears as b subsequence of a
    once and only once.
    '''
    s = string.ascii_lowercase
    a = ""
    b = ""

    for _ in range(n):
        b += random.choice(s)

    # start
    for _ in range(random.randrange(0, m)):
        c = random.choice(s)
        if c != b[0]:
            a += c

    # center
    for i in range(n - 1):
        a += b[i]
        for _ in range(random.randrange(0, m)):
            c = random.choice(s)
            # Ensure that in ...c1...c...c2.. c1 != c and c2 != c
            # otherwise the subsequence is not unique
            if c != b[i] and c != b[i+1]:
                a += c
    a += b[-1]

    # end
    for _ in range(random.randrange(0, m)):
        c = random.choice(s)
        if c != b[-1]:
            a += c

    return a, b


def solve(a, b):
    ans = ""
    i, j = 0, 0
    prev = None

    while i < len(a) and j < len(b):
        if a[i] == b[j]:
            ans += a[i]
            j += 1
            prev = a[i]
        elif prev == a[i]:
            print("Subsequence is not unique")
        else:
            ans += '-'
        i += 1

    for k in range(i, len(a)):
        ans += "-"
        if prev == a[k]:
            print("Subsequence is not unique")

    return ans


def count_subsequence(S, T):
    # Counts how many times S appears as a subsequence of T
    # https://www.geeksforgeeks.org/count-distinct-occurrences-as-a-subsequence/amp/
    m = len(T)
    n = len(S)

    if m > n:
        return 0

    mat = [[0 for _ in range(n + 1)] for __ in range(m + 1)]

    for i in range(1, m + 1):
        mat[i][0] = 0

    for j in range(n + 1):
        mat[0][j] = 1

    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if T[i - 1] != S[j - 1]:
                mat[i][j] = mat[i][j - 1]
            else:
                mat[i][j] = (mat[i][j - 1] + mat[i - 1][j - 1])

    return mat[m][n]


def main():
    n = 30  # size of the bigger string b
    m = 2  # random interval for the creation of a

    a, b = generate(n, m)
    print(a)
    print(b)
    print(solve(a, b))

    # Paranoia check: must be 1
    assert count_subsequence(a, b) == 1


if __name__ == '__main__':
    main()
