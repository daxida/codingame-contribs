import string

alphabet = string.ascii_lowercase

def dist(chr_a, chr_b):
    return abs(ord(chr_a) - ord(chr_b))

def recursive(center, radius, idx):
    if idx == 0:
        return 1

    solutions = 0
    c1 = center[-idx]

    for c2 in alphabet:
        if (d := dist(c1, c2)) <= radius:
            solutions += recursive(center, radius - d, idx - 1)

    return solutions

def main():
    radius = int(input())
    center = input()

    ans = recursive(center, radius, len(center))
    print(ans)

main()
