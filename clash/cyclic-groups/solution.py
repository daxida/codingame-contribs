
n = int(input())
c = input()
s = input().split()

s[0] = ""
s.sort()
g = [c * i for i in range(n)]

if s == g:
    print(n)
else:
    print(*[x for x in g if x not in s])
