a = input()
b = input()
ans = ""
i, j = 0, 0

while i < len(a) and j < len(b):
    if a[i] == b[j]:
        ans += a[i]
        j += 1
    else:
        ans += '-'
    i += 1

print(ans.ljust(len(a), "-"))
