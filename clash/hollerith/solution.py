# Reference: https://en.wikipedia.org/wiki/Hollerith_constant

n = int(input())
s = input()
l = len(s)

ans = []
for i in range(0, l, n):
    chunk = ""
    j = 0
    while j < n and i + j < l:
        chunk += s[i + j]
        j += 1
    ans.append(f"{j}H" + chunk)

print("/" + ",".join(ans) + "/")