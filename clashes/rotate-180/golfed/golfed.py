# 63

print(input().translate("".maketrans("bqunpd","qbnudp"))[::-1])

# 65

a="bupdnq"
print(input().translate(a.maketrans(a,a[::-1]))[::-1])

# 65

a="bupoxzzxodnq"
[print(end=a[~a.find(c)])for c in input()[::-1]]