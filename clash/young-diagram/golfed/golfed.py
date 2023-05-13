# 93 @j4at

p=*map(int,input().split("+")),
print(*[sum(e>i for e in p)for i in range(max(p))],sep=" + ")