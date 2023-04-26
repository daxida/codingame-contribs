# Idea

n = int(input())
c = input()
s = input().split()

print(*[c * i for i in range(1, n) if c * i not in s] or [n])


# Succint

I=input
n=int(I())
c=I()
s=I().split()
I(" ".join(c*i for i in range(1,n)if c*i not in s)or n)
