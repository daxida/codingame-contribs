# 97

I=input
n=int(I())
s=I()
I("/"+','.join(f"{len(c:=s[i:i+n])}H"+c for i in range(0,len(s),n))+"/")

# 106

I=input
n=int(I())
s=I()
k=[]
i=0
while i<len(s):k+=f"{len(c:=s[i:i+n])}H{c}",;i+=n
I("/"+",".join(k)+"/")