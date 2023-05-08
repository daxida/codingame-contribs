# 68 by @j4at

a,b=open(0)
for c in a[:-1]:print(end=["-",c][b!=(b:=b[c==b[:1]:])])

# Explanation

a, b = open(0)
for c in a[:-1]:
    same_char = c == b[:1]   # 0 or 1
    nxt_b = b[same_char:]    # we advance 1 pos or not
    did_advance = b != nxt_b # to know what to print
    print(end = c if did_advance else "-")
    b = nxt_b


# 80

a,b=open(j:=0)
for i in a[:-1]:c=i==b[j];print(end=["-",i][c]);j+=c;j-=j==len(b)

a, b = open(j:=0)
for i in a[:-1]:
    c = i == b[j]
    print(end = i if c else "-")
    if j < len(b) - 1:
        j += c