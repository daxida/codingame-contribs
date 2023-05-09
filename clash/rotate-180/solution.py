
s = input()
l = len(s) // 2

s = s.translate(s.maketrans("bqunpd", "qbnudp"))

s = list(s)
for i in range(l):
    s[i], s[2*l-i-1] = s[2*l-i-1], s[i]

print(''.join(s))
