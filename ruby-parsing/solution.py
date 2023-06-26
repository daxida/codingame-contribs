import sys
import re

# Sadly atm you dont even need to parse the keywords with this method

INPUT = 0

if INPUT:
    sys.stdin = open('in.txt', 'r')


def E(*args): print(*args, file=sys.stderr, flush=True)


n = int(input())
code = [input() for _ in range(n)]
jcode = ' '.join(code)

import re
conds = re.findall(r'(?<=\()[^)]*(?=\))', jcode)
conds = [eval(x.capitalize()) for x in conds]
E(conds)
keywords = re.findall(r'(?<=\.).*?(?=\?)', jcode)
E(keywords)

results = [x[0] for x in re.findall(r'((do|{).*?(end|}))', jcode)]
results = [x.strip('do').strip('end') for x in results]
results = [x.strip('{').strip('}') for x in results]
results = [x.strip() for x in results]
results = [x.strip('puts ') for x in results]
results = [x.strip('\'') for x in results]
E(results)

ans = "none"
from itertools import zip_longest
for cond, kw, res in zip_longest(conds, keywords, results, fillvalue=True):
    E(cond, kw, res)
    if cond:
        ans = res
        break

print(ans)
