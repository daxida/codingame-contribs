import random

DIRS = [(-1, 0), (0, 1), (0, -1), (1, 0)]

step = int(input())
w, h = [int(i) for i in input().split()]
g    = [input() for _ in range(h)]
start = [(x, y) for y in range(h) for x in range(w) if g[y][x] == "A"]


def montecarlo(start):
    iters = 2 * 10 ** 5
    time = 0
    for _ in range(iters):
        x, y = start[0]
        while 0 < y < h - 1 and 0 < x < w - 1:
            dy, dx = DIRS[random.randint(0, 3)]
            y += dy * step
            x += dx * step
            time += 1

    return time / iters


ans = montecarlo(start)
print(f'{ans:.1f}')
