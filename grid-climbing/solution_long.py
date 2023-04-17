# Thanks to @cedricdd for his tips to improve my previous solution

from functools import wraps
from time import time
import sys
import heapq

INF = float("inf")


def E(*a): print(*a, file=sys.stderr)


def timing(f):
    @wraps(f)
    def wrap(*args, **kw):
        ts = time()
        result = f(*args, **kw)
        te = time()
        print(f'Took: {te-ts:2.4f}sec\n', file=sys.stderr, flush=True)
        return result
    return wrap


def dist(p1, p2):
    return max(abs(p1[1] - p2[1]), abs(p1[0] - p2[0]))


@timing
def dijkstra(start, end, grid, costs, n):
    solutions = []

    memo = {point: INF for point in grid}
    h = [(grid[start], start, [start])]
    while h:
        cost, cur, path = heapq.heappop(h)
        if cur == end:
            solutions.append(path)

        for p in grid:
            d = dist(p, cur)
            ncost = cost + costs[d-1] + grid[p]
            if memo[p] > ncost:
                memo[p] = ncost
                nnode = (ncost, p, path + [p])
                heapq.heappush(h, nnode)

    print(memo[end])

    # Visualization
    s = solutions[0]
    for y in range(n):
        row = ""
        for x in range(n):
            row += "X" if (y, x) in s else "."
        E(row)


def main():
    n = int(input())
    costs = list(map(int, input().split()))
    g = dict()
    for y in range(n):
        for x, c in enumerate(input()):
            g[(y, x)] = int(c)

    start = (0, 0)
    end = (n - 1, n - 1)

    st = time()
    dijkstra(start, end, g, costs, n)
    ellapsed = time() - st
    if ellapsed > 1.5:
        print(f"Timeout: {ellapsed}")


main()


''' Generator logic

def generate(n):
    import random

    # Generate the costs of the distance
    penalties = [2]
    for i in range(1, n - 1):
        r = penalties[i - 1] + random.randint(4, 5)
        penalties.append(r)

    # Generates a grid "Higher at the center"
    center = (n - 1) // 2
    grid = []
    for y in range(n):
        row = []
        for x in range(n):
            value = random.randint(0, 9)
            value -= max(abs(y - center), abs(x - center)) // 2 
            value = max(0, value)
            row.append(str(value))
        grid.append(row)

    # Adds random obstacles
    n_obstacles = random.randint(0, 3)
    size_obstacle = random.randint(0, 10)
    for _ in range(n_obstacles):
        y = random.randint(0, n)
        x = random.randint(0, n)
        for dy in range(size_obstacle):
            for dx in range(size_obstacle):
                ny = y + dy
                nx = x + dx
                if 0 <= ny < n and 0 <= nx < n:
                    grid[ny][nx] = "9"

    print(n)
    print(' '.join(str(p) for p in penalties))
    for row in grid:
        print(''.join(row))

    return n, penalties, grid
'''
