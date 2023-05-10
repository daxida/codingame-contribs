# Thanks to @cedricdd for his tips to improve my previous solution

# Old solution, works up to around N = 10

import sys
import time
from collections import deque
from itertools import product

sys.stdin = open('input.txt', 'r')


def E(*args): print(*args, file=sys.stderr)


class Path:
    def __init__(self, last_point, path, cost):
        self.last_point = last_point
        self.path = path
        self.cost = cost

    def print_path(self, grid, n):
        ascii_grid = [[''] * n for _ in range(n)]
        for y in range(n):
            for x in range(n):
                ascii_grid[y][x] = 'X' if (y, x) in self.path else '.'

        for row in ascii_grid:
            E("".join(row))


def get_distance(p1, p2):
    return max(abs(p1[1]-p2[1]), abs(p1[0]-p2[0]))


def get_cost(y, x, yy, xx, grid, costs_table):
    return grid[yy][xx] + costs_table[get_distance((y, x), (yy, xx)) - 1]


def precompute_costs(n, grid, costs_table):
    costs = {}
    for y, x, yy, xx in product(range(n), repeat=4):
        if (y, x) != (yy, xx):
            costs[((y, x), (yy, xx))] = get_cost(y, x, yy, xx, grid, costs_table)

    return costs


def bfs(n, grid, start, end, costs):
    solutions = []
    path = Path((0, 0), [(0, 0)], grid[0][0])
    queue = deque([path])

    # Upper bound of going straight to the end
    upper_bound = grid[0][0] + costs[(start, end)]
    memo = [[upper_bound] * n for _ in range(n)]

    while queue:
        path = queue.popleft()

        for v in range(n*n):
            vy, vx = divmod(v, n)
            if (vy, vx) == path.last_point:
                continue

            new_cost = path.cost + costs[(path.last_point, (vy, vx))]

            if new_cost > memo[vy][vx]:
                continue
            memo[vy][vx] = new_cost

            if new_cost > upper_bound:
                continue

            new_path = Path((vy, vx), path.path + [(vy, vx)], new_cost)

            if (vy, vx) == end:
                upper_bound = new_cost
                solutions.append(new_path)

            queue.append(new_path)

    # Also just equal to memo[n-1][n-1]. Done this way to see the path.
    min_cost = min(s.cost for s in solutions)
    for s in solutions:
        if s.cost == min_cost:
            s.print_path(grid, n)
            break

    return memo[n-1][n-1]


def solve(n, costs_table, grid):
    start, end = (0, 0), (n-1, n-1)

    st = time.time()
    costs = precompute_costs(n, grid, costs_table)
    print(f"Precomputing costs finished in {time.time()-st}", file=sys.stderr)

    st = time.time()
    print(bfs(n, grid, start, end, costs))
    print(f"BFS finished in {time.time()-st}", file=sys.stderr)


def main():
    n = int(input())
    costs_table = list(map(int, input().split()))
    grid = [list(map(int, input())) for _ in range(n)]

    solve(n, costs_table, grid)


if __name__ == '__main__':
    main()
