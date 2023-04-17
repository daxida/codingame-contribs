import heapq


def dist(p1, p2):
    return max(abs(p1[1] - p2[1]), abs(p1[0] - p2[0]))


def dijkstra(start, end, grid, costs):
    memo = {point: float("inf") for point in grid}
    h = [(grid[start], start, [start])]
    while h:
        cost, cur, path = heapq.heappop(h)

        for p in grid:
            d = dist(p, cur)
            ncost = cost + costs[d - 1] + grid[p]
            if memo[p] > ncost:
                memo[p] = ncost
                nnode = (ncost, p, path + [p])
                heapq.heappush(h, nnode)

    print(memo[end])


def main():
    n = int(input())
    costs = list(map(int, input().split()))
    g = dict()
    for y in range(n):
        for x, c in enumerate(input()):
            g[(y, x)] = int(c)

    start = (0, 0)
    end = (n - 1, n - 1)

    dijkstra(start, end, g, costs)


main()
