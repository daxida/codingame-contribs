# <<source:>> https://atcoder.jp/contests/abc258/tasks/abc258_b

n = int(input())
grid = [[*map(int, list(input()))] for _ in range(n)]
D = {'N':(-1,0), 'S':(1,0), 'E':(0,1), 'W':(0,-1)}
values = []

for y in range(n):
    for x in range(n):
        for direction in 'NSEW':
            value = str(grid[y][x])
            d = D[direction]
            dy, dx = d            
            for _ in range(n-1): # n-1 steps
                ny = (y + dy) % n
                nx = (x + dx) % n
                value += str(grid[ny][nx])
                y = ny
                x = nx
            values.append(int(value))

print(max(values))