import random

n = 30


def generate(n, obstacles=False, to_print=False):
    # Generate the costs of the distance
    penalties = [2]
    for i in range(1, n - 1):
        r = penalties[i - 1] + random.randint(4, 5) - i // 10
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
    if obstacles:
        n_obstacles = random.randint(0, n // 10)
        size_obstacle = n // 5
        for _ in range(n_obstacles):
            y = random.randint(0, n - 1)
            x = random.randint(0, n - 1)
            for dy in range(size_obstacle):
                for dx in range(size_obstacle):
                    ny = y + dy
                    nx = x + dx
                    if 0 <= ny < n and 0 <= nx < n:
                        grid[ny][nx] = "9"

        n_random_rows = random.randint(0, n // 10)
        for _ in range(n_random_rows):
            y = random.randint(0, n - 1)
            grid[y] = ["9"] * n
        n_random_lines = random.randint(0, n // 10)
        for _ in range(n_random_rows):
            x = random.randint(0, n - 1)
            for y in range(n):
                grid[y][x] = "9"

    if to_print:
        print(n)
        print(' '.join(str(p) for p in penalties))
        for row in grid:
            print(''.join(row))
    
    return n, penalties, grid


# generate(n, obstacles=True, to_print=True)
