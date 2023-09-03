import numpy as np

DIRS = [(-1, 0), (0, 1), (0, -1), (1, 0)]


def solve(start, step, w, h):
    states = generateStates(step, w, h)
    matrix = transitionMarkov(step, states)

    matrix_size = matrix.shape[0]
    row_start_point = list(states.keys()).index(start)
    inv = np.linalg.inv(np.identity(matrix_size) - matrix).round(10)

    expectancy = round(sum(i for i in inv[row_start_point]), 1)

    return expectancy


def generateStates(step, w, h):
    states = {}

    for y in range(1, h):
        for x in range(1, w):
            future_states = []
            for dy, dx in DIRS:
                s = (x + dx * step, y + dy * step)
                if abs(s[0]) < w - 1 and abs(s[1]) < h - 1:
                    future_states.append(s)
            states.setdefault((x, y), future_states)

    return states


def transitionMarkov(step, states):
    matrix_points = list(states.keys())

    matrix = []
    for point1 in matrix_points:
        row = []
        for point2 in matrix_points:
            row.append(1 / 4 if point2 in states[point1] else 0)
        matrix.append(row)

    return np.matrix(matrix)


def main():
    step = int(input())
    w, h = [int(i) for i in input().split()]
    g = [input() for _ in range(h)]
    start = [(x, y) for y in range(h) for x in range(w) if g[y][x] == "A"]
    expectancy = solve(start[0], step, w, h)

    print(expectancy)


if __name__ == '__main__':
    main()
