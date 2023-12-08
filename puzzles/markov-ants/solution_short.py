'''
https://en.wikipedia.org/wiki/Absorbing_Markov_chain
https://github.com/daxida/CodinGame-contributions/blob/main/markov-ants/Tran_presentation.pdf
(FRENCH) https://idpoisson.fr/berglund/probamass_html/node18.html
'''

from typing import List, Tuple, Dict
import numpy as np

DIRS = [(-1, 0), (0, 1), (0, -1), (1, 0)]

Point  = Tuple[int, int]
States = Dict[Point, List[Point]]
Matrix = np.ndarray


def solve(start: Point, step: int, w: int, h: int) -> float:
    states = generate_states(step, w, h)
    matrix_size = len(states)
    matrix = transition_markov(states, matrix_size)

    row_start_point = list(states).index(start)
    inv = np.linalg.inv(np.identity(matrix_size) - matrix).round(10)

    expectancy = sum(inv[row_start_point])

    return expectancy


def generate_states(step: int, w: int, h: int) -> States:
    states = dict()
    for y in range(1, h):
        for x in range(1, w):
            states[(x, y)] = list()
            for dy, dx in DIRS:
                state = (x + dx * step, y + dy * step)
                if abs(state[0]) < w - 1 and abs(state[1]) < h - 1:
                    states[(x, y)].append(state)

    return states


def transition_markov(states: States, matrix_size: int) -> Matrix:
    matrix = np.zeros((matrix_size, matrix_size))
    for idx1, pt1 in enumerate(states):
        for idx2, pt2 in enumerate(states):
            if pt2 in states[pt1]:
                matrix[idx1, idx2] = 1 / 4

    return matrix


def main():
    step = int(input())
    w, h = [int(i) for i in input().split()]
    g    = [input() for _ in range(h)]
    start = [(x, y) for y in range(h) for x in range(w) if g[y][x] == "A"]
    expectancy = solve(start[0], step, w, h)

    print(round(expectancy, 1))


main()
