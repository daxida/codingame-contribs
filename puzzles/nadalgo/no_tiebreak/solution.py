import math
import numpy as np
from collections import defaultdict

# What is the probablity of winning a best-of-'n' match of tennis
# if the probability of winning a point is 'p'

# Notes: point < game < set < match


def markov(n: int, p: float) -> float:
    # n = number of points to win a game (or nb of games to win a set)
    # p = probability of winning

    # https://idpoisson.fr/berglund/probamass_html/node18.html

    # Building of the transition matrix P
    simple_states = [(x, y) for x in range(n) for y in range(n)]
    # Easier manipulations, doesn't fit the next for loop
    simple_states.remove((n-1, n-1))

    # Graph of states (adjacency form)
    g = defaultdict(dict)
    for sx, sy in simple_states:
        g[sx, sy][sx + 1, sy] = p
        g[sx, sy][sx, sy + 1] = 1 - p

    for i in range(n-1):
        g[n, i][n, i] = 1
        g[i, n][i, n] = 1

    g[(n-1, n-1)][(n-1, n-2)] = p
    g[(n-1, n-1)][(n-2, n-1)] = 1 - p

    def cmp(elt):
        return (1,) + elt if n in elt else (0,) + elt

    # All states.
    # Absorbent are placed last for easier finding of the canonical form.
    states = list(g.keys())
    states.sort(key=cmp)

    amt_states = len(states)
    P = [[0] * amt_states for _ in range(amt_states)]  # transition matrix
    for i, s1 in enumerate(states):
        for s2, prob in g[s1].items():
            j = states.index(s2)
            P[i][j] = prob
    P = np.matrix(P)

    # Computations to find the matrix B (cf. link, ex. 3.4.7)

    # Amount of non absorbant states
    amt_na_states = amt_states - 2 * (n-1)

    Q = P[:amt_na_states, :amt_na_states]
    R = P[:amt_na_states, amt_na_states:]
    N = np.linalg.inv(np.identity(Q.shape[0]) - Q)
    B = N * R

    # Indices of absorbant states that result in a win
    good_abs_idx = [states.index((n, i)) - amt_na_states for i in range(n-1)]

    p_ans = sum(B[0, i] for i in good_abs_idx)

    return p_ans


def pnbinom(q, size, prob):
    # density
    coef = math.comb(q + size - 1, size - 1)
    return coef * prob ** size * (1 - prob) ** q


def win(w, k):
    # cumsum (distribution)
    return sum(pnbinom(i, k, w) for i in range(k))


def main():
    p_point = float(input())
    n = int(input())

    p_game = markov(n=4, p=p_point)
    p_set = markov(n=6, p=p_game)
    p_match_best_of_n = win(p_set, (n + 1) // 2)

    print(round(p_match_best_of_n, 4))


main()