import math
import numpy as np
from collections import defaultdict
from typing import Tuple

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


def markov_set(n: int, p: float) -> Tuple[float, float]:
    # Building of the transition matrix P
    simple_states = [(x, y) for x in range(n) for y in range(n)]

    # Graph of states (adjacency form)
    g = defaultdict(dict)
    for sx, sy in simple_states:
        g[sx, sy][sx + 1, sy] = p
        g[sx, sy][sx, sy + 1] = 1 - p

    for i in range(n-1):
        g[n, i][n, i] = 1
        g[i, n][i, n] = 1
    # Tiebreak state
    g[n, n][n, n] = 1
    
    # Pre-tiebreak, either win or enter tiebreak
    g[n, n-1][n, n-2] = p       # win
    g[n, n-1][n, n]   = 1 - p   # tiebreak

    g[n-1, n][n-2, n] = 1 - p   # lose
    g[n-1, n][n, n]   = p       # tiebreak

    def cmp(elt):
        # Pre-tiebreak states should come first
        if elt == (n-1, n) or elt == (n, n-1):
            return (0,) + elt
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
    amt_na_states = amt_states - 2 * (n-1) - 1 # -1 due to extra TIEBREAK STATE

    Q = P[:amt_na_states, :amt_na_states]
    R = P[:amt_na_states, amt_na_states:]
    N = np.linalg.inv(np.identity(Q.shape[0]) - Q)
    B = N * R

    # Indices of absorbant states that result in a win
    good_abs_idx = [states.index((n, i)) - amt_na_states for i in range(n-1)]
    p_ans = sum(B[0, i] for i in good_abs_idx)

    _, w = B.shape
    idx_tiebreak = w - 1 # Last state
    p_reach_tiebreak = B[0, idx_tiebreak]

    return p_ans, p_reach_tiebreak


def pnbinom(q, size, prob):
    # density
    coef = math.comb(q + size - 1, size - 1)
    return coef * prob ** size * (1 - prob) ** q


def win(w, k):
    # cumsum (distribution)
    return sum(pnbinom(i, k, w) for i in range(k))


def main():
    INPUT = False
    DEBUG = True

    if INPUT:
        p_point = float(input())
        n = int(input())
    else:
        p_point = 0.55
        n = 3

    p_game = markov(n=4, p=p_point)
    p_set_straight_win, p_reach_tiebreak = markov_set(n=6, p=p_game)
    p_set = p_set_straight_win + p_reach_tiebreak * markov(n=7, p=p_point)
    p_match_best_of_n = win(p_set, (n + 1) // 2)

    if DEBUG:
        import sys

        data = {
            "p_game": p_game,
            "p_set_straight_win": p_set_straight_win,
            "p_reach_tiebreak": p_reach_tiebreak,
            "p_set": p_set,
            "p_match_best_of_n": p_match_best_of_n,
        }

        pad = max(len(name) for name in data)
        for name, val in data.items():
            name = name.ljust(pad + 2, " ")
            print(name, val, file=sys.stderr)

    print(round(p_match_best_of_n, 4))


main()