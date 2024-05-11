from itertools import combinations

def cmp(a, b):
    return (a > b) - (a < b)


def get_winner(eggs, h, w):
    # Smaller col, then smaller row
    ay, ax = min(eggs, key=lambda x: (x[1], x[0]))
    a = ax * h + ay  # traverse col
    # Smaller row, then smaller col
    by, bx = min(eggs, key=lambda x: (x[0], x[1]))
    b = by * w + bx  # traverse row

    return cmp(a, b)


def print_history(history):
    draws, b_wins, a_wins = history
    sz = sum(history)
    print(f"{100 * a_wins / sz:.2f}%")
    print(f"{100 * b_wins / sz:.2f}%")
    print(f"{100 * draws / sz:.2f}%")


def exhaustive(h, w, n):
    """
    The for loop iterates math.comb(h * w, n) times, and this forces 
    the constraints to be quite small. 
    For comparison, here, the maximum is obtained at:
        math.comb(6 * 6, 6) = 1947792
    but math.comb(7 * 7, 6) = 13983816 > 1e7, is already too large;
    and math.comb(6 * 6, 7) = 8347680 is very close to TLE.
    """
    points = [(y, x) for y in range(h) for x in range(w)]
    history = [0, 0, 0]  # Draws // B wins // A wins

    for ps in combinations(points, n):
        history[get_winner(ps, h, w)] += 1

    print_history(history)


h = int(input())
w = int(input())
n = int(input())

assert 0 < h <= 6, f"invalid h={h}"
assert 0 < w <= 6, f"invalid w={w}"
assert 0 < n <= 6, f"invalid n={n}"
assert n <= h * w, f"invalid var relation: {n} > {h * w}"

exhaustive(h, w, n)
