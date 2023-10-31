# https://cermics.enpc.fr/~jpc/decision-incertain/index.html

def cmp(a, b):
    return (a > b) - (a < b)


def get_winner(eggs, h, w):
    tmp = min(eggs, key=lambda x: (x[0], x[1]))
    A = tmp[1] * h + tmp[0]  # traverse col
    tmp = min(eggs, key=lambda x: (x[1], x[0]))
    B = tmp[0] * w + tmp[1]  # traverse row

    return cmp(A, B)


def print_history(history):
    draws, b_wins, a_wins = history
    sz = sum(history)
    print(f"{100*a_wins / sz:.2f}%")
    print(f"{100*b_wins / sz:.2f}%")
    print(f"{100*draws / sz:.2f}%")


def exhaustive(h, w):
    points = [(y, x) for y in range(h) for x in range(w)]
    history = [0, 0, 0]  # Draws // B wins // // A wins
    for p_1 in points:
        for p_2 in points:
            if p_1 == p_2:
                continue
            history[get_winner([p_1, p_2], h, w)] += 1

    print_history(history)


h = int(input())
w = int(input())

exhaustive(h, w)
