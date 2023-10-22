import math

# https://het.as.utexas.edu/HET/Software/Numpy/reference/generated/numpy.random.RandomState.negative_binomial.html
# https://stats.stackexchange.com/questions/458802/probability-of-winning-a-competition-k-games-best-of-series-of-n-games/458808#458808


def pnbinom(q, size, prob):
    # density
    coef = math.comb(q + size - 1, size - 1)
    return coef * prob ** size * (1 - prob) ** q


def win(w, k):
    # cumsum (distribution)
    return sum(pnbinom(i, k, w) for i in range(k))


print(win(0.9, 3))  # Output: 0.99144
