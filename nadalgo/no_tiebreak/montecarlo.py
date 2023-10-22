import random

DEBUG = 1

def simu(nb_simu, p=0.55, nb_sets=2):
    wins = 0
    for _ in range(nb_simu):
        scores = [0, 0]
        s1, s2 = scores
        while s1 < nb_sets and s2 < nb_sets:
            scores[set(p)] += 1
            s1, s2 = scores
        if s1 > s2:
            wins += 1

    return wins / nb_simu

def template(p, upto, func, repeat=10000):
    scores = [0, 0]
    for _ in range(repeat):
        scores[func(p)] += 1
        s1, s2 = scores
        if s1 >= upto and s1 - s2 >= 2:
            return 0
        elif s2 >= upto and s2 - s1 >= 2:
            return 1

def jeu(p):
    return template(p, 4, lambda p: 0 if random.random() <= p else 1)

def set(p):
    return template(p, 6, jeu)

if DEBUG:
    print(simu(100000, p=0.55, nb_sets=2)) # 0.95771
    print(simu(50000, p=0.55, nb_sets=2)) # 0.95802
    print(simu(100000, p=0.6, nb_sets=2)) # 0.99969
