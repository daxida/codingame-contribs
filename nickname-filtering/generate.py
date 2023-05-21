import random
import string


def generate():
    CHOICES = [chr(i) for i in range(33, 126) if chr(i) not in string.ascii_lowercase + string.ascii_uppercase]

    n = 3
    n_bans = 5
    n_pseudos = 20

    def random_letters(n):
        return random.sample([chr(i) for i in range(33, 126)], n)

    bans = []
    for _ in range(n_bans):
        w = "".join(random_letters(3))
        bans.append(w)
    # E(bans)

    subs = dict()
    half = False
    for c in string.ascii_lowercase:
        if half and random.randint(0, 1) < 1:
            continue
        amt_subs = 2
        cur_subs = []
        if len(CHOICES) < amt_subs:
            continue
        for _ in range(amt_subs):
            letter = random.choice(CHOICES)
            CHOICES.remove(letter)
            cur_subs.append(letter)

        cur_subs.append(c.upper())
        subs[c] = cur_subs
    # E(subs)

    pseudos = []
    for _ in range(n_pseudos):
        pseudo = "".join(random_letters(2*n + random.randint(5, 10)))
        pseudos.append(pseudo)
    # E(pseudos)

    f = 1
    if f:
        n_subs = len(subs)
        print(n_bans, n_subs, n_pseudos)
        for i in range(n_bans):
            print(bans[i])
        cnt = 0
        for k, v in subs.items():
            print(k, " ".join(v))
            cnt += 1
        assert cnt == n_subs
        for i in range(n_pseudos):
            print(pseudos[i])

    return subs, bans, pseudos

generate()