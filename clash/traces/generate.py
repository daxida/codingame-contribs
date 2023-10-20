from random import randint, sample, shuffle
from itertools import chain
import string


def generate_test():
    ALPHA = list(string.ascii_lowercase)
    N_SECTIONS = randint(1, 1)
    SEPARATOR_SIZE = randint(1, 1)
    SECTIONS_SIZE = randint(2, 22)

    # Generate random separators
    fixeds = []
    for _ in range(N_SECTIONS):
        section = sample(ALPHA, SEPARATOR_SIZE)

        # Remove the separators from the pool
        for x in section:
            ALPHA.remove(x)

        fixeds.append(section)

    # Generate both sequence a and b. Guaranteed to be true.
    a = []
    b = []
    for fixed in fixeds:
        section = sample(ALPHA, SECTIONS_SIZE)

        # Add duplicate letters
        for _ in range(randint(1, 20)):
            random_letter = sample(section, 1)
            section.extend(random_letter * randint(1, 3))

        shuffle(section)
        a.extend(section)

        section = list(section)
        shuffle(section)
        b.extend(section)

        a.extend(fixed)
        b.extend(fixed)

    a = "".join(a)
    b = "".join(b)
    f = "".join(chain(*fixeds))

    # print(a, b, f)
    return a, b, f


n_tests = 100
print(n_tests)
for _ in range(n_tests):
    print(*generate_test())
