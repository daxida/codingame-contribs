from collections import defaultdict
import sys

# Fails at

# AABABA
# A AB ABA

# Expected solution
# A AB ABA


def E(*a): print(*a, file=sys.stderr)


class Word():

    def __init__(self):
        self.occurrences = 0
        self.possible_locations = list()

    def __repr__(self):
        return f"({self.occurrences}, {str(self.possible_locations)})"


original = input()

# Keys: actual words
# Values: pair (ocurrences in original, possible positions)
words = defaultdict(Word)
for word in input().split():
    if word not in words:
        i = 0
        while (idx: = original[i:].find(word)) != -1:
            words[word].possible_locations.append(i + idx)
            i = i + idx + 1
    words[word].occurrences += 1

E(words)

solution = []
while words:
    change_made = False

    # Copy the keys. Otherwise RuntimeError: dictionary changed size during iteration
    for word_1 in list(words.keys()):
        E(word_1, words)
        # Can't solve it if two many options
        if len(words[word_1].possible_locations) != words[word_1].occurrences:
            continue

        # Remove impossible locations
        for loc in words[word_1].possible_locations:

            end = loc + len(word_1) - 1

            # Iterate over words different from word_1
            for word_2 in set(words.keys()) - {word_1}:
                start = loc - len(word_2) + 1

                # Again copy
                for num in list(words[word_2].possible_locations):
                    # E(word_1, word_2, loc, start, num, end)
                    if start <= num <= end:

                        words[word_2].possible_locations.remove(num)

            # Need to store the loc for sorting later on
            solution.append((loc, word_1))

        words.pop(word_1)
        E("Chose", word_1)
        E(words)
        change_made = True

    if not change_made:
        print("Unsolvable")
        exit(0)

solution.sort()
print(' '.join(word for _, word in solution))
