
# ABABABA
# AB ABA BA

# Expected solution
# AB ABA BA
# Obtained
# Unsolvable

# ABABABA
# ABA B ABA

# Expected solution
# ABA B ABA
# Obtained
# ValueError: min() arg is an empty sequence



class Word():

    def __init__(self, word):
        self.length = len(word)
        self.occurrences = 0
        self.possible_locations = []

    def last_possible_pos_covered(self):
        return max(self.possible_locations) + self.length - 1


# Add one word to the solution at a certain location. More importantly, update
# all the possible locations fo remaining words.
def add_word_to_solution(words, solution_word, placement_location, solution):
    solution.append((placement_location, solution_word))
    end = placement_location + len(solution_word) - 1
    for word in set(words.keys()) - set([solution_word]):
        start = placement_location - len(word) + 1
        for num in words[word].possible_locations[:]:
            if start <= num <= end:
                words[word].possible_locations.remove(num)

    words[solution_word].occurrences -= 1
    if words[solution_word].occurrences == 0:
        words.pop(solution_word) 


# Main Program
original  = input()

words = dict()
for word in input().split():
    if word not in words:
        words[word] = Word(word)
        i = 0
        while (idx := original[i:].find(word)) != -1:
            words[word].possible_locations.append(i + idx)
            i = i + idx + 1
    words[word].occurrences += 1


unsolveable = False
solution = []
while words:
    change_made = False
    for word in list(words.keys()):
        if len(words[word].possible_locations) == words[word].occurrences:
            for loc in words[word].possible_locations:
                add_word_to_solution(words, word, loc, solution)
            change_made = True

    # Only when there are no easy reduction techniques available, 
    # use the following logic to look for words that are singleton 
    # candidates to occupy the furthest left or furthest right remaining position.
    if not change_made:
        for word in list(words.keys()):
            if min(words[word].possible_locations) < min(min(words[w].possible_locations) for w in words if w != word):
                loc = min(words[word].possible_locations)
                add_word_to_solution(words, word, loc, solution)
                change_made = True

            elif words[word].last_possible_pos_covered() > max(words[w].last_possible_pos_covered() for w in words if w != word):
                loc = max(words[word].possible_locations)
                add_word_to_solution(words, word, loc, solution)
                change_made = True

    if not change_made:
        unsolveable = True
        break

print(' '.join([word for _, word in sorted(solution)]) if not unsolveable else 'Unsolvable')