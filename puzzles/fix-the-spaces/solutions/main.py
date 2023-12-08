def solve(original, words, solution = []):
    if not original and not words:
        sol = " ".join(solution[::-1])
        solutions.append(sol)
        if len(solutions) > 1: 
            print("Unsolvable")
            exit(0)
    
    for word in set(words): # If you don't use a set you timeout
        if original.startswith(word):
            new_words = words[:]
            new_words.remove(word)
            solve(original[len(word):], new_words, [word] + solution)

original = input()
words = input().split()
solutions = []
solve(original, words)
print(solutions[0])

