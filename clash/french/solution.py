def parse_words(line):
    letters = [c if c.isalpha() else ' ' for c in line]
    return ''.join(letters).split()  

errors = "éd éds éf éfs ér érs éz ézs".split()

t = int(input())
for _ in range(t):
    for word in parse_words(input()):
        if "éx" in word or any(word.endswith(e) for e in errors):
            print(word)
