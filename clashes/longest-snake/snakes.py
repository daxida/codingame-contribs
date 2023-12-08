from random import randrange
import re
import math


def generate(nb_snakes, average_length, width):
    s = ""
    for _ in range(nb_snakes):
        s += ">" + "=" * randrange(1, 2 * average_length - 1) + "o"

    # PADDING
    lines_amount = math.ceil(len(s) / width)

    while len(s) < lines_amount * width:
        r = randrange(1, len(s))
        if s[r] == "=":
            s = list(s)
            s[r] = "=="
            s = "".join(s)

    new = ""
    i = 1
    while s:
        if i % 2:
            new += s[:width]
        else:
            new += (s[:width][::-1]).replace(">", "<")
        new += "\n"
        s = s[width:]
        i += 1

    return new.strip()


def solve(snakes):
    snakes = snakes.split("\n")
    nice_format = ""
    for idx, line in enumerate(snakes):
        if idx % 2:
            nice_format += line[::-1].replace("<", ">")
        else:
            nice_format += line
    # print(nice_format)
    ans = max(re.findall(r'>=*o', nice_format), key=len)

    return len(ans)


def main():
    snakes = generate(nb_snakes=40, average_length=6, width=10)
    if len(snakes) > 1000:
        print("Exceeds IO constraints")
        exit(0)
    print(len(snakes.split("\n")))
    print(snakes)
    print(solve(snakes))


main()
