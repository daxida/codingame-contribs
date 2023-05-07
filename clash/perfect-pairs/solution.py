def perfect_pairing(n):
    to_check = int(str(n ** 2)[::-1]) ** 0.5
    return None if to_check % 1 else round(to_check)


n = int(input())

print(perfect_pairing(n))
