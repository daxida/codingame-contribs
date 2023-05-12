partition = list(map(int, input().split(' + ')))
conjugate = []

while partition:
    while partition[-1] != 0:
        l = len(partition)
        for i in range(l):
            partition[i] -= 1
        conjugate.append(str(l))

    partition.pop()

print(' + '.join(conjugate))