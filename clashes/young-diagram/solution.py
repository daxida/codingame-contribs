partition = list(map(int, input().split(' + ')))
maximum = max(partition)

young = []
for summand in partition:
    young.append((summand * '1').ljust(maximum, '0'))

conjugate = []
for row in zip(*young):
    conjugate.append(str(sum(map(int, ''.join(row)))))

print(' + '.join(conjugate))