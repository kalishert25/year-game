import more_itertools as mit


for n in range(0, 10):
    s = sum(len(x) for x in mit.partitions('a'*n))
    print(f'The partition # for {n=} is {s}')

