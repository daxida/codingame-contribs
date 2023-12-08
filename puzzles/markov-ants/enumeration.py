# by user @Flaaamort

import numpy
import sys


def log(*args):
    print(*args, file=sys.stderr)


def solve(width, height, x, y, step):
    position = numpy.full((width, height), 0.0)
    position[x, y] = 1.0

    average_time = 0

    for time in range(1, 100):
        position_ = numpy.full(position.shape, 0.0)
        position_[step:,:] += position[:-step,:]
        position_[:-step,:] += position[step:,:]
        position_[:,step:] += position[:,:-step]
        position_[:,:-step] += position[:,step:]
        position_ /= 4

        average_time += (position.sum() - position_.sum()) * time
        position = position_


    return average_time


def main():
    step = int(input())
    width, height = map(int, input().split())
    grid = ''.join(input() for _ in range(height))

    start_y, start_x = divmod(grid.index('A'), width)

    average_time = solve(width-2, height-2, start_x-1, start_y-1, step)
    print(f"{average_time:.1f}")


if __name__ == "__main__":
    main()