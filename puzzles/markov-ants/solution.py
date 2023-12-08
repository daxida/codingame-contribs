import numpy as np


def futureStates(x, y, step):
    '''
    Gives a list of every possible future state.
    Here there's trivially 4 cases, but this allows generalization.
    '''
    return [(x + step, y), (x - step, y), (x, y + step), (x, y - step)]


def solve(start, step, w, h):
    '''
    For the math details, check (IN FRENCH):
    https://idpoisson.fr/berglund/probamass_html/node18.html
    '''
    states = generateStates(step, w, h)
    matrix = transitionMarkov(step, states)

    matrix_size = matrix.shape[0]
    row_start_point = list(states.keys()).index(start)
    # The last .round(10) allows us to round the expentacy on the next line
    inv = np.linalg.inv(np.identity(matrix_size) - matrix).round(10)

    expectancy = round(sum(i for i in inv[row_start_point]), 1)

    return expectancy


def generateStates(step, w, h):
    '''
    We create the following dictionary:
        keys = every possible state
             = every point in the interior of the anthill
        values = every possible state after one step
               = every point in futureStates that REMAINS INSIDE the anthill
               (HERE)
               = every point in futureStates that verifies 0 < x < w-1 and 0 < y < h-1

    NB: notice that we NEVER use the ascii anthill. Since we are dealing only
    with rectangles, all the information of the Polygon (=Anthill) is actually alfindStarty
    given by the dimensions (width + height).
    For the general case, this won't work. We would need to build a border of the 
    Polygon from a set of points defining its border (here we could have done it 
    with the coordinates of the '+'). A pseudocode would be:
        points = input()
        polygon = Polygon(points)
        for point in polygon.interior():
            temp = [s for s in futureStates(p, step) if s in polygon.interior()]
            states.setdefault(p, temp)
    The interior could be coded with some PIP algorithm.

    ALTHOUGH, here it's way easier since the INTERIOR is just the coordinates of
    the points with value '.'. If those were not given, a bfs or floodfill from the
    starting position of the ant, stopping at the border ('|-+') would also yield
    the same result.
    '''
    states = {}
    # We loop over the interior of the anthill, i.e.:
    #   (0 < x < w-1 and 0 < y < h-1)
    # For the general solution, loop over the interior of the Polygon.
    for y in range(1, h):
        for x in range(1, w):
            temp = []
            for s in futureStates(x, y, step):
                if abs(s[0]) < w - 1 and abs(s[1]) < h - 1:
                    temp.append(s)
            states.setdefault((x, y), temp)

    return states


def transitionMarkov(step, states):
    '''
    We build the submatrix of the Markov transition matrix without absorbing states.
    '''
    matrix_points = list(states.keys())
    # Here this is always 4, but it generalizes if we add extra directions.
    move_choices = len(futureStates(0, 0, step))

    matrix = []
    # The probability of going from point1 to point2 is (1 / move_choices)
    # if point2 is a possible future state of point1, else 0.
    for point1 in matrix_points:
        row = []
        for point2 in matrix_points:
            row.append(1 / move_choices if point2 in states[point1] else 0)
        matrix.append(row)

    return np.matrix(matrix)


def main():
    step = int(input())
    w, h = [int(i) for i in input().split()]
    entries = [input() for _ in range(h)]

    # Finds the starting position of the ant.
    for y, row in enumerate(entries):
        x = row.find('A')
        if x != -1:
            start = x, y

    expectancy = solve(start, step, w, h)

    print(expectancy)


if __name__ == '__main__':
    main()
