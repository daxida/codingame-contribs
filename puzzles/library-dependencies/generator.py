import random
import string

ALPHA = string.ascii_uppercase

PYTHON_LIBRARIES_NAMES = [
    "sys",
    "math",
    "os",
    "inspect",
    "time",
    "contextlib",
    "random",
    "numpy",
    "pandas",
    "itertools",
    "functools",
    "collections",
    "builtins",
    "requests",
    "unittest",
]


def number_to_base(n, b):
    if n == 0:
        return "A"
    digits = []
    while n:
        digits.append(ALPHA[int(n % b)])
        n //= b
    return "".join(reversed(digits))


def generate(n, names=None, to_print=True):
    if not names:
        libraries = [number_to_base(i, 26) for i in range(n)]
    else:
        libraries = names
        n = len(libraries)

    s = random.sample(libraries, n)  # Change n here to only pick some
    dependencies = {lib: [] for lib in libraries}
    for i, lib in enumerate(s):
        n_dep = random.randint(0, 3)  # Number of dependencies
        for _ in range(n_dep):
            dependencies[lib] = random.sample(libraries, n_dep)
            # Avoid A depends on A
            if lib in dependencies[lib]:
                dependencies[lib].remove(lib)

    if to_print:
        print(n)
        for lib in s:
            print(f"import {lib}")

        cnt = sum(dep != [] for dep in dependencies.values())
        nb_edges = 0
        print(cnt)
        for lib, dep in dependencies.items():
            if dep:
                nb_edges += len(dep)
                dep = ", ".join(dep)
                print(f"{lib} requires {dep}")
        # For evaluating the complexity
        print(f"Number of edges: {nb_edges}")
    else:
        n_imp = n
        _dependencies = dict()
        for lib in s:
            _dependencies[lib] = list()
        n_dep = sum(dep != [] for dep in dependencies.values())
        for lib, dep in dependencies.items():
            if dep:
                _dependencies[lib] = dep

        return _dependencies


# generate(22, names=PYTHON_LIBRARIES_NAMES)
generate(40)
