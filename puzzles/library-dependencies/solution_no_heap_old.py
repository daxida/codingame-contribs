import sys
from copy import deepcopy


def check_compile(dependencies):
    compiled = [[]]
    # We use the insertion order of dependencies
    for mod, deps in dependencies.items():
        for dep in deps:
            if dep not in compiled:
                print(f"Import error: tried to import {mod} but {dep} is required.")
                return
        compiled.append(mod)
    print("Compiled successfully!")
    exit(0)


def kahn(dependencies, n_imp):
    ''' 
    Checks if there is a cycle and also builds the topological sorting.
    -- Initially I intended to do this but it ended up been easier working backwards:
    https://en.wikipedia.org/wiki/Topological_sorting
    '''
    dep_copy = deepcopy(dependencies)
    ans = []
    cur_nodes = [n for n in dep_copy if not dep_copy[n]]  # independent nodes
    while cur_nodes:
        cur_nodes.sort()  # for alphanumeric sorting
        cur = cur_nodes.pop(0)
        ans.append(cur)
        for mod, deps in dep_copy.items():
            if cur in deps:
                deps.remove(cur)
            if not deps and mod not in ans and mod not in cur_nodes:
                cur_nodes.append(mod)

    if len(ans) != n_imp:
        print("Fatal error: interdependencies.")
    else:
        print("Suggest to change import order:")
        for mod in ans:
            print(f"import {mod}")


def main():
    dependencies = dict()
    n_imp = int(input())
    for _ in range(n_imp):
        imp = input().replace("import ", "")
        dependencies[imp] = []
    n_dep = int(input())
    for _ in range(n_dep):
        mod, dep = input().split(" requires ")
        dep = dep.split(", ")
        dependencies[mod] = dep
    print(dependencies, file=sys.stderr)

    check_compile(dependencies)
    kahn(dependencies, n_imp)


if __name__ == '__main__':
    main()
