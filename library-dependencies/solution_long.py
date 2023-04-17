import heapq


def successful_compile(dependencies):
    compiled = set()
    # We use the insertion order of dependencies
    for library, deps in dependencies.items():
        for d in deps:
            if d not in compiled:
                print(f"Import error: tried to import {library} but {d} is required.")
                return False
        compiled.add(library)
    print("Compiled successfully!")
    return True


def kahn(dependencies):
    ''' 
    Checks if there is a cycle and also builds the topological sorting.

    I intended to do this but it ended up been easier working backwards:
    https://en.wikipedia.org/wiki/Topological_sorting

    Here we work with a heap starting with the independent libraries.
    Then we add the libraries that become independent after we imported
    the previous independent libraries (respecting lexicographical order)

    Working with a heap is not needed and you could pass every test 
    sorting a whole array at each iteration.
    '''
    D = dict(dependencies)
    h = [n for n in D if not D[n]]
    h.sort()
    record = []

    while h:
        library = heapq.heappop(h)
        record.append(library)
        for other, deps in D.items():
            if library in deps:
                deps.remove(library)
            if not deps and other not in record and other not in h:
                heapq.heappush(h, other)

    if len(record) != len(dependencies):
        print("Fatal error: interdependencies.")
    else:
        print("Suggest to change import order:")
        for mod in record:
            print(f"import {mod}")


def main():
    dependencies = dict()
    n_imp = int(input())
    for _ in range(n_imp):
        library = input().replace("import ", "")
        dependencies[library] = list()
    n_dep = int(input())
    for _ in range(n_dep):
        library, _deps = input().split(" requires ")
        dependencies[library] = _deps.split(", ")

    if not successful_compile(dependencies):
        kahn(dependencies)


main()
