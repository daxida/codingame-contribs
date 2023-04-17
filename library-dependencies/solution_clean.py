import heapq


def successful_compile(dependencies):
    compiled = set()
    for library, deps in dependencies.items():
        for d in deps:
            if d not in compiled:
                print(f"Import error: tried to import {library} but {d} is required.")
                return False
        compiled.add(library)
    print("Compiled successfully!")
    return True


def kahn(dependencies):
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
    for _ in range(int(input())):
        library = input().replace("import ", "")
        dependencies[library] = list()
    for _ in range(int(input())):
        library, _deps = input().split(" requires ")
        dependencies[library] = _deps.split(", ")

    if not successful_compile(dependencies):
        kahn(dependencies)


main()
