import heapq

def successful_compile(dependencies):
    compiled = set()
    for lib, deps in dependencies.items():
        for d in deps:
            if d not in compiled:
                print(f"Import error: tried to import {lib} but {d} is required.")
                return False
        compiled.add(lib)
    print("Compiled successfully!")
    return True


def kahn(reverse):
    in_degree = {lib: 0 for lib in reverse}
    for deps in reverse.values():
        for lib in deps:
            in_degree[lib] += 1
    
    record = []
    h = []
    for lib, deg in in_degree.items():
        if deg == 0:
            heapq.heappush(h, lib)
    
    while h:
        lib = heapq.heappop(h)
        record.append(lib)
        for dep in reverse[lib]:
            in_degree[dep] -= 1
            if in_degree[dep] == 0:
                heapq.heappush(h, dep)

    if len(record) != len(reverse):
        print("Fatal error: interdependencies.")
    else:
        print("Suggest to change import order:")
        for lib in record:
            print(f"import {lib}")


def main():
    dependencies = dict()
    reverse = dict()
    for _ in range(int(input())):
        lib = input().replace("import ", "")
        dependencies[lib] = list()
        reverse[lib] = list()
    for _ in range(int(input())):
        lib, _deps = input().split(" requires ")
        deps = _deps.split(", ")
        dependencies[lib] = deps
        for d in deps:
            reverse[d].append(lib)

    if not successful_compile(dependencies):
        kahn(reverse)


main()
