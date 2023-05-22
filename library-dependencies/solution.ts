function main(): void {
    const dependencies: Record<string, string[]> = {};
    const reverse: Record<string, string[]> = {};

    const nImp: number = parseInt(readline());
    for (let i = 0; i < nImp; i++) {
        const lib: string = readline().replace("import ", "");
        dependencies[lib] = [];
        reverse[lib] = [];
    }

    const nDep: number = parseInt(readline());
    for (let i = 0; i < nDep; i++) {
        const [lib, _deps] = readline().split(" requires ");
        const deps: string[] = _deps.split(", ");
        dependencies[lib] = deps;
        for (const d of deps) reverse[d].push(lib);
    }

    if (!successfulCompile(dependencies)) {
        kahn(reverse);
    }
}

function successfulCompile(dependencies: Record<string, string[]>): boolean {
    const compiled = new Set<string>();
    for (const [lib, deps] of Object.entries(dependencies)) {
        for (const d of deps) {
            if (!compiled.has(d)) {
                console.log(`Import error: tried to import ${lib} but ${d} is required.`);
                return false;
            }
        }
        compiled.add(lib);
    }
    console.log("Compiled successfully!");
    return true;
}

function kahn(reverse: Record<string, string[]>): void {
    let inDegree: Record<string, number> = {};
    for (const lib of Object.keys(reverse)) {
        inDegree[lib] = 0;
    }
    for (const deps of Object.values(reverse)) {
        for (const lib of deps) {
            inDegree[lib] += 1;
        }
    }
    
    let rec: string[] = [];
    let h: string[] = [];
    for (const [lib, deg] of Object.entries(inDegree)) {
        if (deg === 0) h.push(lib);
    }

    while (h.length > 0) {
        h.sort().reverse(); // This should ideally be a heap
        const lib = h.pop();
        rec.push(lib);
        for (const dep of reverse[lib]) {
            inDegree[dep] -= 1;
            if (inDegree[dep] === 0) {
                h.push(dep);
            }
        }
    }

    if (rec.length !== Object.keys(reverse).length) {
        console.log("Fatal error: interdependencies.");
    } else {
        console.log("Suggest to change import order:");
        for (const lib of rec) {
            console.log(`import ${lib}`);
        }
    }
}

main()