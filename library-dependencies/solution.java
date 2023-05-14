import java.util.*;


class Solution {
    // LinkedHashMap to preserve insertion order
    static Map<String, List<String>> dependencies = new LinkedHashMap<String, List<String>>();
    static Map<String, List<String>> reverse = new LinkedHashMap<String, List<String>>();

    public static void main(String args[]) {
        Scanner in = new Scanner(System.in);
        int nImp = in.nextInt();
        if (in.hasNextLine()) {
            in.nextLine();
        }
        for (int i = 0; i < nImp; i++) {
            String lib = in.nextLine().replace("import ", "");
            dependencies.put(lib, new ArrayList<String>());
            reverse.put(lib, new ArrayList<String>());
        }
        int nDep = in.nextInt();
        if (in.hasNextLine()) {
            in.nextLine();
        }
        for (int i = 0; i < nDep; i++) {
            String[] parts = in.nextLine().split(" requires ");
            String lib = parts[0];
            String[] deps = parts[1].split(", ");
            dependencies.put(lib, new ArrayList<String>(Arrays.asList(deps)));
            for (String d : deps) {
                reverse.get(d).add(lib);
            }
        }

        if (!successfulCompile()) {
            kahn();
        }
    }

    static boolean successfulCompile() {
        Set<String> compiled = new HashSet<String>();
        for (Map.Entry<String, List<String>> entry : dependencies.entrySet()) {
            String lib = entry.getKey();
            List<String> deps = entry.getValue();
            for (String d : deps) {
                if (!compiled.contains(d)) {
                    System.out.printf("Import error: tried to import %s but %s is required.\n", lib, d);
                    return false;
                }
            }
            compiled.add(lib);
        }
        System.out.println("Compiled successfully!");
        return true;
    }

    static void kahn() {
        // Learning foreach. 
        // Could have looped over dictionaries as in successfulCompile
        Map<String, Integer> inDegree = new HashMap<String, Integer>(0);
        reverse.keySet().forEach((lib) -> { inDegree.put(lib, 0); });
        // Can specify the type or not
        reverse.values().forEach((deps) -> { 
            deps.forEach((String lib) -> {
                inDegree.put(lib, inDegree.get(lib) + 1);
            });
        });
  
        List<String> record = new ArrayList<String>();
        PriorityQueue<String> h = new PriorityQueue<String>();
        inDegree.forEach((lib, deg) -> { if (deg == 0) h.add(lib); });

        while (!h.isEmpty()) {
            String lib = h.poll();
            record.add(lib);
            reverse.get(lib).forEach((dep) -> {
                inDegree.put(dep, inDegree.get(dep) - 1);
                if (inDegree.get(dep) == 0) h.add(dep);
            });
        }

        if (record.size() != reverse.size()) {
            System.out.println("Fatal error: interdependencies.");
        } else {
            System.out.println("Suggest to change import order:");
            for (String lib : record) {
                System.out.printf("import %s\n", lib);
            }
        }
    }
}