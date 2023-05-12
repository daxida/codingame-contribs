using System;
using System.Linq;
using System.IO;
using System.Text;
using System.Collections;
using System.Collections.Generic;


class Solution
{
    static Dictionary<string, List<string>> dependencies = new Dictionary<string, List<string>>();
    static Dictionary<string, List<string>> reverse = new Dictionary<string, List<string>>();

    static void Main(string[] args)
    {
        int n = int.Parse(Console.ReadLine());
        for (int i = 0; i < n; i++)
        {
            string lib = Console.ReadLine().Replace("import ", "");
            // Use insertion order for easier logic in SuccessfulCompile
            dependencies[lib] = new List<string>();
            reverse[lib] = new List<string>();
        }
        int m = int.Parse(Console.ReadLine());
        for (int i = 0; i < m; i++)
        {
            string[] parts = Console.ReadLine().Split(" requires ");
            string lib = parts[0];
            string[] deps = parts[1].Split(", ");
            dependencies[lib] = new List<string>(deps);
            foreach (string d in deps)
            {
                reverse[d].Add(lib);
            }
        }

        if (!SuccessfulCompile()) {
            Kahn();
        }
    }

    static bool SuccessfulCompile()
    {
        HashSet<string> compiled = new HashSet<string>();
        foreach (KeyValuePair<string, List<string>> entry in dependencies)
        {
            string lib = entry.Key;
            List<string> deps = entry.Value;
            foreach (string d in deps)
            {
                if (!compiled.Contains(d))
                {
                    Console.WriteLine($"Import error: tried to import {lib} but {d} is required.");
                    return false;
                }
            }
            compiled.Add(lib);
        }
        Console.WriteLine("Compiled successfully!");
        return true;
    }

    static void Kahn()
    {
        Dictionary<string, int> inDegree = new Dictionary<string, int>();
        foreach (string lib in reverse.Keys)
        {
            inDegree[lib] = 0;
        }
        foreach (List<string> deps in reverse.Values)
        {
            foreach (string lib in deps)
            {
                inDegree[lib] += 1;
            }
        }

        List<string> record = new List<string>();
        PriorityQueue<string, string> h = new PriorityQueue<string, string>();
        foreach (KeyValuePair<string, int> entry in inDegree)
        {
            string lib = entry.Key;
            int deg = entry.Value;
            if (deg == 0)
            {
                h.Enqueue(lib, lib);
            }
        }

        while (h.Count > 0)
        {
            string lib = h.Dequeue();
            record.Add(lib);
            foreach (string dep in reverse[lib])
            {
                inDegree[dep] -= 1;
                if (inDegree[dep] == 0)
                {
                    h.Enqueue(dep, dep);
                }
            }
        }

        if (record.Count != reverse.Count)
        {
            Console.WriteLine("Fatal error: interdependencies.");
        }
        else
        {
            Console.WriteLine("Suggest to change import order:");
            foreach (string lib in record)
            {
                Console.WriteLine($"import {lib}");
            }
        }
    }
}