using System;
using System.Linq;
using System.Collections.Generic;


class Solution
{
    public static string solution = "";
    public static List<string> used = new List<string>();

    static void Main(string[] args)
    {
        string original = Console.ReadLine();
        string[] words = Console.ReadLine().Split(" ");
        Solve(original, words);
        Console.WriteLine(solution);
    }

    static void Solve(string original, string[] words)
    {
        if (original == "" || !words.Any())
        {
            if (solution != "")
            {
                Console.WriteLine("Unsolvable");
                Environment.Exit(0);
            }
            solution = String.Join(" ", used);
        }

        HashSet<string> uniqueWords = new HashSet<string>(words);
        foreach (string word in uniqueWords)
        {
            if (original.StartsWith(word))
            {
                int idx = Array.IndexOf(words, word);
                string[] newWords = words.Where((val, i) => i != idx).ToArray();

                used.Add(word);
                Solve(original.Substring(word.Length), newWords);
                used.RemoveAt(used.Count - 1);
            }
        }
    }
}

