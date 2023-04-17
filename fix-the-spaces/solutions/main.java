import java.util.*;


class Solution {
    public static String solution;

    public static void solve(String original, List<String> words, List<String> sol) {
        // Here we found a solution
        if (original.isEmpty() && words.isEmpty()) {
            // If we already had found a solution it would be ambiguous
            if (solution != null) {
                System.out.println("Unsolvable");
                System.exit(0);
            }
            solution = String.join(" ", sol);
        }

        // You need to use a set here for preventing multiple 
        // equal solutions. Plus it speeds up the search
        HashSet<String> uniqueWords = new HashSet<>(words);

        for (String word : uniqueWords) {
            if (original.startsWith(word)) {
                // Try putting a fitting word at the start
                List<String> newWords = new ArrayList<>(words);
                List<String> newSol = new ArrayList<>(sol);
                newWords.remove(word);
                newSol.add(word);
                solve(original.substring(word.length()), newWords, newSol);
            }
        }
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        String original = sc.nextLine();
        List<String> words = Arrays.asList(sc.nextLine().split(" "));
        solve(original, words, new ArrayList<>());
        System.out.println(solution);
    }
}
