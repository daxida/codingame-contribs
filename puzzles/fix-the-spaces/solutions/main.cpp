#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <set>

using namespace std;

vector<string> used;

void solve(string original, vector<string>& words, string& solution) {
    // Here we found a solution
    if (original.empty() && words.empty()) {
        // If we had already found a solution we return unsolvable
        // Note that this works because we used a set, otherwise we
        // will find multiple equal solutions if a word is repeated
        if (solution.size() > 1) {
            cout << "Unsolvable" << endl;
            exit(0);
        }
        solution = used[0];
        for (int i = 1; i < used.size(); ++i) {
            solution += " " + used[i];
        }
    }
    
    // Try a word if it fits the start ot the current "original"
    set<string> unique_words(words.begin(), words.end());
    for (auto word : unique_words) {
        if (original.find(word) == 0) {
            // Copies and removes the word
            vector<string> new_words = words;
            new_words.erase(find(new_words.begin(), new_words.end(), word));

            // Tries this way with the chosen word added to it
            used.push_back(word);
            solve(original.substr(word.length()), new_words, solution);
            used.pop_back();
        }
    }
}

int main()
{
    string original, word;
    getline(cin, original);
    vector<string> words;
    while (cin >> word) words.push_back(word);

    string solution;
    solve(original, words, solution);
    cout << solution << endl;
    return 0;
}

