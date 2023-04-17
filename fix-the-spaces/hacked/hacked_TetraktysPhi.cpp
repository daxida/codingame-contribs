#include <functional>
#include <iostream>
#include <map>
#include <vector>

using namespace std;

int main() {
    string s, sep, phrase;
    map<string, int> words;
    vector<size_t> spaces, solution;
    auto idx = 0;

    getline(cin, phrase);

    while ( cin >> s ) words[s] = 0;

    function<void(size_t)> split = [&](auto idx) {
        if ( idx == phrase.length() ) {
            if ( any_of(cbegin(words), cend(words), [](auto& p) { return !p.second; }) ) return;
            if ( solution.size() ) { cout << "Unsolvable" << endl; exit(0); }
            solution = spaces;
            return;
        }

        for ( size_t len = 1; len <= phrase.length() - idx; ++len ) {
            auto word = phrase.substr(idx, len);

            auto it = words.find(word);

            if ( it == end(words) ) continue;

            spaces.push_back(len), ++it->second;

            split(idx + len);

            spaces.pop_back(), --it->second;
        }
    };

    split(idx);

    for ( auto len : solution ) cout << sep << phrase.substr(idx, len), idx += len, sep = " ";
}
