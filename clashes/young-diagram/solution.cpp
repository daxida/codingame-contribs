#include <iostream>
#include <string>
#include <vector>
#include <sstream>

using namespace std;

int main()
{
    vector<int> partition;
    string token;
    while (getline(cin, token, '+')) {
        partition.push_back(stoi(token));
    }
    
    vector<int> conjugate;
    while (partition.size() > 0) {
        while (partition.back() != 0) {
            int n = partition.size();
            for (int i = 0; i < n; i++) {
                partition[i]--;
            }
            conjugate.push_back(n);
        }
        partition.erase(partition.begin() + partition.size() - 1);   
    }
    
    string sep;
    for (int& n : conjugate) {
        cout << sep << n, sep = " + ";
    }
    cout << endl;
}