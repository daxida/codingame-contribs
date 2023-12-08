#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <sstream>

using namespace std;

int main()
{
    int n; cin >> n;
    cin.ignore();

    char c[2];
    cin.getline(c, 2);

    string s, elt;
    getline(cin, s);
    vector<string> elts;
    stringstream ss(s);
    while (ss >> elt) {
        elts.push_back(elt);
    }

    sort(elts.begin(), elts.end());
    
    bool isGroup = true;
    vector<string> missing;
    for (int i = 0; i < n; ++i) {
        string groupElt(i, c[0]);
        if (groupElt == "") groupElt = "0";
        bool found = false;
        for (int j = 0; j < elts.size(); ++j) {
            if (elts[j] == groupElt) {
                found = true;
                break;
            }
        }
        if (!found) {
            isGroup = false;
            missing.push_back(groupElt);
        }
    }

    if (isGroup) {
        cout << n;
    } else {
        int sz = missing.size();
        for (int i = 0; i < sz; ++i) {
            cout << missing[i];
            if (i < sz - 1) cout << " ";
        }
    }
    cout << endl;
}