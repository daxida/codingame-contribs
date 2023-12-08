#include <iostream>
#include <string>
#include <queue>

using namespace std;

// Not optimal at all, just testing how far can I get before TLE

int count_points(vector<int> icenter, int radius, int len) {
    queue<pair<int, int>> q;
    q.push(make_pair(0, 0));
    int ans = 0;

    while (!q.empty()) {
        auto [size, d] = q.front();
        q.pop();

        if (size == len) ans++;
        else {
            char c1 = icenter[size];
            for (int c2 = 0; c2 < 26; c2++) {
                int nd = d + abs(c1 - c2);
                if (nd <= radius) {
                    q.push(make_pair(size + 1, nd));
                }
            }
        }
    }

    return ans;
}

int main() {
    int radius; 
    cin >> radius;
    cin.ignore();
    string center;
    getline(cin, center);
    vector<int> icenter;
    for (auto& ch : center) icenter.push_back(ch - 'a');

    int ans = count_points(icenter, radius, center.size());
    cout << ans << endl;
}
