#include <iostream>
#include <vector>
#include <algorithm>
#include <cstdint>

#define INF 1e9

using ulll = __int128;
using namespace std;

// Solves the overflow problem up to 1 << 128

ulll gcd(ulll a, ulll b) {
    if (b == 0)
        return a;
    return gcd(b, a % b);
}

void readInt128(ulll& x) {
    string s;
    cin >> s;
    x = 0;
    for (int i = 0; i < s.size(); ++i) {
        x = x * 10 + (s[i] - '0');
    }
}

void printInt128(ulll x) {
    if (x == 0) {
        cout << '0';
        return;
    }

    string result;
    while (x > 0) {
        result += '0' + static_cast<char>(x % 10);
        x /= 10;
    }

    cout << string(result.rbegin(), result.rend());
}

int main() {
    int n;
    cin >> n;

    vector<ulll> xs(n);
    for (int i = 0; i < n; i++) {
        readInt128(xs[i]);
    }
    sort(xs.begin(), xs.end());

    n += 1; // Universal point

    vector<vector<ulll>> dist(n, vector<ulll>(n, 0));
    for (int i = 0; i < n - 1; i++) {
        for (int j = 0; j < n - 1; j++) {
            if (i == j) continue;

            dist[i + 1][j + 1] = gcd(xs[i], xs[j]) == 1 ? 1 : 2;
        }
    }

    // Initialize dp
    vector<vector<ulll>> dp(n, vector<ulll>(1 << n, INF));
    for (int i = 0; i < n; i++) {
        dp[i][1 << i] = dist[0][i];
    }

    // Held-Karp main
    for (int i = 0; i < (1 << n); i++) {
        if ((i & 1) == 0) continue;

        for (int j = 0; j < n; j++) {
            if (dp[j][i] == INF) continue;

            for (int k = 0; k < n; k++) {
                ulll nxt = dp[j][i] + dist[j][k];
                int mask = i | (1 << k);
                dp[k][mask] = min(dp[k][mask], nxt);
            }
        }
    }

    // Backtrack an optimal TSP tour
    int mask = (1 << n) - 1;
    vector<int> path_idxs;
    int cur = 0;

    while (mask > 1) {
        int nxt = -1;
        ulll min_dist = INF;

        for (int i = 1; i < n; i++) {
            if ((mask & (1 << i)) != 0 && dp[i][mask] + dist[i][cur] < min_dist) {
                min_dist = dp[i][mask] + dist[i][cur];
                nxt = i;
            }
        }

        path_idxs.insert(path_idxs.begin(), nxt);
        mask ^= (1 << nxt);
        cur = nxt;
    }

    vector<ulll> path;
    for (int idx : path_idxs) {
        path.push_back(xs[idx - 1]);
    }

    // We need to verify the solution
    bool ok = adjacent_find(path.begin(), path.end(), [](ulll x, ulll y) {
        return gcd(x, y) != 1;
    }) == path.end();

    if (!ok) {
        cout << -1;
        return 0;
    }
    for (int i = path.size() - 1; i >= 0; i--) {
        printInt128(path[i]);
        if (i > 0) cout << ' ';
    }

    return 0;
}
