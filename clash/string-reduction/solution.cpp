#include <iostream>
#include <string>

using namespace std;

int main()
{
    string a, b, ans;
    cin >> a >> b;
    int sa = a.size();
    int sb = b.size();
    int idx_a = 0;
    int idx_b = 0;
    char prev = '$';
    while (idx_a < sa && idx_b < sb) {
        if (a[idx_a] == b[idx_b]) {
            ans += a[idx_a];
            idx_b++;
            prev = a[idx_a];
        } else if (prev == a[idx_a]) {
            cout << "Subsequence is not unique" << endl;
        } else {
            ans += "-";
        }
        idx_a++;
    }
    // Right trailing 
    for (int idx = idx_a; idx < sa; ++idx) {
        ans += "-";
        if (prev == a[idx]) {
            cout << "Subsequence is not unique" << endl;
        }
    }

    cout << ans << endl;
}