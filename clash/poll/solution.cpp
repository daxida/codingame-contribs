#include <iostream>
#include <vector>

using namespace std;

int main()
{
    int timeout, n_entries;
    cin >> timeout >> n_entries; cin.ignore();
    vector<int> record(n_entries, -timeout);
    int ans[2] = {0, 0};
    for (int i = 0; i < n_entries; i++) {
        int user_id, vote, time;
        cin >> user_id >> vote >> time; cin.ignore();
        if (record[user_id] + timeout <= time) {
            ans[vote]++;
            record[user_id] = time;
        }
    }

    cout << ans[0] << ' ' << ans[1] << endl;
}