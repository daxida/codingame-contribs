// https://en.wikipedia.org/wiki/Secretary_problem

#include <iostream>

using namespace std;

double e = 2.7;

int main()
{
    int n; 
    cin >> n;
    int scores[n];
    for (int i = 0; i < n; i++) {
        cin >> scores[i];
    }

    int cutoff = n / e;
    cerr << cutoff << endl;

    int best = -1;
    for (int i = 0; i < cutoff; i++) {
        best = max(best, scores[i]);
    }
    
    int ans = -1;
    for (int i = cutoff; i < n; i++) {
        if (scores[i] > best) {
            ans = i;
            break;
        }
    }
    cout << ans << endl;

    return 0;
}