#include <iostream>
#include <string>
#include <algorithm>
#include <cmath>

using ll = unsigned long long;
using namespace std;


int main() {
    ll n;
    cin >> n;

    ll square = n * n;
    cerr << "Square " << square << endl;

    string reverse_string = to_string(square);
    reverse(reverse_string.begin(), reverse_string.end());
    cerr << "Reversed string " << reverse_string << endl;

    // This doesn't work
    // string::size_type sz = 0;
    // ll reverse_square = stoll(tmp, &sz, 0);
    ll reverse_square = 0;
    for (int i = 0; i < reverse_string.size(); i++) {
        reverse_square *= 10;
        reverse_square += reverse_string[i] - '0';
    }
    cerr << "Reversed " << reverse_square << endl;

    long double to_check = sqrt(reverse_square);
    cerr << "Root " << to_check << endl;

    if (to_check == floor(to_check)) {
        cout << floor(to_check) << endl;
    } else {
        cout << "None" << endl;
    }

    return 0;
}