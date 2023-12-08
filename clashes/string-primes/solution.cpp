#include <iostream>
#include <string>

using namespace std;

int main()
{
    string s;
    getline(cin, s);
    int sum = 0;
    for (char& ch : s) sum += ch;
    for (char& ch : s) {
        if (sum % ch == 0) {
            cout << ch;
            return 0;
        }
    }
    cout << "prime" << endl;
    return 0;
}