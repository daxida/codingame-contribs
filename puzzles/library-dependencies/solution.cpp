#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <set>

using namespace std;


bool successful_compile(vector<string>& order, map<string, vector<string>>& dependencies) 
{
    set<string> compiled;
    for (auto& lib : order) {
        for (auto& d : dependencies[lib]) {
            if (compiled.count(d) == 0) {
                cout << "Import error: tried to import " << lib 
                     << " but " << d << " is required." << endl;
                return false;
            }
        }
        compiled.insert(lib);
    }
    cout << "Compiled successfully!" << endl;
    return true;
}

void kahn(map<string, vector<string>> dependencies) 
{
    vector<string> q;
    for (auto& [lib, deps] : dependencies) {
        if (deps.empty()) q.push_back(lib);
    }

    vector<string> record;
    while (!q.empty()) {
        sort(q.begin(), q.end());
        string library = q.front(); q.erase(q.begin());
        record.push_back(library);

        // Import "library" and remove it from the other "libs" dependencies
        for (auto& [lib, deps] : dependencies) {
            auto it = find(deps.begin(), deps.end(), library);
            if (it != deps.end()) {
                deps.erase(it);
            }
            if (deps.empty() && lib != library 
                && find(record.begin(), record.end(), lib) == record.end() 
                && find(q.begin(), q.end(), lib) == q.end()) {
                q.push_back(lib);
            }
        }
    }

    if (record.size() != dependencies.size()) {
        cout << "Fatal error: interdependencies." << endl;
    } else {
        cout << "Suggest to change import order:" << endl;
        for (auto& library : record) {
            cout << "import " << library << endl;
        }
    }
}

int main()
{
    int n_imp, n_dep;
    cin >> n_imp; cin.ignore();
    vector<string> order;
    map<string, vector<string>> dependencies;
    for (int i = 0; i < n_imp; i++) {
        string library;
        getline(cin, library);
        library = library.substr(7); // remove "import "
        dependencies[library] = vector<string>();
        order.push_back(library);
    }

    cin >> n_dep; cin.ignore();
    for (int i = 0; i < n_dep; i++) {
        string d; 
        getline(cin, d);
        size_t pos = d.find(" requires ");
        string library = d.substr(0, pos);
        string deps = d.substr(pos + 10); // remove " requires "
        size_t comma_pos;
        while ((comma_pos = deps.find(",")) != string::npos) {
            string dep = deps.substr(0, comma_pos);
            dependencies[library].push_back(dep);
            deps = deps.substr(comma_pos + 2);
        }
        dependencies[library].push_back(deps);
    }

    // Visualization
    // for (auto& pair : dependencies) {
    //     cerr << pair.first << ": ";
    //     for (auto&d : pair.second) {
    //         cerr << d << ", ";
    //     }
    //     cerr << endl;
    // }

    if (!successful_compile(order, dependencies))
        kahn(dependencies);

    return 0;
}