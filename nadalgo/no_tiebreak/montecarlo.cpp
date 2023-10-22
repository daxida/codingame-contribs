#include <iostream>
#include <random>

using namespace std;


std::random_device rd;
std::mt19937 gen(rd());
std::uniform_real_distribution<> dis(0.0, 1.0);


int _template(double p, int upto, int (*func)(double), int repeat = 10000) {
    int scores[2] = {0, 0};
    for (int i = 0; i < repeat; i++) {
        scores[func(p)] += 1;
        int s1 = scores[0];
        int s2 = scores[1];
        if (s1 >= upto && s1 - s2 >= 2) {
            return 0;
        } else if (s2 >= upto && s2 - s1 >= 2) {
            return 1;
        }
    }
}

int jeu(double p) {
    return _template(p, 4, [](double p) { return (dis(gen) <= p) ? 0 : 1; });
}

int _set(double p) {
    return _template(p, 6, jeu);
}

double simu(int nb_simu, double p = 0.55, int nb_sets = 2) {
    int wins = 0;
    for (int i = 0; i < nb_simu; i++) {
        int scores[2] = {0, 0};
        int s1 = scores[0];
        int s2 = scores[1];
        while (s1 < nb_sets && s2 < nb_sets) {
            scores[_set(p)] += 1;
            s1 = scores[0];
            s2 = scores[1];
        }
        if (s1 > s2) {
            wins += 1;
        }
    }

    return double(wins) / nb_simu;
}


int main() {
    float p;
    int n_sets;
    bool input = false;
    if (input) {
        cin >> p >> n_sets; cin.ignore();
        cout << simu(5000, p, n_sets);
    } else {
        cout << simu(100000, 0.55, 2) << endl;
        cout << simu(50000, 0.55, 2) << endl;
        cout << simu(100000, 0.6, 2) << endl;
    }
    
    return 0;
}
