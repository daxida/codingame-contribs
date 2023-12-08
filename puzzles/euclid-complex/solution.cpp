#include <iostream>
#include <complex>
#include <iostream>

using namespace std;


void fmt(const complex<float>& z) {
    float y = z.imag();
    float x = z.real();

    if (x == 0) {
        // To prevent weird stuff like -0j
        printf("%.0fj", y != 0 ? y : 0);
    } else {
        if (y > 0) printf("(%.0f+%.0fj)", x, y);
        else if (y < 0) printf("(%.0f-%.0fj)", x, -y);
        else printf("(%.0f+0j)", x);
    } 
}

float closest(float n) {
    float c = ceil(n);
    float f = floor(n);
    float d = abs(n - c);

    return d <= 0.5 ? c : f;
}

complex<float> gcd(complex<float> z1, complex<float> z2) {
    complex<float> z = z1 / z2;

    float y = z.imag();
    float x = z.real();
    float cy = closest(y);
    float cx = closest(x);

    complex<float> q (cx, cy);
    complex<float> r = z1 - z2 * q;
    
    fmt(z1); cout << " = "; 
    fmt(z2); cout << " * ";
    fmt(q);  cout << " + ";
    fmt(r);  cout << endl;
    
    return r.real() == 0 && r.imag() == 0 ? z2 : gcd(z2, r);
}

int main()
{
    float xa, ya, xb, yb;
    cin >> xa >> ya >> xb >> yb;
    complex<float> z1 (xa, ya), z2 (xb, yb);
    complex<float> _gcd = gcd(z1, z2);
    cout << "GCD("; fmt(z1);
    cout << ", "; fmt(z2);
    cout << ") = "; fmt(_gcd);
}