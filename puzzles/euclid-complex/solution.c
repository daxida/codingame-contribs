#include <stdio.h>
#include <complex.h>
#include <math.h>

typedef float complex FC;

void fmt(const FC z) {
    float y = cimag(z);
    float x = creal(z);

    if (x == 0) {
        printf("%.0fj", y != 0 ? y : 0);
    } else {
        if (y > 0) printf("(%.0f+%.0fj)", x, y);
        else if (y < 0) printf("(%.0f-%.0fj)", x, -y);
        else printf("(%.0f+0j)", x);
    } 
}

float closest(const float n) {
    float c = ceilf(n);
    float f = floorf(n);
    float d = fabsf(n - c);

    return d <= 0.5 ? c : f;
}

FC gcd(const FC z1, const FC z2) {
    FC z = z1 / z2;

    float y = cimag(z);
    float x = creal(z);
    float cy = closest(y);
    float cx = closest(x);

    FC q = cx + cy * I;;
    FC r = z1 - z2 * q;
    
    fmt(z1); printf(" = "); 
    fmt(z2); printf(" * ");
    fmt(q);  printf(" + ");
    fmt(r);  printf("\n");
    
    return creal(r) == 0 && cimag(r) == 0 ? z2 : gcd(z2, r);
}

int main()
{
    int xa, ya, xb, yb;
    scanf("%d%d", &xa, &ya);
    scanf("%d%d", &xb, &yb);
    FC za = xa + ya * I;
    FC zb = xb + yb * I;
    FC g = gcd(za, zb);

    printf("GCD("); fmt(za);
    printf(", ");   fmt(zb);
    printf(") = "); fmt(g);

    return 0;
}