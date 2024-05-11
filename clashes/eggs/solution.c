#include <stdlib.h>
#include <stdio.h>

int H, W;
int HISTORY[] = {0, 0, 0};

typedef unsigned long marker;
marker one = 1;

// Based on: https://rosettacode.org/wiki/Combinations#C
void comb(int pool, int need, marker chosen, int point)
{
    if (pool < need + point)
        return;

    if (!need) {
        int ali = 1e9;
        int bob = 1e9;

        int cnt = 0;
        for (point = 0; point < pool; point++) {
            if (chosen & (one << point)) {
                int ph = point / W;
                int pw = point % W;

                int dist_ali = pw * H + ph;
                int dist_bob = ph * W + pw;

                if (dist_ali < ali) ali = dist_ali;
                if (dist_bob < bob) bob = dist_bob;
            }
        }
        int winner = ali == bob ? 2 : ali < bob ? 0 : 1;
        HISTORY[winner]++;
        return;
    }

    comb(pool, need - 1, chosen | (one << point), point + 1);
    comb(pool, need, chosen, point + 1);
}

int main()
{
    int n;
    scanf("%d %d %d", &H, &W, &n);

    comb(H * W, n, 0, 0);

    int sum = HISTORY[0] + HISTORY[1] + HISTORY[2];
    printf(
        "%.2f%%\n%.2f%%\n%.2f%%\n", 
        100 * (double) HISTORY[0] / sum, 
        100 * (double) HISTORY[1] / sum,
        100 * (double) HISTORY[2] / sum
    );

    return 0;
}