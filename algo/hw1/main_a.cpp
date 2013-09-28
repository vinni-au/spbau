#include <bitset>
#include <vector>
#include <iostream>

const int MAXN = 501;

std::bitset<MAXN> solve(std::vector<std::bitset<MAXN> > a, int n, int m) {
    std::bitset<MAXN> result;
    for (int row = 0, col = 0; col < m; ++col) {
        for (int i = row; i < n; ++i) if(a[i][col]) {
            if (i != row)
                std::swap(a[i], a[row]);
        }

        if (row < n && a[row][col]) {
            for(int i = 0; i < n; ++i) {
                if(i != row && a[i][col])
                    a[i] ^= a[row];
            }
            ++row;
        } else {
            result.set(col, 1);
        }
    }

    for (int i = 0; i < n; ++i) {
        if(a[i].count() > 0) {
            int j = 0;
            while (!a[i][j]) {
                ++j;
            }
            result.set(j, (a[i].count() + 1) & 1);
        }
    }

    return result;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    int h, w;
    int x;
    std::cin >> h >> w;
    std::vector<std::bitset<MAXN> > a(h);
    for(int i = 0; i < h; ++i) {
        for(int j = 0; j < w; ++j) {
            std::cin >> x;
            a[i].set(j, x);
        }
    }

    std::bitset<MAXN> y = solve(a, h, w);

    for(int i = 0; i < w; ++i)
        std::cout << "0" << std::endl;

    for(int i = 0; i < w; ++i)
        std::cout << y[i] << std::endl;

    return 0;
}

