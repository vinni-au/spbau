/*
 * RMQ-sharp-min
 */

#include <iostream>
#include <string>
#include <limits>

typedef std::pair<int,int> pii;

pii t[4*100500];

pii combine(pii a, pii b) {
    if (a.first < b.first)
        return a;
    if (b.first < a.first)
        return b;
    return std::make_pair(a.first, a.second + b.second);
}

void build(int a[], int v, int tl, int tr) {
    if (tl == tr) {
        t[v] = std::make_pair(a[tl], 1);
    } else {
        int tm = (tl + tr) >> 1;
        build(a, v*2, tl, tm);
        build(a, v*2+1, tm+1, tr);
        t[v] = combine(t[v*2], t[v*2+1]);
    }
}

pii get_min(int v, int tl, int tr, int l, int r) {
    if (l > r)
        return std::make_pair(std::numeric_limits<int>::max(), 0);
    if (l == tl && r == tr)
        return t[v];
    int tm = (tl + tr) >> 1;
    return combine(
                get_min(v*2, tl, tm, l, std::min(r, tm)),
                get_min(v*2 + 1, tm+1, tr, std::max(l, tm+1), r)
                );
}

void update(int v, int tl, int tr, int pos, int val) {
    if (tl == tr) {
        t[v] = std::make_pair(val, 1);
    } else {
        int tm = (tl + tr) >> 1;
        if (pos <= tm)
            update(v*2, tl, tm, pos, val);
        else
            update(v*2+1, tm+1, tr, pos, val);
        t[v] = combine(t[v*2], t[v*2+1]);
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    int n, q;
    std::cin >> n >> q;
    int* a = new int[n];
    std::string cmd;
    for (int i = 0; i < n; ++i) {
        std::cin >> a[i];
    }
    build(a, 1, 0, n-1);
    int l, r;
    for (int i = 0; i < q; ++i) {
        std::cin >> cmd >> l >> r;
        if (cmd == "count") {
            pii ans = get_min(1, 0, n-1, --l, --r);
            std::cout << ans.second << std::endl;
        } else if (cmd == "change") {
            update(1, 0, n-1, --l, r);
        }
    }
    return 0;
}
