#include <iostream>
#include <string>
#include <algorithm>
#include <limits>

using namespace std;

typedef pair<int,int> pii;

pii tree[100500];

pii make(pii lhs, pii rhs)
{
    if (lhs.first < rhs.first)
        return lhs;
    if (rhs.first < lhs.first)
        return rhs;
    return make_pair(lhs.first, lhs.second + rhs.second);
}

void build(int data[], int i, int l, int r)
{
    if (l == r) {
        tree[i] = make_pair(data[l], 1);
    } else {
        int mid = (l + r) / 2;
        build(data, i*2, l, mid);
        build(data, i*2 + 1, mid + 1, r);
        tree[i] = make(tree[i*2], tree[i*2 + 1]);
    }
}

pii get_min(int i, int treel, int treer, int l, int r)
{
    if (l > r)
        return make_pair(-100000000, 0);
    if (l == treel && r == treer)
        return tree[i];
    int m = (treel + treer) >> 1;
    return make(
                get_min(i << 1, treel, m, l, min(r, m)),
                get_min((i << 1) + 1, m+1, treer, max(1, m+1), r)
                );
}

void update(int i, int l, int r, int pos, int newval)
{
    if (l == r)
        tree[i] = make_pair(newval, 1);
    else {
        int mid = (l + r) >> 1;
        if (pos <= mid)
            update(i << 1, l, mid, pos, newval);
        else update((i << 1) + 1, mid+1, r, pos, newval);
        tree[i] = make(tree[i << 1], tree[(i << 1) + 1]);
    }
}

int main()
{
    int n, q;
    string query;
    cin >> n >> q;
    int *data = new int[n+1];
    for (int i = 1; i <= n; ++i)
        cin >> data[i];

    build(data, 1, 0, n-1);

    for (int i = 0; i < q; ++i) {
        cin >> query;
        if (query == "count") {
            int l, r;
            cin >> l >> r;
            pii res = get_min(1, 0, n-1, l, r);
            cout << res.second << endl;
        }

        if (query == "change") {
            int i, v;
            cin >> i >> v;
            update(0, 0, n-1, i, v);
        }
    }
    return 0;
}
