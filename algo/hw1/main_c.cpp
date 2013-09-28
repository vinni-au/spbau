/*
 * Раскраска отрезка
 *
 * Сторожев Антон
 */
#include <iostream>
#include <cstring>

struct segment_tree {

    explicit segment_tree(size_t size) : size(size), data(new int[4*size]) {
        std::fill_n(data, 4*size, 0);
    }

    int get(int pos) const {
        return get_helper(1, 0, size - 1, pos);
    }

    void update(int l, int r, int color) {
        update_helper(1, 0, size - 1, l, r, color);
    }

    ~segment_tree() {
        delete[] data;
    }

private:
    size_t size;
    int *data;

    void push(int v) {
        if (data[v] != -1) {
            data[v*2] = data[v*2 + 1] = data[v];
            data[v] = -1;
        }
    }

    void update_helper(int v, int tl, int tr, int l, int r, int color) {
        if (l > r) {
            return;
        }
        if (l == tl && r == tr) {
            data[v] = color;
        } else {
            push(v);
            int tm = (tl + tr) >> 1;
            update_helper(v*2, tl, tm, l, std::min(r, tm), color);
            update_helper(v*2 + 1, tm + 1, tr, std::max(l, tm + 1), r, color);
        }
    }

    int get_helper(int v, int tl, int tr, int pos) const {
        if (data[v] != -1)
            return data[v];
        int tm = (tl + tr) >> 1;
        if (pos <= tm) {
            return get_helper(v*2, tl, tm, pos);
        } else {
            return get_helper(v*2 + 1, tm + 1, tr, pos);
        }
    }

    segment_tree(const segment_tree&);
    segment_tree& operator=(const segment_tree&);
};

int main() {
    std::ios_base::sync_with_stdio(false);
    int n;
    int q;
    std::cin >> n >> q;
    segment_tree segment(n);
    for (int i = 0; i < q; ++i) {
        int l, r, c;
        std::cin >> l >> r >> c;
        segment.update(--l, --r, c);
    }

    for (int i = 0; i < n; ++i) {
        std::cout << segment.get(i) << " ";
    }
    std::cout << std::endl;
    return 0;
}
