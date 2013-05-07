#include <iostream>
#include <cstring>

struct rmq {
    rmq(int size)
        : n(size), tree(new int[size*4])
    {
        memset(tree, 0, sizeof(int)*size*4);
    }

    ~rmq()
    {
        delete tree;
    }

    void set(int color, int l, int r)
    {

    }

private:
    int n;
    int *tree;
};


int main()
{
    int n, m;
    rmq(n);
    for (int i = 0; i < m; ++i) {

    }

    return 0;
}

