/**
 * LCA-online
 */
#include <string>
#include <vector>
#include <iostream>

const int maxChild = 25;
int size = 1;
int up[1005000][maxChild];
int d[1005000];

int push(int u, int k) {
    for(int i = 0; i < maxChild; ++i) {
        if(k & (1 << i))
            u = up[u][i];
    }
    return u;
}

int lca(int u, int v) {
    if(d[v] > d[u])
        v = push(v, d[v] - d[u]);
    if(d[u] > d[v])
        u = push(u, d[u] - d[v]);

    if(u == v)
        return u;

    for(int i = maxChild - 1; i >= 0; --i) {
        if(up[u][i] != up[v][i]) {
            u = up[u][i];
            v = up[v][i];
        }
    }
    return up[u][0];
}

int main() {
    std::ios_base::sync_with_stdio(false);
    int n;
    int u, v;
    std::string query;
    std::cin >> n;
    for (int i = 0; i < n; ++i) {
        std::cin >> query;
        if (query == "add") {
            std::cin >> u;
            --u;
            up[size][0] = u;
            d[size] = d[u] + 1;
            for(int i = 1; i < maxChild; ++i) {
                up[size][i] = up[up[size][i - 1]][i - 1];
            }
            ++size;
        } else if (query == "lca") {
            std::cin >> u >> v;
            --u, --v;
            std::cout << lca(u, v)+1 << std::endl;
        }
    }
    return 0;
}
