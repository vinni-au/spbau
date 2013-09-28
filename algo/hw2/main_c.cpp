/*
 * Football
 */
#include <iostream>
#include <limits>

const int MAXN = 21;
const int MAXM = 1000;
const int INF = std::numeric_limits<int>::max();
int w[MAXN];
int r[MAXN];
int a[MAXN][MAXN];
int flow[MAXM][MAXM];
int capability[MAXM][MAXM];
int q[MAXM];
int qstart, qend;
int parent[MAXM];
bool used[MAXM];

int max_flow(int size) {
    while (true) {
        q[0] = 0;
        std::fill_n(used, MAXM, false);
        std::fill_n(parent, size, -1);
        used[0] = true;
        for (int qs = 0, qe = 1; qs < qe; ++qs) {
            int u = q[qs];
            for (int v = 0; v < size; ++v) {
                if (!used[v] && capability[u][v] - flow[u][v] > 0) {
                    used[v] = true;
                    parent[v] = u;
                    q[qe++] = v;
                }
            }
        }

        if(parent[size - 1] == -1)
            break;

        int maxflow = INF;
        for (int u = size - 1; u != 0; ) {
            int prev = parent[u];
            maxflow = std::min(maxflow, capability[prev][u] - flow[prev][u]);
            u = prev;
        }

        for (int u = size - 1; u != 0; ) {
            int prev = parent[u];
            flow[prev][u] += maxflow;
            flow[u][prev] -= maxflow;
            u = prev;
        }
    }

    int result = 0;
    for(int i = 0; i < size; ++i)
        result += flow[0][i];

    return result;
}

int main() {
    std::ios_base::sync_with_stdio(false);

    int n;
    std::cin >> n;
    for (int i = 0; i < n; ++i)
        std::cin >> w[i];
    for (int i = 0; i < n; ++i)
        std::cin >> r[i];

    w[0] += r[0];
    for (int i = 0; i < n; ++i)
        for (int j = 0; j < n; ++j)
            std::cin >> a[i][j];

    int m = (n - 1) * (n - 2) / 2;
    int count = 0;
    int sum = 0;
    for (int i = 1; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            capability[0][n + count] = a[i][j];
            capability[n + count][i] = a[i][j];
            capability[n + count][j] = a[i][j];
            sum += a[i][j];
            ++count;
        }
        capability[i][m + n] = w[0] - w[i];
        if (w[0] - w[i] < 0) {
            std::cout << "NO" << std::endl;
            return 0;
        }
    }

    if(max_flow(n + m + 1) == sum)
        std::cout << "YES" << std::endl;
    else
        std::cout << "NO" << std::endl;

    return 0;
}
