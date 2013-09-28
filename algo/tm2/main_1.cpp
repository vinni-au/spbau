#include <algorithm>
#include <string>
#include <iostream>
#include <cmath>

const int MAXN = 40000;
bool compound[MAXN];
int primes[MAXN];
int primes_count = 0;

void find_primes() {
    for (int i = 2; i < MAXN; i++) {
        if (!compound[i]) {
            primes[primes_count++] = i;
            for (int j = i * i; j < MAXN; j += i)
                compound[j] = true;
        }
    }
}

int powmod(int a, int n, int mod) {
    int res = 1;
    while(n) {
      if(n&1)
          res = ((long long)res * a) % mod;
      n >>= 1;
      a = ((long long) a * a) % mod;
    }
    return res;
}


int factorize(int n) {
    for (int i = 0; i < primes_count; ++i) {
        if (n % primes[i] == 0)
            return primes[i];
    }

    return -1;
}

int gcde(int a, int b, int& u, int& v) {
    if (!b) {
        u = 1;
        v = 0;
        return a;
    }
    int u1, v1;
    int r = a / b;
    int d = gcde(b, a - r * b, u1, v1);
    u = v1;
    v = u1 - r * v1;
    return d;
}

int solve(int a, int p) {
    if(p == 2)
        return a;

    if(a == 0)
        return 0;

    if(powmod(a, (p - 1) / 2, p) == p - 1)
        return -1;

    int a1 = powmod(a, p - 2, p);
    int x = 1;
    while(true) {
        int y2 = ((((long long) x * x) % p) * a1) % p;
        int y = std::sqrt(y2 + 0.9);

        if(y * y == y2)
            return ((long long) x * powmod(y, p - 2, p)) % p;

        x = rand() % (p - 1) + 1;
    }

    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    find_primes();
    int q;
    int a, n;
    std::cin >> q;
    for(int t = 0; t < q; ++t) {
        std::cin >> n >> a;

        int roots[4];
        int count = 0;
        int p = factorize(n);
        int q = n / p;
        int a1 = solve(a % p, p);
        int a2 = solve(a % q, q);
        if(a1 == -1 || a2 == -1) {
            std::cout << "0" << std::endl;
            break;
        }

        int ur = powmod(p, q - 1, n);
        int vr = powmod(q, p - 1, n);
        for (size_t i = 0; i < 2; ++i) {
            int r1 = i ? n - a1 : a1;
            for (size_t j = 0; j < 2; ++j) {
                int r2 = j ? n - a2 : a2;
                roots[count++] = ((long long) vr * r1 + (long long) ur * r2) % n;
            }
        }
        std::sort(roots, roots + count);
        count = std::unique(roots, roots + count) - roots;

        std::cout << count << " ";
        for (int i = 0; i < count; ++i)
            std::cout << roots[i] << " ";
        std::cout << std::endl;
    }
    return 0;
}
