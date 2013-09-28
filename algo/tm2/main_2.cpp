/*
 * Fast multiplication
 */
#include <iostream>
#include <complex>
#include <vector>
#include <cmath>

typedef std::complex<double> base_t;

void fft (std::vector<base_t> & a, bool invert) {
    size_t n = a.size();
    if (n == 1)
        return;

    std::vector<base_t> a0 (n/2);
    std::vector<base_t> a1 (n/2);
    for (size_t i = 0, j =0; i<n; i+=2, ++j) {
        a0[j] = a[i];
        a1[j] = a[i+1];
    }

    fft (a0, invert);
    fft (a1, invert);

    double ang = 2*M_PI/n * (invert ? -1 : 1);
    base_t w (1),  wn (cos(ang), sin(ang));
    for (size_t i=0; i<n/2; ++i) {
        a[i] = a0[i] + w * a1[i];
        a[i+n/2] = a0[i] - w * a1[i];
        if (invert)
            a[i] /= 2,  a[i+n/2] /= 2;
        w *= wn;
    }
}

void multiply (const std::vector<int> & a, const std::vector<int> & b, std::vector<int> & res) {
    std::vector<base_t> fa (a.begin(), a.end()),  fb (b.begin(), b.end());
    size_t n = 1;
    while (n < std::max (a.size(), b.size()))  n <<= 1;
    n <<= 1;
    fa.resize (n),  fb.resize (n);

    fft (fa, false),  fft (fb, false);
    for (size_t i=0; i<n; ++i)
        fa[i] *= fb[i];
    fft (fa, true);

    res.resize (n);
    for (size_t i=0; i<n; ++i)
        res[i] = int (fa[i].real() + 0.5);

    int carry = 0;
    for (size_t i=0; i<n; ++i) {
        res[i] += carry;
        carry = res[i] / 10;
        res[i] %= 10;
    }
    int i = res.size();
    while (res[--i] == 0 && i > 0) ;
    if (++i != res.size())
        res.resize(i);
}

int main()
{
    std::ios_base::sync_with_stdio(false);
    int n;
    std::cin >> n;
    std::vector<int> result;
    std::string tmp;
    for (int i = 0; i < n; ++i) {
        std::vector<int> a;
        std::vector<int> b;
        std::cin >> tmp;
        a.reserve(tmp.length());
        for (auto it = tmp.rbegin(); it != tmp.rend(); ++it) {
            a.push_back((int)((*it)-'0'));
        }
        std::cin >> tmp;
        b.reserve(tmp.length());
        for (auto it = tmp.rbegin(); it != tmp.rend(); ++it) {
            b.push_back((int)((*it)-'0'));
        }
        multiply(a, b, result);
        for (auto it = result.rbegin(); it != result.rend(); ++it) {
            std::cout << *it;
        }
        std::cout << std::endl;
    }
    return 0;
}

