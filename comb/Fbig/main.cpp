#include <iostream>
#include <vector>

using namespace std;

long long Fbig_r(int n, vector<int> a) {
    if (n < 0)
        return 0;

    if (n == 0)
        return 1;

    if (a.size() == 1) {
        if (n % a.back() == 0)
            return 1;
    }

    if (a.size() > 1) {
        vector<int> b = a;
        b.erase(b.end() - 1, b.end());
        return Fbig_r(n - a.back(), a) + Fbig_r(n, b);
    }
    return 0;
}

int main()
{
    cout << Fbig_r(78, {1,1,2,5,10,10,20,50}) << endl;
    cout << Fbig_r(78, {1,2,5,10,20,50}) << endl;
    cout << Fbig_r(10, {1,2,3,5}) << endl;
    cout << Fbig_r(100, {2,5}) << endl;
    cout << Fbig_r(100, {3,5}) << endl;
    return 0;
}
