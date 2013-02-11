#include <iostream>
#include <cstdio>
#include <limits.h>
#include "bigint.hpp"

int main()
{
    using namespace std;
    freopen("input.txt", "r", stdin);
//    freopen("output.txt", "w", stdout);
    BigInt a(123);
    BigInt b(123);
    cin >> a;
    cin >> b;
    cout << ((BigInt(-500) % -499) == -1) << endl;
    return 0;
}
