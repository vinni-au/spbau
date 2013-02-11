#include <iostream>
#include "scoped_ptr.txx"

using namespace std;

int main()
{
    scoped_ptr<int> i(new int);
    (*i) = 4;
    cout << *i;
    return 0;
}

