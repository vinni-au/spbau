#include <iostream>
#include <string>
#include "memoryallocator.hpp"

int main()
{
    using namespace std;
    int size;
    cin >> size;
    MemoryAllocator ma(size);
    string s;
    while (cin >> s) {
        if (s == "ALLOC") {
            int b;
            cin >> b;
            void* p = ma.alloc(b);
            if (p != 0)
                cout << "+ " << ma.r_ptr(p);
            else cout << "-";
            cout << endl;
        } else if (s == "FREE") {
            int p;
            cin >> p;
            if (ma.free(ma.ptr(p)))
                cout << "+";
            else cout << "-";
            cout << endl;
        } else if (s == "INFO") {
            ma.info(cout);
        } else if (s == "MAP") {
            cout << ma;
        } else if (s == "EXIT" || s == "QUIT")
            break;
        else
            cout << "Can't understand your request" << endl;
    }
    return 0;
}
