#include <iostream>
#include <string>
#include "rational.hpp"

using namespace std;

int main()
{
    Rational a;
    Rational b;
    string operation;

    cout << "Enter first argument: ";
    cin >> a;
    cout << "Enter second argument: ";
    cin >> b;
    cout << "Enter operation: ";
    cin >> operation;
    cout << "Result: ";
    if (operation == "+")
        cout << (a + b);
    else if (operation == "-")
        cout << (a - b);
    else if (operation == "*")
        cout << (a * b);
    else if (operation == "/")
        cout << (a / b);
    else if (operation == "<")
        cout << (a < b);
    else if (operation == "<=")
        cout << boolalpha << (a <= b);
    else if (operation == ">")
        cout << boolalpha << (a > b);
    else if (operation == ">=")
        cout << boolalpha << (a >= b);
    else if (operation == "==")
        cout << boolalpha << (a == b);
    else if (operation == "!=")
        cout << boolalpha << (a != b);

    cout << endl;

    return 0;
}
