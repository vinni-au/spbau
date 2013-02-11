#ifndef RATIONAL_HPP
#define RATIONAL_HPP

#include <iostream>
#include <string>
//alexey.kudinkin+cppc@gmail.com

struct Rational
{
    Rational();
    Rational(int nom, int den);
    Rational(const Rational& other);
    ~Rational();

    int nom() const
    { return m_nom; }

    int den() const
    { return m_den; }

    Rational& operator += (const Rational& b);
    Rational& operator -= (const Rational& b);
    Rational& operator /= (const Rational& b);
    Rational& operator *= (const Rational& b);

    Rational& operator=(const Rational& b);

    friend std::ostream& operator<<(std::ostream &os, const Rational& r);
    friend std::istream& operator>>(std::istream &is, Rational& r);

    friend Rational operator+(Rational a, const Rational& b);
    friend Rational operator-(Rational a, const Rational& b);
    friend Rational operator*(Rational a, const Rational& b);
    friend Rational operator/(Rational a, const Rational& b);

    friend bool operator==(const Rational& a, const Rational& b);
    friend bool operator!=(const Rational& a, const Rational& b);
    friend bool operator<(const Rational& a, const Rational& b);
    friend bool operator<=(const Rational& a, const Rational& b);
    friend bool operator>(const Rational& a, const Rational& b);
    friend bool operator>=(const Rational& a, const Rational& b);

private:
    int m_nom;
    int m_den;

    inline void simplify();
};

std::ostream& operator<<(std::ostream &os, const Rational& r);
std::istream& operator>>(std::istream &is, Rational& r);

#endif // RATIONAL_HPP
