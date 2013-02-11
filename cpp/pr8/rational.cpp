#include "rational.hpp"
#include <list>

inline int gcd(int a, int b)
{
    int c;
    while (b) {
        c = a % b;
        a = b;
        b = c;
    }
    return a >= 0 ? a : -a;
}

Rational::Rational()
    : m_nom(0),
      m_den(1)
{   }

Rational::Rational(int nom, int den)
    : m_nom(nom),
      m_den(den)
{   }

Rational::Rational(const Rational &other)
    : m_nom(other.m_nom),
      m_den(other.m_den)
{   }

Rational::~Rational()
{   }

void Rational::simplify()
{
    int g = gcd(m_den, m_nom);
    if (g != 1) {
        m_nom /= g;
        m_den /= g;
    }
}

Rational& Rational::operator =(const Rational& other)
{
    m_nom = other.nom();
    m_den = other.den();
}

Rational& Rational::operator +=(const Rational& b)
{
    int d = den()*b.den();
    m_nom = nom()*b.den() + den()*b.nom();
    m_den = d;
    simplify();
    return *this;
}

Rational& Rational::operator -=(const Rational& b)
{
    int d = den()*b.den();
    int bnom = -b.nom();
    m_nom = nom()*b.den() + den()*bnom;
    m_den = d;
    simplify();
    return *this;
}

Rational& Rational::operator *=(const Rational& b)
{
    m_nom *= b.nom();
    m_den *= b.den();
    simplify();
    return *this;
}

Rational& Rational::operator /=(const Rational& b)
{
    m_nom *= b.den();
    m_den *= b.nom();
    simplify();
    return *this;
}

std::ostream& operator<<(std::ostream &os, const Rational& r)
{
    os << r.nom() << "/" << r.den();
    return os;
}

std::istream& operator>>(std::istream &is, Rational& r)
{
    int nom = 0, den = 0;
    char c = is.get();
    std::list<char> n;
    std::list<char> d;
    bool neg = false;
    if (c == '-') {
        neg = true;
        c = is.get();
    }
    while (c >= '0' && c <= '9') {
        n.push_front(c);
        c = is.get();
    }
    if (c == '/')
        c = is.get();
    else return is;
    while (c >= '0' && c <= '9') {
        d.push_front(c);
        c = is.get();
    }
    std::list<char>::iterator it = n.begin();
    int m = 1;
    for (; it != n.end(); ++it) {
        nom += m*((*it) - '0');
        m *= 10;
    }
    if (neg)
        nom = -nom;
    m = 1;
    it = d.begin();
    for (; it != d.end(); ++it) {
        den += m*((*it) - '0');
        m *= 10;
    }
    if (den == 0)
        den = 1;
    r.m_den = den;
    r.m_nom = nom;
    r.simplify();
    return is;
}

Rational operator+(Rational a, const Rational& b)
{
    Rational res(a.nom()*b.den(), a.den()*b.nom());
    res.simplify();
    return res;
}

Rational operator-(Rational a, const Rational& b)
{
    Rational res(a.nom()*b.den(), a.den()*(-b.nom()));
    res.simplify();
    return res;
}

Rational operator*(Rational a, const Rational& b)
{
    Rational res(a.nom()*b.nom(), a.den()*b.den());
    res.simplify();
    return res;
}

Rational operator/(Rational a, const Rational& b)
{
    Rational res(a.nom()*b.den(), a.den()*b.nom());
    res.simplify();
    return res;
}

bool operator==(const Rational& a, const Rational& b)
{
    return a.nom() == b.nom() && a.den() == b.den();
}

bool operator!=(const Rational& a, const Rational& b)
{
    return a.nom() != b.nom() || a.den() != b.den();
}

bool operator<(const Rational& a, const Rational& b)
{
    return (a-b).nom() < 0;
}

bool operator<=(const Rational& a, const Rational& b)
{
    return (a-b).nom() <= 0;
}

bool operator>(const Rational& a, const Rational& b)
{
    return (a-b).nom() > 0;
}

bool operator>=(const Rational& a, const Rational& b)
{
    return (a-b).nom() >= 0;
}
