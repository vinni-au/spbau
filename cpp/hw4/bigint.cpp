#include "bigint.hpp"

BigInt::BigInt()
    : buffer(0),
      neg(false),
      len(1),
      capacity(MINCAP)
{
    buffer = new char[MINCAP];
    memset(buffer, 0, MINCAP);
}

BigInt::BigInt(int n)
    : buffer(0),
      neg(false),
      len(1),
      capacity(MINCAP)
{
#ifdef DEBUG
    static size_t count = 0;
    std::cout << "long long ctor #" << count++ << std::endl;
#endif
    buffer = new char[MINCAP];
    memset(buffer, 0, MINCAP);
    if (n == 0) {
        len = 1;
        return;
    }
    if (n < 0) {
        neg = true;
        n = -n;
    }
    int i = 0;
    while (n) {
        buffer[i++] = n % 10;
        n /= 10;
    }
    len = i;
}

BigInt::BigInt(std::string s)
    : buffer(0),
      neg(false),
      len(1)
{
    int end = s.size();
    if (end > 0) {
        if (s[0] == '-') {
            neg = true;
            --end;
        }
        buffer = new char[end*2];
        capacity = end*2;
        len = end;
        for (int i = end - 1; i >= 0; --i) {
            buffer[end - 1 - i] = s[i] - '0';
        }
    }
}

BigInt::BigInt(const BigInt &other)
    : buffer(0),
      neg(other.neg),
      len(other.len),
      capacity(other.capacity)
{
#ifdef DEBUG
    static size_t count = 0;
    std::cout << "copy ctor #" << count++ << std::endl;
#endif
    buffer = new char[capacity];
    memcpy(buffer, other.buffer, len);
}

BigInt::~BigInt()
{
#ifdef DEBUG
    static size_t count = 0;
    std::cout << "dtor #" << count++ << std::endl;
#endif
    delete[] buffer;
}

BigInt& BigInt::operator =(const BigInt& b)
{
#ifdef DEBUG
    static size_t count = 0;
    std::cout << "- operator #" << count++ << std::endl;
#endif
    if (this != &b)
        BigInt(b).swap(*this);
    return *this;
}

const BigInt operator -(const BigInt& a)
{
    BigInt res(a);
    res.neg = !res.neg;
    res.isZero();
    return res;
}

void BigInt::swap(BigInt &b)
{
#ifdef DEBUG
    static size_t count = 0;
    std::cout << "swap #" << count++ << std::endl;
#endif
    std::swap(buffer, b.buffer);
    std::swap(len, b.len);
    std::swap(capacity, b.capacity);
    std::swap(neg, b.neg);
}

BigInt operator + (const BigInt& a, const BigInt& b)
{
    if (a.neg && !b.neg)
        return b - (-a);

    if (!a.neg && b.neg)
        return a - (-b);

    BigInt res;
    res.neg = a.neg && b.neg;

    BigInt::add_helper(a.buffer, a.len, b.buffer, b.len, res);
    res.isZero();
    return res;
}

BigInt& BigInt::operator +=(const BigInt& b)
{
    BigInt tmp;
    if (neg && !b.neg)
        tmp = b - (-(*this));
    else
    if (!neg && b.neg)
        tmp = (*this) - (-b);
    else
        tmp = (*this) + b;

    tmp.swap(*this);

    return *this;
}

BigInt operator - (const BigInt& a, const BigInt& b)
{
    if (a.neg && !b.neg)
        return -(-a + b);

    if (!a.neg && b.neg)
        return a + (-b);

    BigInt res;
    bool abigger = BigInt::ge_helper(a.buffer, a.len, b.buffer, b.len);
    if (abigger)
        BigInt::sub_helper(a.buffer, a.len, b.buffer, b.len, res);
    else BigInt::sub_helper(b.buffer, b.len, a.buffer, a.len, res);

    res.neg = a.neg ^ !abigger;
    res.isZero();
    return res;
}

BigInt& BigInt::operator -=(const BigInt& b)
{
    BigInt tmp;
    if (neg && !b.neg)
        tmp = -(-(*this) + b);
    else
    if (!neg && b.neg)
        tmp = (*this) + (-b);
    else
        tmp = (*this) - b;

    tmp.swap(*this);
    return *this;
}

BigInt operator * (const BigInt& a, const BigInt& b)
{
    BigInt res;
    BigInt::mul_helper(a.buffer, a.len, b.buffer, b.len, res);
    res.neg = a.neg ^ b.neg;
    res.isZero();
    return res;
}

BigInt& BigInt::operator *=(const BigInt& b)
{
    BigInt tmp = (*this)*b;
    tmp.swap(*this);
    return *this;
}

#include <iostream>

BigInt operator / (const BigInt& a, const BigInt& b)
{
    if (BigInt::lt_helper(a.buffer, a.len, b.buffer, b.len))
        return BigInt();

    if (BigInt::eq_helper(a.buffer, a.len, b.buffer, b.len)) {
        BigInt res(1);
        res.neg = a.neg ^ b.neg;
        res.isZero();
        return res;
    }

    BigInt res;
    res.realloc(a.len);
    BigInt b_(b);
    b_.neg = false;
    BigInt curv;
    for (int i = a.len -1; i >= 0; --i) {
        curv *= 10;
        curv.buffer[0] = a.buffer[i];
        char x = 0;
        char l = 0;
        char r = 10;
        while (l <= r) {
            char mid = (l + r) / 2;
            BigInt cur = b_ * mid;
            if (cur <= curv) {
                x = mid;
                l = mid + 1;
            } else
                r = mid - 1;
        }
        res.buffer[i] = x;
        curv -= b_ * x;
    }
    res.len = a.len;
    res.correctLen();
    res.neg = a.neg ^ b.neg;
    res.isZero();
    return res;
}

BigInt& BigInt::operator /=(const BigInt& b)
{
    BigInt tmp = (*this)/b;
    tmp.swap(*this);
    return *this;
}

BigInt operator % (const BigInt& a, const BigInt& b)
{
    if (BigInt::lt_helper(a.buffer, a.len, b.buffer, b.len))
        return BigInt(a);

    if (BigInt::eq_helper(a.buffer, a.len, b.buffer, b.len))
        return BigInt(0);

    BigInt res;
    BigInt b_(b);
    b_.neg = false;
    res.realloc(a.len);
    BigInt curv;
    for (size_t i = a.len; i--;) {
        curv *= 10;
        curv.buffer[0] = a.buffer[i];
        char x = 0;
        char l = 0;
        char r = 10;
        while (l <= r) {
            int mid = (l + r) / 2;
            BigInt cur = b_ * mid;
            if (cur <= curv) {
                x = mid;
                l = mid + 1;
            } else
                r = mid - 1;
        }
        res.buffer[i] = x;
        curv = curv - b_*x;
    }
    curv.neg = a.neg;
    curv.isZero();
    return curv;
}

BigInt& BigInt::operator %=(const BigInt& b)
{
    BigInt tmp = (*this) % b;
    tmp.swap(*this);
    return *this;
}

bool operator == (const BigInt& a, const BigInt& b)
{
    if (a.neg != b.neg)
        return false;

    return BigInt::eq_helper(a.buffer, a.len, b.buffer, b.len);
}

bool operator != (const BigInt& a, const BigInt& b)
{
    return !(a == b);
}

bool operator < (const BigInt& a, const BigInt& b)
{
    if (a.neg && !b.neg)
        return true;

    if (!a.neg && b.neg)
        return false;

    return (a.neg && b.neg) ? BigInt::gt_helper(a.buffer, a.len, b.buffer, b.len):
                              BigInt::lt_helper(a.buffer, a.len, b.buffer, b.len);
}

bool operator <= (const BigInt& a, const BigInt& b)
{
    if (a.neg && !b.neg)
        return true;

    if (!a.neg && b.neg)
        return false;

    return (a.neg && b.neg) ? BigInt::ge_helper(a.buffer, a.len, b.buffer, b.len):
                              BigInt::le_helper(a.buffer, a.len, b.buffer, b.len);
}

bool operator > (const BigInt& a, const BigInt& b)
{
    return !(a <= b);
}

bool operator >= (const BigInt& a, const BigInt& b)
{
    return !(a < b);
}

void BigInt::add_helper(char *a, size_t as, char *b, size_t bs, BigInt& res)
{
    int carry = 0;
    int sum = 0;
    size_t maxlen = std::max(as, bs);
    if (res.capacity < maxlen + 1)
        res.realloc(maxlen*2);
    size_t i = 0;
    for (; i < maxlen; ++i) {
        sum = (i < as ? a[i] : 0) + (i < bs ? b[i] : 0) + carry;
        res.buffer[i] = sum % 10;
        carry = sum / 10;
    }
    if (carry)
        res.buffer[i++] = carry;
    res.len = i;
}

void BigInt::sub_helper(char *a, size_t as, char *b, size_t bs, BigInt &res)
{
    int carry = 0;
    if (res.capacity < as)
        res.realloc(as * 2);
    memcpy(res.buffer, a, as);
    res.len = as;
    for (size_t i = 0; i < bs || carry; ++i) {
        res.buffer[i] -= carry + (i < bs ? b[i] : 0);
        carry  = res.buffer[i] < 0;
        if (carry)
            res.buffer[i] += 10;
    }
    res.correctLen();
}

void BigInt::mul_helper(char *a, size_t as, char *b, size_t bs, BigInt &res)
{
    res.realloc(as + bs);
    int carry;
    for (size_t i = 0; i < as; ++i) {
        carry = 0;
        for (size_t j = 0; j < bs || carry; ++j) {
            int cur = res.buffer[i+j] + a[i] * (j < bs ? b[j] : 0) + carry;
            res.buffer[i+j] = char(cur % 10);
            carry = int(cur / 10);
        }
    }
    res.len = as + bs;
    res.correctLen();
}

void BigInt::correctLen()
{
    while (len > 1 && buffer[len-1] == 0)
        --len;
    if (len == 0)
        ++len;
}

void BigInt::levelup()
{
    if (capacity <= len - 1)
        realloc();
    for (size_t i = len; i >= 1; --i)
        buffer[i] = buffer[i-1];
    if (buffer[len])
        len++;
}

bool BigInt::isZero()
{
    if (len == 1 && buffer[0] == 0) {
        neg = false;
        return true;
    }
    return false;
}

bool BigInt::eq_helper(char *a, size_t as, char *b, size_t bs)
{
    if (as != bs)
        return false;

    for (size_t i = 0; i < as; ++i)
        if (a[i] != b[i])
            return false;

    return true;
}

bool BigInt::ne_helper(char *a, size_t as, char *b, size_t bs)
{
    return !BigInt::eq_helper(a, as, b, bs);
}

bool BigInt::lt_helper(char *a, size_t as, char *b, size_t bs)
{
    if (as != bs)
        return as < bs;

    for (size_t i = as; i--;)
        if (a[i] < b[i])
            return true;
        else if (a[i] > b[i])
            return false;

    return false;
}

bool BigInt::le_helper(char *a, size_t as, char *b, size_t bs)
{
    if (as != bs)
        return as < bs;

    for (size_t i = as; i--;)
        if (a[i] < b[i])
            return true;
        else if (a[i] > b[i])
            return false;

    return true;
}

bool BigInt::gt_helper(char *a, size_t as, char *b, size_t bs)
{
    return !BigInt::le_helper(a, as, b, bs);
}

bool BigInt::ge_helper(char *a, size_t as, char *b, size_t bs)
{
    return !BigInt::lt_helper(a, as, b, bs);
}

void BigInt::realloc(size_t newcap)
{
#ifdef DEBUG
//    std::cout << "Realloc to " << newcap << std::endl;
#endif
    char* newbuf = new char[newcap == 0 ? capacity <<= 1 : capacity = newcap];
    memset(newbuf, 0, capacity);
    memcpy(newbuf, buffer, len);
    delete[] buffer;
    buffer = newbuf;
}

std::ostream& operator<<(std::ostream& os, const BigInt& a)
{
    if (a.neg)
        os << "-";
    for (int i = a.len - 1; i >= 0; --i)
        os << static_cast<char>(a.buffer[i] + '0');
    return os;
}

std::istream& operator>>(std::istream& is, BigInt& a)
{
    std::string s;
    is >> s;
    size_t size = s.size();
    if (size > 0) {
        if (s[0] == '-') {
            a.neg = true;
            s = s.substr(1, --size);
        }
        if (a.capacity < size) {
            delete[] a.buffer;
            a.buffer = new char[size*2];
            a.capacity = size*2;
        }
        a.len = size;
        for (size_t i = 0; i < size; ++i)
            a.buffer[i] = s[size - i - 1] - '0';
    }
    return is;
}
