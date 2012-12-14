#ifndef BIGINT_HPP
#define BIGINT_HPP

#include <string>
#include <ostream>
#include <istream>
#include <cstring>
#include <algorithm>
#include <vector>
#include <complex>
#define M_PI       3.14159265358979323846
#include <cmath>
#ifdef DEBUG
    #include <iostream>
#endif

//! Big integer
class BigInt {
public:
    BigInt();
    BigInt(long long n);
    BigInt(std::string s);
    BigInt(const BigInt& other);

    ~BigInt();

    operator long long();

    BigInt& operator += (const BigInt& b);
    BigInt& operator -= (const BigInt& b);
    BigInt& operator *= (const BigInt& b);
    BigInt& operator /= (const BigInt& b);
    BigInt& operator %= (const BigInt& b);

    BigInt& operator += (long long b);
    BigInt& operator -= (long long b);
    BigInt& operator *= (long long b);
    BigInt& operator /= (long long b);
    BigInt& operator %= (long long b);

    BigInt& operator = (const BigInt& b);

    friend const BigInt operator - (const BigInt& a);

    friend std::ostream& operator << (std::ostream& os, const BigInt& a);
    friend std::istream& operator >> (std::istream& os, BigInt& a);

    friend BigInt operator + (const BigInt& a, const BigInt& b);
    friend BigInt operator - (const BigInt& a, const BigInt& b);
    friend BigInt operator * (const BigInt& a, const BigInt& b);
    friend BigInt operator / (const BigInt& a, const BigInt& b);
    friend BigInt operator % (const BigInt& a, const BigInt& b);

    friend BigInt operator + (const BigInt& a, long long b);
    friend BigInt operator + (long long a, const BigInt& b);
    friend BigInt operator - (const BigInt& a, long long b);
    friend BigInt operator - (long long a, const BigInt& b);
    friend BigInt operator * (const BigInt& a, long long b);
    friend BigInt operator * (long long a, const BigInt& b);
    friend BigInt operator / (const BigInt& a, long long b);
    friend BigInt operator / (long long a, const BigInt& b);
    friend BigInt operator % (const BigInt& a, long long b);
    friend BigInt operator % (long long a, const BigInt& b);

    friend bool operator == (const BigInt& a, const BigInt& b);
    friend bool operator != (const BigInt& a, const BigInt& b);
    friend bool operator < (const BigInt& a, const BigInt& b);
    friend bool operator <= (const BigInt& a, const BigInt& b);
    friend bool operator > (const BigInt& a, const BigInt& b);
    friend bool operator >= (const BigInt& a, const BigInt& b);

    friend bool operator == (const BigInt& a, long long b);
    friend bool operator == (long long a, const BigInt& b);
    friend bool operator != (const BigInt& a, long long b);
    friend bool operator != (long long a, const BigInt& b);
    friend bool operator < (const BigInt& a, long long b);
    friend bool operator < (long long a, const BigInt& b);
    friend bool operator <= (const BigInt& a, long long b);
    friend bool operator <= (long long a, const BigInt& b);
    friend bool operator > (const BigInt& a, long long b);
    friend bool operator > (long long a, const BigInt& b);
    friend bool operator >= (const BigInt& a, long long b);
    friend bool operator >= (long long a, const BigInt& b);


    bool isZero() const;

private:
    typedef std::complex<double> fft_base_t;
    char* buffer;
    bool neg;
    size_t len;
    size_t capacity;

    void swap(BigInt &b) throw();
    void realloc(size_t newcap = 0);

    //helpers
    static void add_helper(char* a, size_t as, char* b, size_t bs, BigInt& res);
    //this assumes a >= b
    static void sub_helper(char* a, size_t as, char* b, size_t bs, BigInt& res);
    static void mul_helper(char* a, size_t as, char* b, size_t bs, BigInt& res);
    static void mul_helper_fft(char* a, size_t as, char* b, size_t bs, BigInt& res);
    //this comparsion helpers doesn't deal with sign
    static bool lt_helper(char* a, size_t as, char* b, size_t bs);
    static bool le_helper(char* a, size_t as, char* b, size_t bs);
    static bool gt_helper(char* a, size_t as, char* b, size_t bs);
    static bool ge_helper(char* a, size_t as, char* b, size_t bs);
    static bool eq_helper(char* a, size_t as, char* b, size_t bs);
    static bool ne_helper(char* a, size_t as, char* b, size_t bs);

    void correctLen();
    void levelup();

    //fft
    static void fft_helper(std::vector<fft_base_t> &res, bool inverse);
};

//--------------------fwd decl------------------------------
std::ostream& operator << (std::ostream& os, const BigInt& a);
std::istream& operator >> (std::istream& os, BigInt& a);

const BigInt operator - (const BigInt& a);

BigInt operator + (const BigInt& a, const BigInt& b);
BigInt operator - (const BigInt& a, const BigInt& b);
BigInt operator * (const BigInt& a, const BigInt& b);
BigInt operator / (const BigInt& a, const BigInt& b);
BigInt operator % (const BigInt& a, const BigInt& b);

bool operator == (const BigInt& a, const BigInt& b);
bool operator != (const BigInt& a, const BigInt& b);
bool operator < (const BigInt& a, const BigInt& b);
bool operator <= (const BigInt& a, const BigInt& b);
bool operator > (const BigInt& a, const BigInt& b);
bool operator >= (const BigInt& a, const BigInt& b);

bool operator == (const BigInt& a, long long b);
bool operator == (long long a, const BigInt& b);
bool operator != (const BigInt& a, long long b);
bool operator != (long long a, const BigInt& b);
bool operator < (const BigInt& a, long long b);
bool operator < (long long a, const BigInt& b);
bool operator <= (const BigInt& a, long long b);
bool operator <= (long long a, const BigInt& b);
bool operator > (const BigInt& a, long long b);
bool operator > (long long a, const BigInt& b);
bool operator >= (const BigInt& a, long long b);
bool operator >= (long long a, const BigInt& b);


BigInt operator + (const BigInt& a, long long b);
BigInt operator + (long long a, const BigInt& b);
BigInt operator - (const BigInt& a, long long b);
BigInt operator - (long long a, const BigInt& b);
BigInt operator * (const BigInt& a, long long b);
BigInt operator * (long long a, const BigInt& b);
BigInt operator / (const BigInt& a, long long b);
BigInt operator / (long long a, const BigInt& b);
BigInt operator % (const BigInt& a, long long b);
BigInt operator % (long long a, const BigInt& b);

#endif // BIGINT_HPP
