#define H
#ifdef H
#include <iostream>
#include <bitset>

/*
1. class Array <T, size_t>
2. class Stack <T, size_t, Storage>
3. class Array <bool, size_t>
*/

template <class T, size_t Size>
class Array {
public:
    Array()
    {}

    const T& operator[] (size_t i) const {
        return m_buffer[i];
    }

    T& operator[] (size_t i) {
        return m_buffer[i];
    }

    T& at(int i) {
        return m_buffer[i];
    }

    T at(int i) const {
        return m_buffer[i];
    }

    size_t size() const {
        return Size;
    }
private:
    T m_buffer[Size];
};

#define TB
#ifdef TB
template<size_t Size>
class Array<bool, Size> {
public:
    Array() {
        memset(m_buffer, 0, sizeof(unsigned char)*Size);
    }

    const bool& operator[] (size_t i) const {
        return m_buffer[i/tsize] & 1 << i%tsize;
    }

    bool at(size_t i) const {
        return m_buffer[i/tsize] & (1 << i%tsize);
    }

    void set(size_t i, bool value) {
        size_t word = i/tsize;
        unsigned char bit = 1 << (i%tsize);
        if (value) {
            m_buffer[word] |= bit;
        } else
            m_buffer[word] &= ~bit;
    }

private:
    static const size_t tsize = sizeof(unsigned char);
    unsigned char m_buffer[Size/sizeof(unsigned char)];
};
#endif

template <class T, size_t Size, template<class,size_t> class Storage>
class Stack {
public:
    Stack() :
        m_head(-1)
    {
    }

    bool isEmpty() const {
        return m_head == -1;
    }

    T& top() {
        return m_stor[m_head];
    }

    const T& top() const {
        return m_stor[m_head];
    }

    void push(const T& item) {
        m_stor[++m_head] = item;
    }

    void pop() {
        --m_head;
    }

private:
    int m_head;
    Storage<T, Size> m_stor;
};

template <size_t Size, template<size_t Size> class Storage>
class Stack<bool, Size, Storage > {
public:
    Stack() :
        m_head(-1)
    {
    }

    bool isEmpty() const {
        return m_head == -1;
    }

    const bool& top() const {
        return m_stor[m_head];
    }

    void push(const bool& item) {
        m_stor.set(++m_head, item);
    }

    void pop() {
        --m_head;
    }

private:
    int m_head;
    Array<bool, Size> m_stor;
};


int main()
{
    Array<int, 10> m;
    Stack<int, 10, Array> s;
    s.push(1);
    s.push(2);
    while (!s.isEmpty()) {
        std::cout << s.top() << " ";
        s.pop();
    }
    std::cout << std::endl;
#ifdef TB
    std::cout << "Bool test" << std::endl;
    Stack<bool, 10, Array> sb;
    sb.push(true);
    sb.push(false);
    while (!sb.isEmpty()) {
        std::cout << sb.top() << " ";
        sb.pop();
    }
#endif
    return 0;
}


#endif


#ifndef H
#include <stdio.h>
#include <math.h>

void carryingOver(int, int, int);

int main()
{
   int number, countDisk, counter = 1, count;

   printf("Enter N: "); /* Ханойская башня */
   scanf("%d", &number);

   while (counter <= pow(2.0, number) - 1) { /* Запускаем цикл повторений */

      if (counter % 2 != 0) { /* На нечетном ходу мы будем трогать только самый маленький диск */
         carryingOver(number, counter, 1); /* С помощью этой функции определяем для данного диска перемещение */
      }

      else {  /* Определяем диск который нужно переместить на четном ходу */
         count = counter;
         countDisk = 0;

         while (count % 2 == 0) {  /* Диск который нужно переместить */
            countDisk++;           /* будет числом деления номера хода на 2 без остатка */
            count = count / 2;
         }
         carryingOver(number, counter, countDisk + 1);
      }
      counter++;
   }
   return 0;
}

/* Функция определения перемещения дисков */
void carryingOver(int n ,int i, int k)
{
   int t, axisX, axisY, axisZ;

   if (n % 2 == 0) {  /* Определяем порядок осей в зависимости от четности */
      axisX = 1;      /* и не четности количества дисков */
      axisY = 2;
      axisZ = 3;
   }

   else {
      axisX = 1;
      axisY = 3;
      axisZ = 2;
   }

   /* Номер хода можно представить единственным образом */
   /* как произведение некоего нечетного числа на степень двойки */
   /* k будет номером диска который мы перемещаем */
   t = ((i / pow(2.0, k - 1)) - 1) / 2;

   if (k % 2 != 0) {    /* Определяем перемещение дисков  для нечетного хода */

      switch (t % 3) {         /* Выбираем перемещение в зависимости от данного условия */
         case 0:
            printf("%d -> %d\n", axisX, axisY);
            break;
         case 1:
            printf("%d -> %d\n", axisY, axisZ);
            break;
         case 2:
            printf("%d -> %d\n", axisZ, axisX);
            break;
      }
   }

   else {     /* Определяем перемещение дисков  для чётного хода */

      switch (t % 3) {
         case 0:
            printf("%d -> %d\n", axisX, axisZ);
            break;
         case 1:
            printf("%d -> %d\n", axisZ, axisY);
            break;
         case 2:
            printf("%d -> %d\n", axisY, axisX);
            break;
      }
   }
}
#endif