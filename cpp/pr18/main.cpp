/**
 * Интрузивные контейнеры
 */
#include <iostream>
#include <exception>

/**
 * @brief The list_base_hook struct
 */
struct list_base_hook {
    list_base_hook() : next(0), prev(0) {   }
    list_base_hook * next;
    list_base_hook * prev;
};


template< class Value_t >
struct intr_list {
    intr_list() : head(0), tail(0) { }

    void push_front(Value_t& val) {
        if (head == 0) {
            tail = head = &val;
            head->prev = tail->next = 0;
            return;
        }

        list_base_hook* new_head = &val;
        new_head->prev = 0;
        new_head->next = head;
        head->prev = new_head;
        head = new_head;
    }

    void push_back(Value_t& val) {
        if (tail == 0) {
            tail = head = &val;
            head->prev = tail->next = 0;
            return;
        }

        list_base_hook* new_tail = &val;
        new_tail->next = 0;
        new_tail->prev = tail;
        tail->next = new_tail;
        tail = new_tail;
    }

    void pop_front() {
        if (head == 0)
            return;

        if (head == tail) {
            head = tail = 0;
            return;
        }

        list_base_hook* new_head = head->next;
        new_head->prev = 0;
        head->next = head->prev = 0;
        head = new_head;
    }

    void pop_back() {
        if (tail == 0)
            return;

        if (head == tail) {
            head = tail = 0;
            return;
        }

        list_base_hook* new_tail = tail->prev;
        new_tail->next = 0;
        tail->next = tail->prev = 0;
        tail = new_tail;
    }

    /**
     * @brief back Returns reference to the last element of the list. Throws std::exception in case of an empty list
     * @return reference to the last element of the list
     */
    Value_t& back() {
        if (tail == 0)
            throw std::exception();

        return static_cast<Value_t&>(*tail);
    }

    /**
     * @brief front
     * @return
     */
    Value_t& front() {
        if (head == 0)
            throw std::exception();

        return static_cast<Value_t&>(*head);
    }

    size_t size() const {
        size_t result = 0;
        list_base_hook* cur = head;
        while (cur != 0) {
            cur = cur->next;
            ++result;
        }
        return result;
    }

private:
    list_base_hook* head;
    list_base_hook* tail;
};



struct Foo : list_base_hook {
    friend std::ostream& operator<<(std::ostream& os, Foo& foo);

    Foo() {
        val = ++new_val;
    }

private:
    Foo(Foo&);

    int val;
    static int new_val;
};

struct Boo {

};

int Foo::new_val = 0;

template< class Value_t >
std::ostream& operator<<(std::ostream& os, intr_list<Value_t> list) {
    Value_t& foo = list.front();
    list_base_hook* cur = (list_base_hook*)(&foo);
    while (cur != 0) {
        os << (Value_t&)(*cur);
        cur = cur->next;
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, Foo& foo) {
    os << foo.val << " ";
    return os;
}


int main()
{
    using std::cout;
    using std::endl;
    Foo foo1;
    Foo foo2;
    Foo foo3;
    intr_list<Foo> list;

    try {
        //testing
        cout << "push_back" << endl;
        list.push_back(foo1);
        cout << "size: " << list.size() << endl;
        cout << list << endl << endl;

        cout << "push_back" << endl;
        list.push_back(foo2);
        cout << "size: " << list.size() << endl;
        cout << list << endl << endl;

        cout << "push_front" << endl;
        list.push_front(foo3);
        cout << "size: " << list.size() << endl;
        cout << list << endl << endl;

        cout << "pop_back" << endl;
        list.pop_back();
        cout << "size: " << list.size() << endl;
        cout << list << endl << endl;

        cout << "pop_front" << endl;
        list.pop_front();
        cout << "size: " << list.size() << endl;
        cout << list << endl << endl;
    } catch(...) {
        cout << "Exception caught!" << endl;
    }

    return 0;    

}

