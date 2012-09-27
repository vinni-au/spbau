#include <iostream>
#include <cstdlib>
#include <cstring>

#define MAX_SIZE (1000)

struct Node {
    int value;
    Node* next;
};

Node g_array[MAX_SIZE];
Node* g_head = 0;
Node* g_tail = 0;

void add(Node*& list, int value)
{
    if (list == 0) {
        list = g_array;
        list->value = value;
        list->next = 0;
        g_tail = list;
    } else {
        Node* cur = list;
        while (cur->next)
            cur = cur->next;
        cur->next = cur + 1;
        cur->next->value = value;
        g_tail = cur->next;
    }
}

void print(Node* list)
{
    while (list) {
        std::cout << list->value << " ";
        list = list->next;
    }
    std::cout << std::endl;
}

size_t length(Node* list)
{
    size_t res = 0;
    while (list) {
        ++res;
        list = list->next;
    }
    return res;
}

size_t length(Node* left, Node* right)
{
    size_t res = 0;
    while (left != right) {
        left = left->next;
        ++res;
    }
    return res+1;
}

void merge(Node* left, Node* mid, Node* right)
{
    size_t len = length(left, right);
    Node tmp[len];
    Node* i1 = left;
    Node* i2 = mid;
    int j = 0;
    while (j < len) {
        if (i1 == mid) {
            (tmp+j)->value = i2->value;
            i2 = i2->next;
        } else if (i2 == right->next) {
            (tmp+j)->value = i1->value;
            i1 = i1->next;
        } else {
            if (i1->value < i2->value) {
                (tmp+j)->value = i1->value;
                i1 = i1->next;
            } else {
                (tmp+j)->value = i2->value;
                i2 = i2->next;
            }
        }
        ++j;
    }
    j = 0;
    while (j < len) {
        left->value = (tmp+j)->value;
        left = left->next;
        ++j;
    }
}

void mergeSort(Node* left, Node* right)
{
    if (left == right)
        return;
    Node* mid = left;
    size_t len = length(left, right);
    for (int i = 0; i < (len-1)/2; ++i)
        mid = mid->next;
    mergeSort(left, mid);
    mergeSort(mid->next, right);
    merge(left, mid->next, right);
}

int main()
{
    memset((void*)g_array, 0, MAX_SIZE*sizeof(Node));
#ifdef DEBUG
    for (int i = 1000; i > 0; --i)
        add(g_head, i);
#else
    int n;
    int i = 0;
    do {
        std::cin >> n;
        add(g_head, n);
    } while (n && !std::cin.eof() && i++ < 1000);
#endif
    std::cout << "Array: " << std::endl;
    print(g_head);
    mergeSort(g_head, g_tail);
    std::cout << std::endl << "Sorted array: " << std::endl;
    print(g_head);
    return 0;
}
