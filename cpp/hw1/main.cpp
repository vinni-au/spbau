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

void merge(Node*& left, Node* mid)
{
	Node* tmp = 0;
	Node* tail = 0;
    Node* i1 = left;
    Node* i2 = mid;
	if (i1->value < i2->value) {
		tmp = i1;
		i1 = i1->next;
	} else {
		tmp = i2;
		i2 = i2->next;
	}
	tail = tmp;
	while (i1 && i2) {
		if (i1->value < i2->value) {
			tail->next = i1;
			i1 = i1->next;
		} else {
			tail->next = i2;
			i2 = i2->next;
		}
		tail = tail->next;
	}
	if (i1)
		tail->next = i1;
	if (i2)
		tail->next = i2;
	left = tmp;
}

void mergeSort(Node*& left)
{
    if (left->next == 0)
        return;
    Node* mid = left;
    size_t len = length(left);
    for (int i = 0; i < (len-1)/2; ++i)
        mid = mid->next;
	Node* midn = mid->next;
	mid->next = 0;
    mergeSort(left);
    mergeSort(midn);
    merge(left, midn);
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
	g_tail->next = 0;
#endif
    std::cout << "Array: " << std::endl;
    print(g_head);
    mergeSort(g_head);
    std::cout << std::endl << "Sorted array: " << std::endl;
    print(g_head);
    return 0;
}
