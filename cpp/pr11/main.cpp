#include <iostream>
#include <algorithm>
#include <vector>
#include <list>
#include <initializer_list>

//non-efficient
template< class RAIt >
void rotate( RAIt first, RAIt n_first, RAIt last) {
    std::reverse(first, n_first);
    std::reverse(n_first, last);
    std::reverse(first, last);
}

//efficient
template< class RAIt >
void ef_rotate( RAIt first, RAIt n_first, RAIt last) {

}

template< class RAIt >
void right_rotate( RAIt first, RAIt n_first, RAIt last ) {
}

int main()
{
    typedef std::vector<int> container_t;
    container_t v1{0, 1, 2, 3, 4};
    std::cout << "Left rotate:" << std::endl;
    auto n_f1 = ++++v1.begin();
    ::rotate(v1.begin(), n_f1, v1.end());
    for (auto it = v1.begin(); it != v1.end(); it++)
        std::cout << *it << " ";
    std::cout << std::endl;

    container_t v2{0, 1, 2, 3, 4};
    std::cout << "Right rotate:" << std::endl;
    auto n_f2 = ++++v2.begin();
    ::right_rotate(v2.begin(), n_f2, v2.end());
    for (auto it = v2.begin(); it != v2.end(); it++)
        std::cout << *it << " ";
    std::cout << std::endl;


    container_t v{0, 1, 2, 3, 4};
    std::cout << "STL rotate: " << std::endl;
    auto n_f = ++++v.begin();
    std::rotate(v.begin(), n_f, v.end());
    for (auto it = v.begin(); it != v.end(); it++)
        std::cout << *it << " ";
    std::cout << std::endl;

    return 0;
}
