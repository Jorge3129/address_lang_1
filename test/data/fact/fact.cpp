#include <iostream>

unsigned long long fact(unsigned long long n) {
    if (n <= 1) return 1;
    auto prevFact = fact(n - 1);
    return n * prevFact;
}

int main() {
    for (size_t i = 0; i <= 20; i++) {
        auto res = fact(i);
        std::cout << res << std::endl;
    }

    return 0;
}