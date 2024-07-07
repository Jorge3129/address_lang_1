#include <iostream>
#include <vector>

struct node {
    int value;
    node* next;
};

// this is a builtin function in ADPL
void printList(node** head) {
    std::cout << '[';
    for (auto cur = *head; cur != nullptr; cur = cur->next) {
        std::cout << cur->value;
        if (cur->next != nullptr) std::cout << ',';
    }
    std::cout << ']' <<  std::endl;
}

node** list_empty() {
    auto s = new node*;
    *s = nullptr;
    return s;
}

node** create_list(std::vector<int> values) {
    auto list = list_empty();
    auto current_node = *list;
    
    for (size_t i = 0; i < values.size(); ++i) {
        auto value = values[i];
        auto new_addr = new node;
        if (i == 0) {
            *list = new_addr;
        } else {
            current_node->next = new_addr;
        }
        new_addr->next = nullptr;
        new_addr->value = value;
        current_node = new_addr;
    }
    
    return list;
}

int main() {
    node** list = create_list({1,2,3,4,5});
    printList(list);
    printList(create_list({}));
    printList(create_list({1}));
    printList(create_list({1,2}));

    return 0;
}