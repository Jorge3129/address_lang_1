#include <iostream>

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

node** stack_new() {
    auto s = new node*;
    *s = nullptr;
    return s;
}

void stack_push(int val, node** head) {
    auto old_addr = *head;
    
    auto new_node = new node;
    
    *head = new_node;
    new_node->next = old_addr;
    new_node->value = val;
}

int stack_pop(node** head) {
    auto old_addr = *head;
    if (old_addr == nullptr) {
        return 0; 
    }

    auto old_val = old_addr->value;
    auto next_addr = old_addr->next;
    *head = next_addr;
    delete old_addr;
    return old_val;
}

bool stack_is_empty(node** head) {
    return *head == nullptr;
}

int main() {
    auto s = stack_new();
    printList(s);

    for (int i = 1; i <= 5; i++) {
        stack_push(i, s);
    }
    printList(s);

    while (!stack_is_empty(s)) {
        auto top_val = stack_pop(s);
        std::cout << top_val << std::endl;
        printList(s);
    }
    
    delete s;
    return 0;
}
