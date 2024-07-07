#include <iostream>
#include <vector>

struct node {
    int value;
    node* next;
};

// TODO rename fn
int double_(int val) {
    return val * 2;
}

node** list_empty() {
    auto s = new node*;
    *s = nullptr;
    return s;
}

void list_add(int val, node** head) {
    auto last_node = *head;

    if (*head != nullptr) {
        for (auto i = *head; i != nullptr; i = i->next) {
            last_node = i;
        }
    }
    auto new_node = new node;
    if (*head == nullptr) {
        *head = new_node;
    } else {
        last_node->next = new_node;
    }
    new_node->next = nullptr;
    new_node->value = val;
}

node** map(int (*f)(int val), node** head) {
    auto r = list_empty();

    if (*head == nullptr) {
        return r;
    }

// TODO unify pi
    for (auto i = *head; i != nullptr; i = i->next) {
        auto val = i->value;
        auto new_val = (*f)(val);

        list_add(new_val, r);
    }

    return r;
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

// this is a builtin function in ADPL
void printList(node** head) {
    std::cout << '[';
    for (auto cur = *head; cur != nullptr; cur = cur->next) {
        std::cout << cur->value;
        if (cur->next != nullptr) std::cout << ',';
    }
    std::cout << ']' <<  std::endl;
}

int main() {
    {
        node** list = create_list({1,2,3,4,5});
        printList(list);

        auto new_list = map(&double_,list);

        printList(new_list);

        delete list;
        delete new_list;
    }

    {
        node** list = create_list({});
        printList(list);

        auto new_list = map(&double_,list);

        printList(new_list);

        delete list;
        delete new_list;
    }

        {
        node** list = create_list({1});
        printList(list);

        auto new_list = map(&double_,list);

        printList(new_list);

        delete list;
        delete new_list;
    }

    return 0;
}