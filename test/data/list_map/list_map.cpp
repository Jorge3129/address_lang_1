#include <iostream>
#include <vector>

struct node {
  int value;
  node* next;
};

void list_add(int val, node** head) {
  auto last_node = *head;

  if (*head != nullptr) {
    for (auto pi = *head; pi != nullptr; pi = pi->next) {
      last_node = pi;
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

node** list_map(int (*f)(int val), node** head) {
  auto r = new node*;
  *r = nullptr;

  if (*head == nullptr) return r;

  for (auto pi = *head; pi != nullptr; pi = pi->next) {
    auto val = pi->value;
    auto new_val = (*f)(val);
    list_add(new_val, r);
  }

  return r;
}

int double_val(int val) {
  return val * 2;
}

// this is done with syntactic sugar in ADPL
node** create_list(std::vector<int> values) {
  auto list = new node*;
  *list = nullptr;
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
  node** list = create_list({1,2,3});
  printList(list);

  auto new_list = list_map(&double_val,list);

  printList(new_list);

  delete list;
  delete new_list;

  return 0;
}