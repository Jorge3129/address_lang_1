<table>
<tr>
<th>Original Syntax</th>
<th>ADPL</th>
<th>C++</th>
</tr>

<tr>
<td style="vertical-align: top;">

```
main
!

fact ... ∅ ⇒ n, ∅ ⇒ res
    P { 'n <= 1 } 1 ⇒ 'res; ꓭ ↓
    П fact { 'n - 1, prevFact }
    'n × 'prevFact ⇒ 'res
    ꓭ

main ...
    Ц { 0 (1) 20 ⇒ i } l1
        П fact { 'i, res }
        печать 'res
    l1 ...
```

</td>

<td style="vertical-align: top;">

```
main
!

@fact ... Nil => n, Nil => res
    P { 'n <= 1 } 1 => 'res; Ret |
    Pg fact { 'n - 1, prevFact }
    'n * 'prevFact => 'res
    Ret

@main ...
    L { 0 (1) 20 => i } l1
        Pg fact { 'i, res }
        print 'res
    @l1 ...
```

</td>

<td style="vertical-align: top;">

```
#include <iostream>

unsigned long fact(unsigned long n) {
    if (n <= 1) {
        return 1;
    }
    
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
```

</td>

</tr>

<tr>
<td style="vertical-align: top;">

```
main
!

stack_new ... ∅ → res_addr
    0 ⇒ res_addr
    ꓭ

stack_push ... ∅ → val, ∅ → head
    old_addr = 'head

    new_addr = alloc 2

    new_addr ⇒ head
    old_addr ⇒ new_addr
    val ⇒ new_addr + 1
    ꓭ

stack_pop ... ∅ → head, ∅ → res_addr
    old_addr = 'head
    P { old_addr = 0 } ꓭ ↓

    old_val = '(old_addr + 1)

    next_addr = 'old_addr
    next_addr ⇒ head
    0 ⇒ old_addr
    old_val ⇒ res_addr
    ꓭ

stack_is_empty ... ∅ → head, ∅ → res_addr
    'head = 0 ⇒ res_addr
    ꓭ

main ...
    П stack_new { s }
    printList s

    Ц { 1(1)5 ⇒ i } l1
        П stack_push { 'i, s }
    l1 ...
    printList s

    П stack_is_empty { s, s_em }
    Ц { 0(1) P { 's_em ≠ 1 } ⇒ pi } l2
        П stack_pop { s, top_val }
        печать 'top_val
        printList s
        П stack_is_empty { s, s_em }
    l2 ...
```

</td>

<td style="vertical-align: top;">

```
main
!

@stack_new ... Nil -> res_addr
    0 => res_addr
    Ret

@stack_push ... Nil -> val, Nil -> head
    old_addr = 'head

    new_addr = alloc 2

    new_addr => head
    old_addr => new_addr
    val => new_addr + 1
    Ret

@stack_pop ... Nil -> head, Nil -> res_addr
    old_addr = 'head
    P { old_addr == 0 } Ret |

    old_val = '(old_addr + 1)

    next_addr = 'old_addr
    next_addr => head
    0 => old_addr
    old_val => res_addr
    Ret

@stack_is_empty ... Nil -> head, Nil -> res_addr
    'head == 0 => res_addr
    Ret

@main ...
    Pg stack_new { s }
    printList s

    L { 1(1)5 => i } l1
        Pg stack_push { 'i, s }
    @l1 ...
    printList s

    Pg stack_is_empty { s, s_em }
    L { 0(1) P { 's_em /= 1 } => pi } l2
        Pg stack_pop { s, top_val }
        print 'top_val
        printList s
        Pg stack_is_empty { s, s_em }
    @l2 ...
```

</td>

<td style="vertical-align: top;">

```
#include <iostream>

struct node {
    int value;
    node* next;
};

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

void printList(node** head) {
    std::cout << '[';
    for (auto cur = *head; cur != nullptr; cur = cur->next) {
        std::cout << cur->value;
        if (cur->next != nullptr) {
             std::cout << ',';
        }
    }
    std::cout << ']' <<  std::endl;
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

```

</td>

</tr>
</table>
