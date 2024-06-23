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
</table>
