# ADPL Syntax

ADPL is an adaptation of the Address Programming Language that allows the programmer
to write code using only ASCII characters.

## General principles

A program in the Address programming language consists of lines.
A line can contain one or multiple formulae (i.e. statements).
Multiple formulae can be separated with `;` or `,`.

### Line labels

Each line can be marked by one or more **labels**.

In the original syntax, line labels can contain digits, characters and even spaces.  
In **ADPL**, a line label can only contain Latin letters, digits and the \_ character, and it must start with a letter.

In the original syntax an ellipsis must follow after each label:

```
lab1 ... lab2 ...  <statements>
```

In ADPL, an additional requirement is to put a `@` character before each label:

```
@lab1 ... @lab2 ...  <statements>
```

## Expressions

### Mathematical expressions

In general, most mathematical expressions in **ADPL** are written in the same way as in other modern programming languages.

Thus, float literals are written using a dot (instead of a comma like in the original syntax).
For example, `3,14` becomes `3.14`.

Arithmetic operators `+`, `−`, `×`, `:` are written as `+`, `-`, `*`, `/` in **ADPL**.

Address arithmetic operator `⊕` is written inside angle brackets in **ADPL**:
`<+>`.

Equality operators `=` and `≠` are written as `==` and `/=` in **ADPL**,
while a single `=` is used for assignment only.

Logical operators `∧`, `∨`, `¬` are replaced with keywords `and`, `or`, `not` in **ADPL**.

Functions arguments in **ADPL** are written using spaces, like in Haskell: `max 1 2`.

### Stroke operation

The "stroke" (pointer dereference) operation is written as a single quote `'`:

```
value = 'a
```

where `a` is an address.

It is similar to the unary star operator in C/C++:

```c
auto value = *a;
```

### Multiple stroke operation

Multiple stroke operation is a unique operation that is equivalent to applying the stroke operation multiple times.

In the original syntax, the multiple stroke operation is written as **<sup>n</sup>a**
(e.g. **<sup>2</sup>a** is equivalent to **''a**).

In ADPL the equivalent for that is `` `n`a ``
(e.g. `` `2`a `` for **<sup>2</sup>a**,
`` `(i + 1)`a `` for **<sup>(i + 1)</sup>a**,
`` `i`(a + 1) `` for **<sup>i</sup>(a + 1)** etc.)

### Negative stroke operation

The negative stroke operation is written as **<sup>-n</sup>a** or **<sub>n</sub>a**.
It is used to find addresses that contain references to a given address `a`.
If `n` is greater than 1 then process is applied repeatedly to intermediate results.

In **ADPL** the equivalent for that is `` m`n`a ``, where `m` stands for "minus" (negative).

For example, this program:

```
ptr(1000) => 2000
ptr(1000) => 3000
printList m`1`(1000)
```

should print:

```
[2000,3000]
```

The helper functions `ptr` and `printList` are explained a little later.
The key takeaway here is that the negative stroke operations returns a list of the addresses (`[2000,3000]`)
that contain pointers to the given address `1000`.

### Empty set character

In the original syntax, the empty set character `∅` is used to represent a null value, or a placeholder for a value in some expressions.  
In ADPL this is represented by the keyword `Nil`.

### Additional expressions

**ADPL** introduces some new types of expressions not described in the original syntax.

One of them is the syntax sugar for linked lists: `[1,2,3]`.

Yet another type of expression is `&a`, where `a` is a name of some label.
It is useful for passing subprograms as a parameter.

Also, there are some builtin functions in **ADPL**:

- `alloc n` allocates `n` free cells of memory
- `printList list` pretty-prints a linked list created with the syntax sugar (e.g. `[1,2,3]`).
- `ptr x` and `int x` help convert between types `Pointer` and `Int`

## Formulae

### Send formula

The **"Send"** formula allows the programmer to modify the value at some address in memory.
In the original syntax it is written with the special character `⇒`.
For example, the statement `a ⇒ b` means "put the value `a` at the address `b`".

In ADPL it is written as `=>`:

```
a => b
```

### Exchange formula

The **"Exchange"** formula allows the programmer to exchange the values between two addresses in memory.
In the original syntax it is written with the special character `⇔`.

In ADPL it is written as `<=>`:

```
a <=> b
```

### Jump formula

An unconditional jump formula (similar to **goto** statement in C/C++) consists of just the name of the label.
This is true for both the original syntax and ADPL.  
For example, the following line reads as "go to the line marked with the label **k**":

```
k
```

There are some other types of jumps in the Address Programming Language (computed jump, relative jump).
These are not available in the current ADPL implementation, but might be implemented in the future.

### Halt formula

In Address programming language, there are two **"Halt"** formulas: absolute and relative.

The **absolute halt formula** (the **stop** formula) is written as `!`.
It stops the program immediately regardless of where it occurs (including inside subprograms).

The **relative halt formula** (the **return** formula) `ꓭ` is similar to the `return;` statement in the C-like languages.
If it occurs inside a subprogram, it ends the current subprogram call.
If it occurs on the top level, it stops the program just like `!`.

In **ADPL** the absolute halt formula is written as `!`, the same way as in the original syntax.  
On the other hand, the relative halt formula `ꓭ` is replaced with the keyword `Ret`.

### Predicate formula

The **Predicate** formula is used for control flow, similar to the **if/else** statement in the C-like languages.

In the original syntax, the basic form for **Predicate** formula is
`P { <conditionExpr> } <thenStatements> ↓ <elseStatements>`,
where the special character `↓` acts as a separator between the execution branches.

For example, the statement `P { 'a < 5 } 'a + 1 ⇒ a ↓ k` means
"if the value at the address **a** is less than 5, increment it;
otherwise, jump to the label **k**".

It is possible to use multiple statements in both branches:

```
P { a = 1 } 1 ⇒ b; 2 ⇒ c ↓ 3 ⇒ b; 4 ⇒ c
```

To improve readability we can rewrite this with unconditional jumps to labels `label_then` and `label_else`:

```
P { a = 1 } label_then ↓ label_else
label_then ...
    1 ⇒ b
    2 ⇒ c
    label_end
label_else ...
    3 ⇒ b
    4 ⇒ c
label_end ...
```

Please note how we need an additional label `label_end` to jump over the `else` branch.

Since it is possible to leave a branch of a **Predicate** formula empty,
we can also rewrite without using `label_then`:

```
P { a = 1 } ↓ label_else
    1 ⇒ b
    2 ⇒ c
    label_end
label_else ...
    3 ⇒ b
    4 ⇒ c
label_end ...
```

Here in case the condition is true the next line `1 ⇒ b` is executed automatically.
Otherwise, we jump to `label_else`.

In **ADPL**, the only thing that actually changes in the **Predicate** formula is the separator:
`↓` is represented with the vertical bar `|`.
Thus, we can rewrite the example above in the ADPL syntax like this:

```
P { a == 1 } | label_else
    1 => b
    2 => c
    label_end
@label_else ...
    3 => b
    4 => c
@label_end ...
```

### Loop formula

The **Loop** formula is somewhat similar to the `for` loop statement in the C-like languages.

In the original syntax, the basic form for the **Loop** formula is
`Ц { a, С∅, P { Lc } ⇒ π } α`,
where

- **a** is the initial value of the loop counter;
- **С∅** is the step expression;
  - **C** is the **Successor** operation (not directly implemented as part of ADPL); the most basic version of Successor operation is **incrementation**;
  - **∅** is used as a placeholder for the value of the loop counter;
- **P { Lc }** is the loop condition, where **Lc** is a boolean expression;
- **π** is an the identifier for the pointer to the loop counter
- **α** is the **scope** label of the Loop formula

The **scope** label is a label of the line which delimits the body of the **Loop** formula.
That is, all lines between the **Loop** formula declaration
and the line marked with the given label (**α** in the example)
are considered to be the body of the loop:

```
Ц { a, С∅, P { Lc } ⇒ π } α
    <loop-body-1>
    <loop-body-2>
α...
```

In **ADPL** the letter `Ц` is replaced with `L` (since `Ц` stands for **"цикл"**, which means "loop").
The abstract **Successor** operation is replaced with a particular implementation.
For example, a loop that counts from **1** to **5** with step **1** can be written like this:

```
L { 1, Nil + 1, P { 'pi <= 5 } => pi } alpha
    print 'pi
@alpha ...
```

Here, the loop starts by placing the initial value **1** at the address **pi** (equivalent to `1 => pi`).  
In the loop body, we get the actual value of the counter by dereferencing `pi` with the stroke operation (since it's a pointer), and then print it.  
The loop step is equivalent to the statement `'pi + 1 => pi`.
`Nil` is replaced with the expression `'pi` (current counter value),
then the new result is written back to the address `pi`.  
The `'pi` expression is also used in the loop condition to check that the counter is less or equal to 5.

#### Loop formula simplifications

The **Loop** formula also allows several simplifications.

Firstly, in case the **step expression** is of the form `Nil + b` (where `b` is some scalar value),
it can be rewritten as just `(b)`. By applying this rule to the previous example, we get:

```
L { 1 (1) P { 'pi <= 5 } => pi } alpha
    print 'pi
@alpha ...
```

Here we use parentheses instead of commas to indicate that `(1)` is actually the **step value**.

Secondly, we can also use an **end value** instead of the condition:

```
L { 1 (1) 5 => pi } alpha
    print 'pi
@alpha ...
```

Please note that in the current implementation of ADPL
the **end value** is transformed to a condition using the `<=` operator,
so this won't work with a negative step.

#### Loop formula for lists

The Loop formula can be used to iterate over linked lists created with the syntax sugar `[1,2,3]`.

For example, the following program

```
list = [1,2,3]

L { 'list, 'Nil, P { 'i /= 0 } => i } l1
    val = '('i + 1)
    print val
@l1 ...
```

should print

```
1
2
3
```

In this loop the value of the **loop counter** `'i` points to the address of the current node of the linked list.

Since the first cell of each node contains the address of the next node,
the step expression of the loop is `'Nil`, which is equivalent to `'('i) => 'i`, that is,
replace the value of the current counter with the value at the address that it points to.

The loop condition is `'i /= 0`, since the last node of the list always points to the `0` address.

The value of each node is always in the next memory cell after the node address itself.
That's why we use `'('i + 1)` to get the value in each iteration.

### Subprogram call formula

#### Subprogram declaration

A **subprogram (subroutine) declaration** in the Address programming language consists of
a **labelled line** where all statements are of the form `∅ ⇒ <variable>` or `∅ → <variable>`,
followed by one or more lines that constitute the body of the subprogram.
The last line of the body must be a single return formula (`ꓭ`). For example:

```
f ... ∅ ⇒ a, ∅ → b
    Печать 'a
    Печать b
ꓭ
```

Here we declare a subprogram with the name `f`. It takes two parameters, `a` and `b`.  
The first parameter is passed using the **Send** formula (`⇒`),
so the actual value can be accessed with `'a`.  
The second parameter is passed directly (`→`), so the value can be accessed with just `b`.

In **ADPL** this would be written as follows:

```
@f ... Nil => a, Nil -> b
    print 'a
    print b
Ret
```

#### Subprogram call

In the original syntax, a subprogram call is written in the form of `П <subprogram-name> { <args> }`.
`П` stands for "Подпрограмма" ("subprogram"). Arguments are separated by comma.
For example, to call the subprogram declared above, we would write:

```
П f { 1, 2 }
```

In **ADPL** the letter `П` is replaced with `Pg`:

```
Pg f { 1, 2 }
```

This call would result in

```
1
2
```

printed to the terminal.

There is also a special way to call a subprogram not explicitly described in the original syntax.
For example, if we wanted to save the address of a subprogram to a variable and later call it,
we could write:

```
fv = &f
Pg [fv] { 1, 2 }
```

This allows us to pass a subprogram as a parameter to another subprogram.

#### Returning values

In **ADPL** there is no direct equivalent for the `return` statement in the C-like languages.
Instead, one or more "output" parameters are passed to the subprogram,
which are used as addresses to store the results. For example:

```
Pg double { 2, res }
print 'res

!
@double ... Nil -> value, Nil -> result
    value * 2 => result
Ret
```

This program should print `4`.

### Replace formula

The **"Replace"** formula is used in Address Programming Language for metaprogramming.
It is somewhat similar to macros in C/C++.  
The basic form in the original syntax is `З { a1 → c1, an → cn } α, β`.

Here the labels `α, β` represent the scope of the formula.
That is, the formula is applied to the lines that are between the line marked with the label `α` (inclusive)
and the line marked with the label`β` (exclusive).

The expression `a1 → c1, an → cn` is a list of replacement rules.
A single replacement rule `a → c` consists of a pattern to match (`a`)
and a replacement for that pattern (`c`).

Here's an example in the original syntax:

```
n = 10
З { - → +, n → 100 } α, β
!
α ...
    Печать 10 - 1
    Печать 10 - 2
    Печать 10 - 3
    Печать n
β ...
```

Without the replacement, this would print

```
9
8
7
10
```

With the replacement, it prints the following instead:

```
11
12
13
100
```

In **ADPL** the letter `З` is replaced with the keyword `R`.
Also, replacement rules are separated with `;` instead of `,`.
Thus, the above example would be written as:

```
n = 10
R { - -> +; n -> 100 } alpha, beta
!
@alpha ...
    print 10 - 1
    print 10 - 2
    print 10 - 3
    print n
@beta ...
```

In the **ADPL** implementation it is also possible to replace entire statements.
