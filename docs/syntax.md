# ADPL Syntax

ADPL is an adaptation of the Address Programming Language that allows the programmer 
to write code using only ASCII characters.

## General principles

### Line labels

A program in the Address programming language consists of lines. 
A line can contain one or multiple formulae (i.e. statements). 
Each line can be marked by **labels**.

In the original syntax, line labels can contain digits, characters and even spaces.  
In ADPL, a line label can only contain Latin letters, digits and the _ character, and it must start with a letter.

A line can be marked with one or more labels. In the original syntax an ellipsis must follow after each label:

```
lab1 ... lab2 ...  <statements>
```

In ADPL, an additional requirement is to put a @ character before each label:

```
@lab1 ... @lab2 ...  <statements>
```

## Expressions

### Mathematical expressions

In general, most mathematical expressions in ADPL are written in the same way as in other modern programming languages.

Thus, float literals are written using a dot (instead of a comma like in the original syntax).

### Stroke operation

The "stroke" (pointer dereference) operation is written as a single quote ':

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

In ADPL the equivalent for that is `` m`n`a ``, where ``m`` stands for "minus" (negative).

### Empty set character

In the original syntax, the empty set character ∅ is used to represent a null value, or a placeholder for a value in some expressions.  
In ADPL this is represented by the keyword ``Nil``.

## Formulae

### Send formula

The **"Send"** formula allows the programmer to modify the value at some address in memory.
In the original syntax it is written with the special character ⇒.
For example, the statement **a ⇒ b** means "put the value **a** at the address **b**".

In ADPL it is written as ``=>``:

```
a => b
```

### Exchange formula

The **"Exchange"** formula allows the programmer to exchange the values between two addresses in memory.
In the original syntax it is written with the special character ⇔.

In ADPL it is written as ``<=>``:

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

### Predicate formula

The **Predicate** formula is used for control flow, similar to the **if/else** statement in C-like languages.

In the original syntax, the basic form for **Predicate** formula is 
``P { <conditionExpr> } <thenStatements> ↓ <elseStatements>``, 
where the special character `↓` acts as a separator between the execution branches.

For example, the statement ``P { 'a < 5 } 'a + 1 ⇒ a ↓ k`` means 
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

Here in case the condition is true the next line ``1 ⇒ b`` is executed automatically. 
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

The **Loop** formula is somewhat similar to the **for** loop statement in C-like languages.

In the original syntax, the basic form for the **Loop** formula is 
`Ц { a, С∅, P { Lc } ⇒ π } α, l1`,
where
  * **a** is the initial value of the loop counter;
  * **С∅** is the step expression;
    * **C** is the **Successor** operation (not directly implemented as part of ADPL); the most basic version of Successor operation is **incrementation**;
    * **∅** is used as a placeholder for the value of the loop counter);
  * **P { Lc }** is the loop condition, where **Lc** is a boolean expression;
  * **π** is the identifier for the pointer to loop counter
  * **α** is the **scope** label of the Loop formula
  * **l1** is the optional label of the line which should be executed after the loop is finished

The **scope** label is a label of the line which delimits the body of the **Loop** formula. 
That is, all lines between the **Loop** formula declaration 
and the line marked with the given label (**α** in the example)
are considered to be the body of the loop:

```
Ц { a, С∅, P { Lc } ⇒ π } α, l1
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


### Subprogram call formula

### Replace formula