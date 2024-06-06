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

In ADPL it is written as =>:

```
a => b
```

### Exchange formula

The **"Exchange"** formula allows the programmer to exchange the values between two addresses in memory.
In the original syntax it is written with the special character ⇔.

In ADPL it is written as <=>:

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

### Loop formula

### Subprogram call formula

### Replace formula